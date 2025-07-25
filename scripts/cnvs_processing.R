# cnvs_processing.R
# This script processes multiple CNV CSV files with repeated tables and consolidates them
# into a unified, pivoted CNV database while validating consistency.

#' Process CNV files and generate a consolidated pivot table.
#'
#' This function reads CNV data from multiple files in a directory, identifies multiple tables per file,
#' validates data consistency, and outputs a wide-format CNV matrix (Sample_ID x Target).
#'
#' @param input_dir Directory containing raw CNV .csv files.
#' @param output_file Output path for the consolidated CNV file (default: "cnvs/cnvs.csv").
#' @param header_pattern Regex pattern identifying the start of a CNV table (default: "^Sample Name,Target,Reference").
#' @return A list indicating success status and a detailed message.
process_cnvs <- function(input_dir = "input_cnvs",
                         output_file = "cnvs/cnvs.csv",
                         header_pattern = "^Sample Name,Target,Reference") {
  
  # Create output directory if needed
  if (!dir.exists("cnvs")) {
    dir.create("cnvs")
  }
  
  # Check if input directory exists
  if (!dir.exists(input_dir)) {
    return(list(success = FALSE, message = paste("ERROR: Input directory '", input_dir, "' does not exist. Ensure the .csv files are there.", sep = "")))
  }
  
  input_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(input_files) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No .csv files found in directory:", input_dir)))
  }
  
  all_long_data_frames <- list()
  error_messages <- c()
  
  # Process each input file individually
  for (current_input_file in input_files) {
    current_file_name <- basename(current_input_file)
    
    tryCatch({
      all_lines <- readLines(current_input_file, warn = FALSE)
      start_indices <- grep(header_pattern, all_lines)
      
      if (length(start_indices) == 0) {
        stop(sprintf("Header pattern '%s' not found in '%s'. Please check the file format.", header_pattern, current_file_name))
      }
      
      list_of_dataframes_for_current_file <- list()
      
      # Process each detected table in the file
      for (i in seq_along(start_indices)) {
        current_start <- start_indices[i]
        section_end_line <- length(all_lines)
        
        # Try to identify end of table based on three consecutive empty lines
        if (length(all_lines) >= 3) {
          for (j in (current_start + 1):(length(all_lines) - 2)) {
            if (all(trimws(all_lines[j:(j + 2)]) == "")) {
              section_end_line <- j - 1
              break
            }
          }
        }
        
        header_line_text <- all_lines[current_start]
        data_lines_text <- if (section_end_line >= (current_start + 1)) {
          all_lines[(current_start + 1):section_end_line]
        } else NULL
        
        # Parse header line into column names
        header_df_temp <- read.csv(text = header_line_text, header = TRUE, stringsAsFactors = FALSE)
        col_names_in_r <- names(header_df_temp)
        
        # Set all columns to character unless CN is known
        col_types_list <- rep("character", length(col_names_in_r))
        names(col_types_list) <- col_names_in_r
        
        cn_col_name_in_r <- NULL
        if ("CN.Predicted" %in% col_names_in_r) {
          col_types_list["CN.Predicted"] <- "numeric"
          cn_col_name_in_r <- "CN.Predicted"
        } else if ("CN.Calculated" %in% col_names_in_r) {
          col_types_list["CN.Calculated"] <- "numeric"
          cn_col_name_in_r <- "CN.Calculated"
        } else {
          stop(sprintf("Neither 'CN.Predicted' nor 'CN.Calculated' found in section %d of file '%s'.", i, current_file_name))
        }
        
        # If the section has no data lines, return empty df with appropriate columns
        if (is.null(data_lines_text) || length(data_lines_text) == 0) {
          empty_df <- data.frame(matrix(ncol = length(col_names_in_r), nrow = 0))
          names(empty_df) <- col_names_in_r
          empty_df$OriginalLineNumber <- integer(0)
          list_of_dataframes_for_current_file[[i]] <- empty_df
          next
        }
        
        # Read section data
        temp_df <- suppressWarnings(
          read.csv(text = data_lines_text, header = FALSE, stringsAsFactors = FALSE, colClasses = col_types_list)
        )
        names(temp_df) <- col_names_in_r
        
        # Validate CN column is numeric
        if (!is.null(cn_col_name_in_r) && !is.numeric(temp_df[[cn_col_name_in_r]]) && any(!is.na(temp_df[[cn_col_name_in_r]]))) {
          stop(sprintf("CN column '%s' in section %d of file '%s' contains non-numeric values.", cn_col_name_in_r, i, current_file_name))
        }
        
        # Track original line numbers (for debugging/inconsistencies)
        temp_df$OriginalLineNumber <- (current_start + 1) + seq_len(nrow(temp_df)) - 1
        list_of_dataframes_for_current_file[[i]] <- temp_df
      }
      
      combined_data <- bind_rows(list_of_dataframes_for_current_file)
      
      # Determine which CN column to use
      cn_value_col <- if ("CN.Predicted" %in% names(combined_data)) {
        "CN.Predicted"
      } else if ("CN.Calculated" %in% names(combined_data)) {
        "CN.Calculated"
      } else {
        stop(sprintf("Neither CN.Predicted nor CN.Calculated column present after combining file '%s'", current_file_name))
      }
      
      # Keep only required columns and standardize names
      selected_data <- combined_data %>%
        select(
          `Sample.Name`,
          Target,
          !!sym(cn_value_col),
          OriginalLineNumber
        ) %>%
        rename(`Sample ID` = `Sample.Name`, `CN.Value` = !!sym(cn_value_col))
      
      selected_data$SourceFile <- current_file_name
      all_long_data_frames[[current_file_name]] <- selected_data
      
    }, error = function(e) {
      error_messages <<- c(error_messages, sprintf("ERROR in file '%s': %s", current_file_name, e$message))
    })
  }
  
  # If nothing was processed successfully
  if (length(all_long_data_frames) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No CNV files were successfully processed.\n", paste(error_messages, collapse = "\n"))))
  }
  
  # Merge all CNV data
  consolidated_long_data <- bind_rows(all_long_data_frames)
  
  # Check for conflicting duplicate values (same Sample ID and Target with different CN values)
  inconsistent_duplicates <- consolidated_long_data %>%
    group_by(`Sample ID`, Target) %>%
    filter(n_distinct(`CN.Value`) > 1) %>%
    ungroup()
  
  if (nrow(inconsistent_duplicates) > 0) {
    inconsistency_report <- inconsistent_duplicates %>%
      select(`Sample ID`, Target, `CN.Value`, SourceFile, OriginalLineNumber) %>%
      arrange(`Sample ID`, Target, SourceFile, OriginalLineNumber)
    return(list(success = FALSE, message = sprintf(
      "ERROR: Inconsistent CN values found for repeated 'Sample ID' and 'Target' combinations.\nDetails:\n%s",
      paste(capture.output(as.data.frame(inconsistency_report)), collapse = "\n")
    )))
  }
  
  # Keep unique entries only
  processed_long_data <- consolidated_long_data %>%
    distinct(`Sample ID`, Target, `CN.Value`, .keep_all = FALSE)
  
  # Pivot long format to wide format
  final_cnvs_db <- processed_long_data %>%
    pivot_wider(
      names_from = Target,
      values_from = `CN.Value`,
      id_cols = `Sample ID`,
      names_repair = "minimal"
    )
  
  # Drop samples with no CNV values
  final_cnvs_db <- final_cnvs_db %>%
    filter(rowSums(!is.na(select(., -`Sample ID`))) > 0)
  
  # Save final CNV matrix
  write.csv(final_cnvs_db, output_file, row.names = FALSE, na = "")
  
  return(list(
    success = TRUE,
    message = paste0("CNV processing completed successfully. Table saved to '", output_file, "' (", nrow(final_cnvs_db), " rows).")
  ))
}
