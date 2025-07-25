# filter.R
# Applies filters to the output match files based on CNV (Copy Number Variation) reference data.
# This step ensures matched samples are not falsely identified due to known CNV patterns.

#' Applies CNV-based filtering to output match files.
#'
#' This function compares 'Hs' gene columns between output match files and a CNV reference database.
#' It filters out rows where all values of common 'Hs' columns are completely different from the CNV reference,
#' assuming these cases are likely false positives or irrelevant matches.
#'
#' @param output_dir Directory containing the *_matches.csv files to be filtered.
#' @param cnvs_file Path to the CNV reference database (e.g., cnvs.csv).
#' @param output_processed_dir Directory where filtered output files will be written.
#' @return A list with a success flag and a message describing the results.
apply_filter <- function(output_dir = "output", cnvs_file = "cnvs/cnvs.csv", output_processed_dir = "output_processed") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_processed_dir)) {
    dir.create(output_processed_dir)
  }
  
  # Verify CNV database file exists
  if (!file.exists(cnvs_file)) {
    return(list(success = FALSE, message = paste("ERROR: CNVs file not found:", cnvs_file)))
  }
  
  # Load CNV database, keeping original column names (especially 'HsXXXX_cn')
  cnvs_db <- read.csv(cnvs_file, stringsAsFactors = FALSE, check.names = FALSE)
  names(cnvs_db)[1] <- "Sample_ID"  # Rename first column for consistency
  
  # Get all match result files in the output directory
  output_files <- list.files(output_dir, pattern = "_matches\\.csv$", full.names = TRUE)
  if (length(output_files) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No match files found in directory:", output_dir)))
  }
  
  filter_results <- list()
  
  # Process each match file
  for (file_path in output_files) {
    tryCatch({
      matches_df <- read.csv(file_path, stringsAsFactors = FALSE)
      filtered_matches_df <- data.frame()  # To store rows that pass filtering
      
      # Iterate over each row (i.e., each match entry)
      for (i in 1:nrow(matches_df)) {
        current_row_matches <- matches_df[i, ]
        sample_id_to_check <- current_row_matches$Sample_ID
        
        # Identify 'Hs' columns in the match file (e.g., Hs12345 or Hs12345_1)
        hs_cols_in_matches_base <- grep("^Hs\\d+(_\\d+)?$", names(current_row_matches), value = TRUE)
        
        # Extract values from these columns for the current match row
        values_from_matches <- as.character(current_row_matches[hs_cols_in_matches_base]) 
        
        # --- Filtering Logic ---
        
        # Condition 1: If any value is NA → Keep the row (cannot compare reliably)
        if (any(is.na(values_from_matches))) {
          filtered_matches_df <- bind_rows(filtered_matches_df, current_row_matches)
          next
        }
        
        # Get matching CNV reference row based on Sample_ID
        cnv_row_reference <- cnvs_db %>% filter(Sample_ID == sample_id_to_check)
        
        # Condition 2: If the sample doesn't exist in the CNV DB → Keep the row
        if (nrow(cnv_row_reference) == 0) {
          filtered_matches_df <- bind_rows(filtered_matches_df, current_row_matches)
          next
        }
        
        # Handle multiple entries in CNV DB → use first one only
        if (nrow(cnv_row_reference) > 1) {
          warning(paste("Multiple entries for Sample_ID", sample_id_to_check, "in CNV DB. Using first entry."))
          cnv_row_reference <- cnv_row_reference[1, ]
        }
        
        # Identify CNV columns (e.g., Hs12345_cn)
        cnv_hs_cols_with_cn <- grep("^Hs\\d+(_\\d+)?_cn$", names(cnv_row_reference), value = TRUE)
        
        # Get base names (remove "_cn") to match with output file column names
        cnv_hs_cols_base <- str_replace(cnv_hs_cols_with_cn, "_cn$", "")
        
        # Find common 'Hs' columns in both match row and CNV DB
        common_hs_cols_base <- intersect(hs_cols_in_matches_base, cnv_hs_cols_base)
        
        # Condition 3: If no common columns exist → Keep the row for safety
        if (length(common_hs_cols_base) == 0) {
          filter_results <- c(filter_results, paste0("WARNING: No matching 'Hs' columns found for Sample_ID ", sample_id_to_check, ". Row retained."))
          filtered_matches_df <- bind_rows(filtered_matches_df, current_row_matches)
          next
        }
        
        # Condition 4: Check if all values differ (and are not NA) between match row and CNV reference
        all_different <- TRUE
        for (base_col_name in common_hs_cols_base) {
          val_matches <- as.character(current_row_matches[[base_col_name]])
          val_cnvs <- as.character(cnv_row_reference[[paste0(base_col_name, "_cn")]])
          
          # If any values are NA or equal → Not all different → Keep the row
          if (is.na(val_matches) || is.na(val_cnvs) || val_matches == val_cnvs) {
            all_different <- FALSE
            break
          }
        }
        
        # Final Decision:
        # - If all_different = TRUE → filter the row (do NOT keep)
        # - Otherwise → keep the row
        if (!all_different) {
          filtered_matches_df <- bind_rows(filtered_matches_df, current_row_matches)
        }
      }
      
      # Write filtered output file if any rows were retained
      if (nrow(filtered_matches_df) > 0) {
        output_file_name <- basename(file_path)
        output_file_name_filtered <- paste0(tools::file_path_sans_ext(output_file_name), "_filtered.csv")
        write.csv(filtered_matches_df, file.path(output_processed_dir, output_file_name_filtered), row.names = FALSE)
        filter_results <- c(filter_results, paste0("Filtered file generated: ", output_file_name_filtered, " with ", nrow(filtered_matches_df), " rows."))
      } else {
        filter_results <- c(filter_results, paste0("No rows passed filtering for ", basename(file_path), ". No file generated."))
      }
      
    }, error = function(e) {
      filter_results <- c(filter_results, paste0("ERROR processing ", basename(file_path), ": ", e$message))
    })
  }
  
  # Return final processing result
  if (length(filter_results) > 0) {
    return(list(success = TRUE, message = paste("Filtering process completed.\n", paste(filter_results, collapse = "\n"))))
  } else {
    return(list(success = FALSE, message = "ERROR: No match files were successfully filtered."))
  }
}
