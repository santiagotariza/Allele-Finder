# samples_processing.R
# Script to extract and process genotype data from compressed sample ZIP files,
# merge duplicate rsID columns, and export cleaned genotype matrices.

#' Process and extract genotype data from sample ZIP files.
#'
#' This function looks for ZIP files in the specified directory, extracts their contents,
#' locates genotype data (specifically 'Genotype Matrix.csv'), cleans and merges redundant
#' rsID columns (e.g., rs12345 and rs12345.1), and outputs a cleaned version of each matrix.
#'
#' @param samples_dir Path to the directory containing the sample ZIP files.
#' @return A list indicating success and a message with the generated output files or errors.
process_samples <- function(samples_dir = "samples") {
  
  # List all ZIP files in the provided samples directory
  zip_files <- dir_ls(samples_dir, glob = "*.zip")
  
  # Return early if no ZIP files are found
  if (length(zip_files) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No .zip files found in directory:", samples_dir)))
  }
  
  results <- list()
  
  # Process each ZIP file
  for (zip_file in zip_files) {
    tryCatch({
      
      # Derive names and paths
      zip_name <- path_file(zip_file)
      folder_name <- path_ext_remove(zip_name)
      folder_path <- file.path(samples_dir, folder_name)
      
      # Create temporary extraction folder
      if (!dir.exists(folder_path)) {
        dir.create(folder_path)
      }
      
      # Unzip contents to folder
      unzip(zip_file, exdir = folder_path)
      
      # Locate the genotype matrix CSV
      csv_file <- file.path(folder_path, "Genotype Matrix.csv")
      if (!file.exists(csv_file)) {
        stop(paste("Missing 'Genotype Matrix.csv' in", folder_path))
      }
      
      # Read genotype data, skipping any comment lines
      genotype_data <- read.csv(csv_file, comment.char = "#", stringsAsFactors = FALSE)
      
      # Skip empty files
      if (nrow(genotype_data) == 0) {
        warning(paste("Empty or invalid file:", csv_file))
        next
      }
      
      # Remove rows after any "Empty" marker
      empty_row <- which(genotype_data == "Empty", arr.ind = TRUE)[1]
      if (!is.na(empty_row)) {
        genotype_data <- genotype_data[1:(empty_row - 1), ]
      }
      
      # Ensure all values are character type
      genotype_data <- data.frame(lapply(genotype_data, as.character), stringsAsFactors = FALSE)
      
      # Clean column names by removing trailing suffixes (e.g., _X)
      colnames(genotype_data) <- sub("_.*", "", colnames(genotype_data))
      
      # Reorder columns: ID column first, then sorted rsID columns
      genotype_data <- genotype_data[, c(1, order(colnames(genotype_data)[-1]) + 1)]
      
      # Identify rsIDs that have ".1" duplicates (e.g., rs123 and rs123.1)
      base_rsids_with_variants <- unique(
        sub("\\.1$", "", grep("^rs[0-9]+\\.1$", colnames(genotype_data), value = TRUE))
      )
      
      # Initialize result container and track columns to remove
      merged_genotypes <- genotype_data[, 1, drop = FALSE]
      cols_to_remove_from_genotype_data <- c()
      
      # Helper function to merge two genotype values
      merge_genotypes <- function(val_rsid, val_rsid_dot1) {
        is_valid_genotype <- function(val) {
          !(val %in% c("NOAMP", "UND", "") || is.na(val))
        }
        
        val_rsid_is_valid <- is_valid_genotype(val_rsid)
        val_rsid_dot1_is_valid <- is_valid_genotype(val_rsid_dot1)
        
        if (val_rsid_is_valid && val_rsid_dot1_is_valid) {
          if (val_rsid == val_rsid_dot1) {
            return(val_rsid)
          } else {
            # If both values are valid but different, prioritize val_rsid
            return(val_rsid)
          }
        } else if (val_rsid_is_valid) {
          return(val_rsid)
        } else if (val_rsid_dot1_is_valid) {
          return(val_rsid_dot1)
        } else {
          return(val_rsid)
        }
      }
      
      # Merge rsID and rsID.1 columns
      for (base_col in base_rsids_with_variants) {
        variant_col <- paste0(base_col, ".1")
        if (base_col %in% colnames(genotype_data) && variant_col %in% colnames(genotype_data)) {
          merged_result <- mapply(merge_genotypes,
                                  genotype_data[[base_col]],
                                  genotype_data[[variant_col]],
                                  SIMPLIFY = TRUE, USE.NAMES = FALSE)
          merged_genotypes[[base_col]] <- merged_result
          cols_to_remove_from_genotype_data <- c(cols_to_remove_from_genotype_data, variant_col)
        } else if (base_col %in% colnames(genotype_data)) {
          merged_genotypes[[base_col]] <- genotype_data[[base_col]]
        }
      }
      
      # Add rsIDs that don't have a ".1" variant
      all_rs_base_cols <- grep("^rs[0-9]+$", colnames(genotype_data), value = TRUE)
      rs_cols_without_variants <- setdiff(all_rs_base_cols, base_rsids_with_variants)
      for (col in rs_cols_without_variants) {
        merged_genotypes[[col]] <- genotype_data[[col]]
      }
      
      # Retain other non-rsID columns (e.g., sample metadata)
      other_non_rs_cols <- setdiff(colnames(genotype_data), c(all_rs_base_cols, cols_to_remove_from_genotype_data, colnames(merged_genotypes)))
      for (col in other_non_rs_cols) {
        merged_genotypes[[col]] <- genotype_data[[col]]
      }
      
      # Preserve original non-rsID columns (e.g., sample ID or platform metadata)
      non_rs_columns_final <- grep("^rs[0-9]+", colnames(genotype_data), invert = TRUE, value = TRUE)
      final_data <- genotype_data[, non_rs_columns_final, drop = FALSE]
      
      # Avoid duplicating metadata columns during merge
      if ("Sample.Assay" %in% colnames(final_data) && "Sample.Assay" %in% colnames(merged_genotypes)) {
        merged_genotypes <- merged_genotypes %>% select(-`Sample.Assay`)
      }
      
      # Final combined data
      genotype_data <- bind_cols(final_data, merged_genotypes)
      
      # Sort columns alphabetically, keeping the first column (ID) first
      first_col_name <- colnames(genotype_data)[1]
      other_cols <- setdiff(colnames(genotype_data), first_col_name)
      genotype_data <- genotype_data[, c(first_col_name, sort(other_cols))]
      
      # Write the processed genotype matrix to a new CSV file
      output_csv <- file.path(samples_dir, paste0(folder_name, "_Genotype_Matrix_short.csv"))
      fwrite(genotype_data, output_csv)
      results <- c(results, paste0(" - Genotype Data: ", output_csv))
      
    }, error = function(e) {
      warning(paste("Error processing", zip_file, ":", e$message))
    }, finally = {
      # Clean up extracted folder
      if (exists("folder_path") && dir.exists(folder_path)) {
        unlink(folder_path, recursive = TRUE)
      }
    })
  }
  
  # Return result summary
  if (length(results) > 0) {
    return(list(success = TRUE, message = paste("Sample processing completed successfully. Generated files:\n", paste(results, collapse = "\n"))))
  } else {
    return(list(success = FALSE, message = "ERROR: No sample could be processed successfully."))
  }
}
