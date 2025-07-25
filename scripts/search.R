# search.R
# Script for comparing genetic sample data against a reference database
# and identifying perfect matches based on rsID markers

#' Perform a search for matching samples in a reference database.
#'
#' This function compares processed sample CSV files against a reference database CSV
#' and identifies records with complete genotype matches across shared rsID markers.
#'
#' @param samples_dir Path to the directory containing processed sample CSV files.
#' @param db_file Path to the reference database file (e.g., db.csv).
#' @param output_dir Directory where output match files will be saved.
#' @return A list with a success flag and a descriptive message.
perform_search <- function(samples_dir = "samples", db_file = "db/db.csv", output_dir = "output") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Check if the database file exists
  if (!file.exists(db_file)) {
    return(list(success = FALSE, message = paste("ERROR: Database file not found:", db_file)))
  }
  
  # Load the reference database
  db <- read.csv(db_file, stringsAsFactors = FALSE)
  
  # Clean database: convert all genotype values to character and replace empty strings with NA
  for (col_name in names(db)[-c(1:5)]) {
    db[[col_name]] <- as.character(db[[col_name]])
    db[[col_name]][db[[col_name]] == ""] <- NA
  }
  
  # Locate all sample files (CSV format) in the specified directory
  sample_files <- list.files(samples_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Exit if no sample files are found
  if (length(sample_files) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No sample .csv files found in directory:", samples_dir)))
  }
  
  search_results <- list()
  
  # Process each sample file individually
  for (sample_file in sample_files) {
    tryCatch({
      sample <- read.csv(sample_file, stringsAsFactors = FALSE)
      
      # Convert all genotype columns to character
      for (col_name in names(sample)[-1]) {
        sample[[col_name]] <- as.character(sample[[col_name]])
      }
      
      # Extract sample name from file name (without extension)
      sample_name <- tools::file_path_sans_ext(basename(sample_file))
      
      # Identify rsID columns present in both the sample and the database
      sample_rsIDs <- names(sample)[-1]
      db_rsIDs <- names(db)[-c(1:5)]
      common_rsIDs <- intersect(sample_rsIDs, db_rsIDs)
      
      matches <- data.frame()
      
      # Normalize invalid genotype values in sample data
      for (rsID_col in sample_rsIDs) {
        sample[[rsID_col]][sample[[rsID_col]] %in% c("NOAMP", "UND")] <- NA
      }
      
      # Compare each sample record to each database entry
      for (i in 1:nrow(sample)) {
        for (j in 1:nrow(db)) {
          matched_rsIDs_for_this_comparison <- c()
          non_na_db_rsIDs_to_match <- 0
          actual_matches_count <- 0
          
          # Evaluate matching rsID genotypes between sample and DB entry
          for (rsID in common_rsIDs) {
            sample_val <- sample[i, rsID]
            db_val <- db[j, rsID]
            
            if (!is.na(db_val)) {
              non_na_db_rsIDs_to_match <- non_na_db_rsIDs_to_match + 1
              if (identical(sample_val, db_val) || (is.na(sample_val) && is.na(db_val))) {
                actual_matches_count <- actual_matches_count + 1
                matched_rsIDs_for_this_comparison <- c(matched_rsIDs_for_this_comparison, rsID)
              }
            }
          }
          
          # If all non-NA database rsIDs match, record the match
          if (non_na_db_rsIDs_to_match > 0 && actual_matches_count == non_na_db_rsIDs_to_match) {
            matches <- bind_rows(matches, data.frame(
              Sample_ID = sample[i, 1],
              db[j, 1:5],  # First 5 columns of DB typically contain metadata
              rsIDs_matched = paste(matched_rsIDs_for_this_comparison, collapse = ", "),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Save results if any matches were found
      if (nrow(matches) > 0) {
        output_file <- file.path(output_dir, paste0(sample_name, "_matches.csv"))
        write.csv(matches, output_file, row.names = FALSE)
        search_results <- c(search_results, paste0("File generated: ", basename(output_file), " with ", nrow(matches), " matches."))
      } else {
        search_results <- c(search_results, paste0("No matches found for ", sample_name, "."))
      }
      
    }, error = function(e) {
      # Log any errors encountered during processing
      search_results <- c(search_results, paste0("ERROR processing ", basename(sample_file), ": ", e$message))
    })
  }
  
  # Return overall results
  if (length(search_results) > 0) {
    return(list(success = TRUE, message = paste("Search completed.\n", paste(search_results, collapse = "\n"))))
  } else {
    return(list(success = FALSE, message = "ERROR: Search could not be performed on any sample."))
  }
}
