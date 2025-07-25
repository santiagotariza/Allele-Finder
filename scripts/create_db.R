# create_db.R
# This script creates a standardized genotype database from a raw input file matching "AT*.csv".

#' Create a standardized database file from a formatted input CSV.
#'
#' This function processes a specific input format (from AT...csv files), cleans headers,
#' normalizes alleles, and outputs a curated database file for downstream analysis.
#'
#' @param input_folder Directory containing the raw input file.
#' @param output_folder Directory to write the processed database file.
#' @param output_file_name Name of the output CSV file.
#' @return A list with a success status and a descriptive message.
create_database <- function(input_folder = "input_db", 
                            output_folder = "db", 
                            output_file_name = "db.csv") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Find the input file that matches pattern 'AT*.csv'
  input_file_path <- list.files(path = input_folder, pattern = "^AT.*\\.csv$", full.names = TRUE)
  
  if (length(input_file_path) == 0) {
    return(list(success = FALSE, message = paste0("ERROR: No file matching 'AT*.csv' found in '", input_folder, "'.")))
  } else if (length(input_file_path) > 1) {
    warning(paste0("WARNING: Multiple files matched 'AT*.csv'. Using the first one: ", input_file_path[1]))
    input_file_path <- input_file_path[1]
  }
  
  output_file_path <- file.path(output_folder, output_file_name)
  
  # Read entire file to check line count
  all_lines <- readLines(input_file_path)
  if (length(all_lines) < 18) {
    return(list(success = FALSE, message = "ERROR: Input file does not contain enough lines for processing."))
  }
  
  # Read raw data with no headers
  raw_df <- read_csv(
    input_file_path,
    col_names = FALSE,
    col_types = cols(.default = "c"),
    skip = 0
  )
  
  # Extract and reconstruct proper header row
  # Row 9 contains "Gene" in column 1, and columns 2:5 are replaced with those from row 10
  raw_df[9, 1] <- "Gene"
  raw_df[9, 2:5] <- raw_df[10, 2:5]
  
  new_header_row_vec <- as.character(raw_df[9, ])
  
  # The actual data starts from row 11 onwards
  data_rows <- raw_df[11:nrow(raw_df), ]
  
  # Clean and deduplicate header names (remove anything after underscore)
  clean_header_names <- str_replace_all(new_header_row_vec, "_.*$", "")
  unique_col_indices <- !duplicated(clean_header_names)
  final_header_names <- clean_header_names[unique_col_indices]
  final_df <- data_rows[, unique_col_indices]
  colnames(final_df) <- final_header_names
  
  # Normalize alleles: sort A/B so "A/B" and "B/A" are equivalent
  normalize_allele <- function(value) {
    if (is.na(value) || !str_detect(value, "/")) {
      return(value)
    }
    parts <- str_split(value, "/", simplify = TRUE)
    sorted_parts <- sort(parts)
    return(paste(sorted_parts, collapse = "/"))
  }
  
  # Apply allele normalization to all genotype columns (from column 6 onward)
  final_df <- final_df %>%
    mutate(across(6:ncol(.), ~sapply(., normalize_allele)))
  
  # Save processed database to CSV
  write_csv(final_df, output_file_path)
  
  return(list(
    success = TRUE, 
    message = paste0("Database successfully created and saved to '", output_file_path, "' (", ncol(final_df), " columns).")
  ))
}
