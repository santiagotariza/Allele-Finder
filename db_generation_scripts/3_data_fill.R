# Load Required Libraries
library(dplyr)   # For data manipulation
library(readr)   # For reading and writing CSV files

# List of genes to be processed
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP3A4", "CYP1A2", "CYP2B6")

# Create output directory for storing updated CSV files (if it doesn't exist)
dir.create("updated_data", showWarnings = FALSE)

# Define a function to process and fill missing values in allele definition tables
process_allele_definition_table <- function(gene) {
  
  # Construct input and output file paths based on gene name
  input_file <- paste0("processed_data/", gene, "_allele_definition_table.csv")
  output_file <- paste0("updated_data/", gene, "_allele_definition_table.csv")
  
  # Check if the input file exists; if not, print a message and skip
  if (!file.exists(input_file)) {
    message("File not found for gene: ", gene)
    return(NULL)
  }
  
  # Read the CSV file with all columns treated as character strings
  allele_table <- read_csv(input_file, col_types = cols(.default = "c"))
  
  # Replace all string occurrences of "NA" with real NA values in the data frame
  allele_table[allele_table == "NA"] <- NA
  
  # Check if the table has at least 8 rows; otherwise skip processing
  if (nrow(allele_table) < 8) {
    message("File for gene ", gene, " has fewer than 8 rows. Skipping.")
    return(NULL)
  }
  
  # Extract reference values from row 8 (index 7 in R since it is 1-based),
  # excluding the first column (usually a label or ID column)
  reference_values <- unlist(allele_table[7, -1], use.names = FALSE)
  
  # Starting from row 9 to the end, replace NA values in each column with
  # the corresponding reference value from row 8 for that column
  for (col_idx in seq_along(reference_values)) {
    current_column <- allele_table[8:nrow(allele_table), col_idx + 1, drop = TRUE]  # Select the column slice
    
    # Use ifelse to substitute NA values with reference values, keep others unchanged
    allele_table[8:nrow(allele_table), col_idx + 1] <- ifelse(
      is.na(current_column),
      reference_values[col_idx],
      current_column
    )
  }
  
  # Write the updated allele table back to a CSV file in the output folder
  write_csv(allele_table, output_file)
  
  # Print confirmation message for the processed gene
  message("Processed and saved file for gene: ", gene)
}

# Apply the processing function to each gene in the list
lapply(genes, process_allele_definition_table)
