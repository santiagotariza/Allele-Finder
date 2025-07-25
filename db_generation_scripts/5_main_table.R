# Load required libraries for data manipulation
library(dplyr)
library(tidyverse)

# Define input and output folders
input_folder_definitions <- "merged_data"  # Folder containing merged tables (previous step output)
output_folder <- "the_table"                # Folder where final table will be saved

# List of genes to process
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP3A4", "CYP1A2", "CYP2B6")

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) dir.create(output_folder)

# Initialize an empty dataframe to accumulate data from all genes
main_table <- data.frame()

# Loop over each gene to process its data
for (gene in genes) {
  
  cat("\nProcessing gene:", gene, "\n")
  
  # Build the path to the merged CSV file for the current gene
  basic_file <- file.path(input_folder_definitions, paste0(gene, "_merged_table.csv"))
  
  # Read the merged table as a data frame (strings remain as strings)
  basic_table <- read.csv(basic_file, stringsAsFactors = FALSE, check.names = FALSE)
  
  # ----------------------------------------------------------
  # Step 1: Clean the base table by removing unwanted header or metadata rows
  # ----------------------------------------------------------
  
  # Remove the first 4 rows which typically contain metadata or extra headers
  basic_table_filtered <- basic_table[-c(1:4), ]
  
  # Use the first row of the filtered table as the new column names
  colnames(basic_table_filtered) <- as.character(unlist(basic_table_filtered[1, ]))
  
  # Remove the two rows used for column names to keep only data rows
  basic_table_filtered <- basic_table_filtered[-c(1:2), ]
  
  # ----------------------------------------------------------
  # Step 2: Remove columns with missing or empty column names to avoid errors
  # ----------------------------------------------------------
  
  basic_table_filtered <- basic_table_filtered[, !is.na(colnames(basic_table_filtered)) & colnames(basic_table_filtered) != ""]
  
  # ----------------------------------------------------------
  # Step 3: Prepare the table for combination
  # ----------------------------------------------------------
  
  # Create an empty dataframe with the cleaned structure (not directly used here, but useful for reference)
  basic_table_head <- basic_table_filtered[0, ]
  
  # Extract the actual content to be combined later
  basic_table_content <- basic_table_filtered
  
  # Add a new column called 'Gene' at the beginning to identify which gene each row belongs to
  basic_table_content <- basic_table_content %>%
    mutate(Gene = gene, .before = 1)
  
  # ----------------------------------------------------------
  # Step 4: Append the processed data to the main cumulative table
  # ----------------------------------------------------------
  
  main_table <- bind_rows(main_table, basic_table_content)
}

# ----------------------------------------------------------
# Step 5: Save the combined main table to a CSV file
# ----------------------------------------------------------

output_file <- file.path(output_folder, "main_table.csv")
write.csv(main_table, output_file, row.names = FALSE)

cat("\nMain table saved to:", output_file, "\n")

# ----------------------------------------------------------
# Step 6: Check for duplicate column names in the final table header
# ----------------------------------------------------------

# Identify duplicated column names in main_table
duplicated_columns <- duplicated(colnames(main_table))

# Print duplicated column names if any exist
if (any(duplicated_columns)) {
  cat("There are duplicated columns in the 'main_table' header:\n")
  print(colnames(main_table)[duplicated_columns])
} else {
  cat("No duplicated columns found in the 'main_table' header.\n")
}
