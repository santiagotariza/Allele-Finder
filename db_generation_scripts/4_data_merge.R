# Load required libraries
library(dplyr)
library(tidyverse)

# Define input and output directories
input_folder_definitions <- "updated_data"
input_folder_haplotypes <- "processed_data"
output_folder <- "merged_data"

# List of genes to process
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP3A4", "CYP1A2", "CYP2B6")

# Create output directory if it does not exist
if (!dir.exists(output_folder)) dir.create(output_folder)

# Main loop: process one gene at a time
for (gene in genes) {
  cat("\n--- Processing gene:", gene, "---\n")
  
  # Construct input file paths
  definition_file <- file.path(input_folder_definitions, paste0(gene, "_allele_definition_table.csv"))
  haplotype_file <- file.path(input_folder_haplotypes, paste0(gene, "_haplotypes_table.csv"))
  
  # Load allele definition and haplotype tables
  definition_data <- read.csv(definition_file, stringsAsFactors = FALSE, check.names = FALSE)
  haplotype_data <- read.csv(haplotype_file, stringsAsFactors = FALSE, check.names = FALSE)
  
  # ----------------------------------------------------------
  # Step 1: Clean and prepare allele definition table
  # ----------------------------------------------------------
  
  # The first 4 rows typically contain metadata or titles → remove them
  definition_data_filtered <- definition_data[-c(0:4), ]
  
  # Set the first data row (formerly row 5) as the header
  colnames(definition_data_filtered) <- as.character(unlist(definition_data_filtered[1, ]))
  
  # Remove the first and second rows (now redundant after setting column names)
  definition_data_filtered <- definition_data_filtered[-c(1, 2), ]
  
  # Save the first allele row (row 8 originally) as a "template" to fill NA values later
  definition_data_head <- definition_data_filtered[1, ]
  
  # ----------------------------------------------------------
  # Step 2: Process and reshape haplotype table
  # ----------------------------------------------------------
  
  # Extract only relevant columns: haplotype name, rsID, and variant allele
  haplotype_data_slimmed <- haplotype_data[, c(1, 3, 8)]
  
  # Remove rows where rsID is missing or invalid
  haplotype_data_slimmed <- haplotype_data_slimmed %>% filter(rsID != "-")
  
  # Optional check for duplicates: same haplotype name and rsID
  duplicates <- haplotype_data_slimmed %>%
    group_by(`Haplotype Name`, rsID) %>%
    filter(n() > 1)
  if (nrow(duplicates) > 0) {
    cat("Warning: duplicates found in haplotype data for", gene, "\n")
    print(duplicates)
  }
  
  # Reshape haplotype data to wide format: one row per haplotype, one column per rsID
  haplotype_wide <- haplotype_data_slimmed %>%
    pivot_wider(
      names_from = rsID,
      values_from = `Variant Allele`
    )
  
  # Rename "Haplotype Name" column to "rsID" to align with allele table
  haplotype_wide <- haplotype_wide %>% rename(rsID = `Haplotype Name`)
  
  # Remove gene name prefix from haplotype identifiers (e.g., "CYP2C19*1" → "*1")
  pattern <- paste0("(", paste(genes, collapse = "|"), ")")
  haplotype_wide$rsID <- gsub(pattern, "", haplotype_wide$rsID)
  
  # ----------------------------------------------------------
  # Step 3: Align haplotype columns to allele definition format
  # ----------------------------------------------------------
  
  # Get correct rsID column order from the definition template
  rsid_order <- colnames(definition_data_head)
  
  # Identify columns that exist in both the definition and haplotype tables
  common_columns <- intersect(rsid_order, colnames(haplotype_wide))
  
  # Subset the haplotype table to keep only the rsIDs present in definition table
  haplo_subset <- haplotype_wide %>% select(all_of(common_columns))
  
  # Create an empty data frame with same column order as definition table
  aligned_df <- as.data.frame(matrix(NA, nrow = nrow(haplo_subset), ncol = length(rsid_order)))
  colnames(aligned_df) <- rsid_order
  
  # Copy matching columns from haplotype data into the aligned data frame
  aligned_df[, common_columns] <- haplo_subset
  
  # Add the reference row from allele definition on top of aligned haplotypes
  combined <- bind_rows(definition_data_head, aligned_df)
  
  # Fill in any missing values (NA) using the first row (reference values)
  for (col in colnames(combined)) {
    reference_value <- combined[[col]][1]
    combined[[col]][is.na(combined[[col]])] <- reference_value
  }
  
  # Restore the original column names from the full allele definition
  colnames(combined) <- colnames(definition_data)
  
  # ----------------------------------------------------------
  # Step 4: Merge header and metadata rows back into final table
  # ----------------------------------------------------------
  
  # Recover top 6 metadata rows (originally at the top of the file)
  header_rows <- definition_data[1:6, ]
  
  # Combine everything: metadata + cleaned reference + new haplotypes
  combined_final <- bind_rows(header_rows, combined)
  
  # Append any remaining rows from original file not already included
  # (e.g., annotations or unusual haplotypes at the bottom)
  missing_rows <- definition_data %>%
    filter(!(definition_data[[1]] %in% combined_final[[1]]))
  combined_final <- bind_rows(combined_final, missing_rows)
  
  # ----------------------------------------------------------
  # Step 5: Sort rows by numeric value extracted from first column
  # ----------------------------------------------------------
  
  # Extract name of the first column (typically the allele/haplotype name)
  id_col <- colnames(combined_final)[1]
  
  # Separate metadata and allele rows
  metadata_rows <- combined_final[1:6, ]
  allele_rows <- combined_final[7:nrow(combined_final), ]
  
  # Extract the numeric part from identifiers like "*1", "*2A", "*41" for sorting
  allele_rows <- allele_rows %>%
    mutate(NumericPart = as.numeric(gsub("[^0-9.]", "", .data[[id_col]]))) %>%
    arrange(NumericPart) %>%
    select(-NumericPart)
  
  # Combine again after sorting
  combined_final <- bind_rows(metadata_rows, allele_rows)
  
  # ----------------------------------------------------------
  # Step 6: Save output to CSV
  # ----------------------------------------------------------
  
  output_file <- file.path(output_folder, paste0(gene, "_merged_table.csv"))
  write.csv(combined_final, output_file, row.names = FALSE)
  
  cat("✓ Merged table saved to:", output_file, "\n")
}
