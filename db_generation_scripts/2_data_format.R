# Load Required Libraries
library(readr)        # For reading/writing delimited files
library(dplyr)        # For optional data manipulation
library(data.table)   # For efficient I/O

# Gene List to Process
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP3A4", "CYP1A2", "CYP2B6")

# Create Output Directory
dir.create("processed_data", showWarnings = FALSE)

# Function to Convert Raw Data Tables to CSV
convert_to_csv <- function(gene) {
  message("Converting tables for gene: ", gene)
  
  # Define input and output paths
  allele_definition_input <- paste0("raw_data/", gene, "_allele_definition_table.xlsx")
  diplotype_phenotype_input <- paste0("raw_data/", gene, "_diplotype_phenotype_table.xlsx")
  haplotypes_input <- paste0("raw_data/", gene, "_haplotypes_table.tsv")
  
  allele_definition_output <- paste0("processed_data/", gene, "_allele_definition_table.csv")
  diplotype_phenotype_output <- paste0("processed_data/", gene, "_diplotype_phenotype_table.csv")
  haplotypes_output <- paste0("processed_data/", gene, "_haplotypes_table.csv")
  
  tryCatch({
    # Process allele definition table (XLSX to CSV)
    if (file.exists(allele_definition_input)) {
      allele_definition_table <- suppressMessages(readxl::read_excel(allele_definition_input))
      write_csv(allele_definition_table, allele_definition_output)
      message("✔ Allele definition table processed for ", gene)
    } else {
      message("⚠ Allele definition file not found for ", gene)
    }
    
    # Process diplotype/phenotype table (XLSX to CSV)
    if (file.exists(diplotype_phenotype_input)) {
      diplotype_phenotype_table <- suppressMessages(readxl::read_excel(diplotype_phenotype_input))
      write_csv(diplotype_phenotype_table, diplotype_phenotype_output)
      system(paste("head -n 5", diplotype_phenotype_output))
      message("✔ Diplotype/phenotype table processed for ", gene)
    } else {
      message("⚠ Diplotype/phenotype file not found for ", gene)
    }
    
    # Process haplotype table (TSV to clean CSV)
    if (file.exists(haplotypes_input)) {
      haplotypes_table <- suppressMessages(
        read_tsv(haplotypes_input, skip = 1, col_types = cols(.default = "c"))
      )
      
      haplotypes_table <- haplotypes_table %>%
        mutate(across(everything(), ~ gsub("\t", ",", .)))
      
      write.table(haplotypes_table, haplotypes_output, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
      system(paste("head -n 5", haplotypes_output))
      message("✔ Haplotype table processed for ", gene)
    } else {
      message("⚠ Haplotype file not found for ", gene)
    }
    
  }, error = function(e) {
    message("❌ Error while processing ", gene, ": ", e$message)
  })
}

# Run Conversion for Each Gene
lapply(genes, convert_to_csv)
