# start_time <- Sys.time()  # Record the start time to measure script duration

# Load necessary library for fast data manipulation
library(data.table)

# Define input and output folders
input_folder_definitions <- "the_table"  # Folder containing the main table
output_folder <- "the_table"              # Folder to save the genotype table

# Construct the file path to the main combined table CSV
basic_file <- file.path(input_folder_definitions, "main_table.csv")

# Read the main table into a data.table for fast processing
data <- fread(basic_file)

# Function to generate all possible genotype combinations for a given gene
generate_genotypes <- function(gene_data) {
  
  # Generate all unique pairwise combinations of haplotypes (row indices)
  # 'combn' produces combinations of 2 haplotypes, transposed to rows
  comb <- t(combn(nrow(gene_data), 2))
  
  # Select the first haplotype of each pair
  hap1 <- gene_data[comb[, 1], ]
  
  # Select the second haplotype of each pair
  hap2 <- gene_data[comb[, 2], ]
  
  # Initialize a new data.table to store the genotype combinations
  result <- data.table(
    Gene = hap1$Gene,                # Gene name for each pair
    Haplotype1 = hap1$rsID,         # First haplotype identifier
    Haplotype2 = hap2$rsID          # Second haplotype identifier
  )
  
  # Combine alleles for each genetic variant (columns starting from 3rd)
  # For each rsID column, concatenate the alleles from haplotype 1 and 2 separated by '|'
  for (col in names(gene_data)[3:ncol(gene_data)]) {
    result[[col]] <- paste(hap1[[col]], hap2[[col]], sep = "|")
  }
  
  # Return the generated genotype table for this gene
  return(result)
}

# Apply the genotype generation function to each gene separately
# 'split' divides the data by gene, then 'lapply' processes each subset
genotypes <- rbindlist(lapply(split(data, data$Gene), generate_genotypes))

# Save the combined genotype table to a CSV file
output_file <- file.path(output_folder, "genotypes_table_hyperboost.csv")
fwrite(genotypes, output_file)

cat("\nGenotype table generated and saved as 'genotypes_table_hyperboost.csv'.\n")

# Record and print the total execution time
# end_time <- Sys.time()
# print(end_time - start_time)

# Check for duplicated genotype combinations
# This can help identify if any genotype pairs appear more than once
# duplicated_combinations <- genotypes[, .N, by = .(Gene, Haplotype1, Haplotype2)][N > 1]
# print(duplicated_combinations)
