# Load Required Libraries
library(httr)      # For HTTP requests
library(dplyr)     # For optional data manipulation
library(fs)        # For file system utilities

# Gene Configuration
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP2B6")
more_genes <- c("CYP3A4", "CYP1A2")

# Create Required Directories
dir_create("temp_files")
dir_create("raw_data")

# Alternative URLs for genes not found in standard CPIC structure
more_genes_urls <- list(
  CYP1A2 = "https://s3.pgkb.org/submission/CYP1A2_allele_definition_table-PS216394-1452817360.xlsx",
  CYP3A4 = "https://s3.pgkb.org/submission/CYP3A4_allele_definition_table-PS216409-1452255420.xlsx"
)

# Utility: Download file from URL with retries
download_file <- function(url, output_path, gene, description, retries = 3) {
  for (i in 1:retries) {
    cat(sprintf("Downloading %s for %s (attempt %d of %d)...\n", description, gene, i, retries))
    response <- try(GET(url, write_disk(output_path, overwrite = TRUE)), silent = TRUE)
    
    if (!inherits(response, "try-error") && response$status_code == 200 && file_exists(output_path)) {
      return(TRUE)
    }
    Sys.sleep(1)
  }
  message("❌ Failed to download ", description, " for gene ", gene)
  return(FALSE)
}

# Utility: Download and extract PharmVar ZIP file
download_and_extract_pharmvar_zip <- function(pharmvar_url, gene) {
  temp_dir <- tempfile()
  dir_create(temp_dir)
  zip_file <- file.path(temp_dir, "pharmvar_data.zip")
  
  response <- try(GET(pharmvar_url, write_disk(zip_file, overwrite = TRUE)), silent = TRUE)
  if (inherits(response, "try-error") || response$status_code != 200) {
    message("❌ Could not download ZIP archive for ", gene)
    unlink(temp_dir, recursive = TRUE)
    return(FALSE)
  }
  
  unzip_result <- try(unzip(zip_file, exdir = temp_dir), silent = TRUE)
  if (inherits(unzip_result, "try-error")) {
    message("❌ Failed to extract ZIP archive for ", gene)
    unlink(temp_dir, recursive = TRUE)
    return(FALSE)
  }
  
  extracted_dirs <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
  if (length(extracted_dirs) == 0) {
    message("❌ No subdirectory found inside ZIP for ", gene)
    unlink(temp_dir, recursive = TRUE)
    return(FALSE)
  }
  
  refseq_dir <- file.path(extracted_dirs[1], "RefSeqGene")
  refseq_tsv <- list.files(refseq_dir, pattern = "\\.haplotypes\\.tsv$", full.names = TRUE, recursive = TRUE)
  
  if (length(refseq_tsv) == 0) {
    message("❌ No .haplotypes.tsv file found in RefSeqGene directory for ", gene)
    unlink(temp_dir, recursive = TRUE)
    return(FALSE)
  }
  
  file.copy(refseq_tsv[1], file.path("raw_data", paste0(gene, "_haplotypes_table.tsv")), overwrite = TRUE)
  unlink(temp_dir, recursive = TRUE)
  return(TRUE)
}

# Main Function to Process a Gene
process_gene <- function(gene) {
  message("\n▶ Processing gene: ", gene)
  
  if (gene %in% more_genes) {
    allele_definition_url <- more_genes_urls[[gene]]
    allele_success <- download_file(
      url = allele_definition_url,
      output_path = paste0("raw_data/", gene, "_allele_definition_table.xlsx"),
      gene = gene,
      description = "allele definition table"
    )
    diplotype_success <- TRUE
  } else {
    allele_definition_url <- paste0("https://files.cpicpgx.org/data/report/current/allele_definition/", gene, "_allele_definition_table.xlsx")
    diplotype_phenotype_url <- paste0("https://files.cpicpgx.org/data/report/current/diplotype_phenotype/", gene, "_Diplotype_Phenotype_Table.xlsx")
    
    allele_success <- download_file(
      url = allele_definition_url,
      output_path = paste0("raw_data/", gene, "_allele_definition_table.xlsx"),
      gene = gene,
      description = "allele definition table"
    )
    
    diplotype_success <- download_file(
      url = diplotype_phenotype_url,
      output_path = paste0("raw_data/", gene, "_diplotype_phenotype_table.xlsx"),
      gene = gene,
      description = "diplotype phenotype table"
    )
  }
  
  pharmvar_url <- paste0("https://www.pharmvar.org/get-download-file?name=", gene, "&refSeq=ALL&fileType=zip&version=current")
  zip_success <- download_and_extract_pharmvar_zip(pharmvar_url, gene)
  
  if (!allele_success || !diplotype_success || !zip_success) {
    message("⚠️ Errors occurred while processing gene ", gene, ". See messages above.")
  } else {
    message("✅ Gene ", gene, " processed successfully.")
  }
}

# Run Processing for All Genes
lapply(c(genes, more_genes), process_gene)

# Cleanup
unlink("temp_files", recursive = TRUE)
