# Cargar librerías necesarias
library(httr)      # Para descargar archivos desde URLs
library(dplyr)     # Para manipulación de datos (opcional aquí)

# Lista de genes a procesar
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP2B6")
missing_genes <- c("CYP3A4", "CYP1A2")

# Crear directorios para archivos temporales y datos crudos
dir.create("temp_files", showWarnings = FALSE)  # Carpeta para archivos descargados temporales
dir.create("raw_data", showWarnings = FALSE)   # Carpeta para guardar archivos crudos

# URLs alternativas para los genes en missing_genes
missing_genes_urls <- list(
  CYP1A2 = "https://s3.pgkb.org/submission/CYP1A2_allele_definition_table-PS216394-1452817360.xlsx",
  CYP3A4 = "https://s3.pgkb.org/submission/CYP3A4_allele_definition_table-PS216409-1452255420.xlsx"
)

# Función para descargar archivos desde URLs
download_file <- function(url, output_path, gene, description) {
  response <- try(GET(url, write_disk(output_path, overwrite = TRUE)), silent = TRUE)
  if (inherits(response, "try-error") || response$status_code != 200) {
    message("No se pudo descargar el archivo: ", description, " para el gen ", gene)
    return(FALSE)
  }
  return(TRUE)
}

# Función para descargar y extraer archivo ZIP desde PharmVar
download_and_extract_pharmvar_zip <- function(pharmvar_url, gene) {
  temp_dir <- tempfile()        # Crear carpeta temporal única
  dir.create(temp_dir)
  
  zip_file <- file.path(temp_dir, "pharmvar_data.zip")
  response <- try(GET(pharmvar_url, write_disk(zip_file, overwrite = TRUE)), silent = TRUE)
  if (inherits(response, "try-error") || response$status_code != 200) {
    message("No se pudo descargar el archivo ZIP para el gen ", gene)
    unlink(temp_dir, recursive = TRUE)
    return(FALSE)
  }
  
  # Intentar extraer el archivo ZIP
  unzip_files <- try(unzip(zip_file, exdir = temp_dir), silent = TRUE)
  if (inherits(unzip_files, "try-error")) {
    message("No se pudo extraer el archivo ZIP para el gen ", gene)
    unlink(temp_dir, recursive = TRUE)
    return(FALSE)
  }
  
  # Determinar el nombre de la carpeta principal que contiene RefSeqGene
  extracted_dirs <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
  if (length(extracted_dirs) == 0) {
    message("No se encontró una carpeta dentro del archivo ZIP para el gen ", gene)
    unlink(temp_dir, recursive = TRUE)
    return(FALSE)
  }
  
  # Buscar el archivo .tsv en la carpeta RefSeqGene
  refseq_dir <- file.path(extracted_dirs[1], "RefSeqGene")
  refseq_tsv <- list.files(refseq_dir, 
                           pattern = "\\.haplotypes\\.tsv$", 
                           full.names = TRUE, recursive = TRUE)
  
  if (length(refseq_tsv) == 0) {
    message("No se encontró el archivo .haplotypes.tsv en la carpeta RefSeqGene para el gen ", gene)
    unlink(temp_dir, recursive = TRUE)
    return(FALSE)
  }
  
  # Mover el archivo .tsv a la carpeta 'raw_data'
  file.copy(refseq_tsv[1], file.path("raw_data", paste0(gene, "_haplotypes_table.tsv")))
  
  # Limpiar archivos temporales
  unlink(temp_dir, recursive = TRUE)
  return(TRUE)
}

# Función principal para procesar un gen
process_gene <- function(gene) {
  message("Procesando el gen: ", gene)
  
  if (gene %in% missing_genes) {
    # Descargar desde URLs alternativas para missing_genes
    allele_definition_url <- missing_genes_urls[[gene]]
    allele_success <- download_file(allele_definition_url, 
                                    paste0("raw_data/", gene, "_allele_definition_table.xlsx"), 
                                    gene, "allele definition table")
    diplotype_success <- TRUE  # No existe para missing_genes
  } else {
    # URLs de archivos XLSX para descargar (tablas CPIC)
    allele_definition_url <- paste0("https://files.cpicpgx.org/data/report/current/allele_definition/", gene, "_allele_definition_table.xlsx")
    diplotype_phenotype_url <- paste0("https://files.cpicpgx.org/data/report/current/diplotype_phenotype/", gene, "_Diplotype_Phenotype_Table.xlsx")
    
    # Descargar archivos XLSX
    allele_success <- download_file(allele_definition_url, 
                                    paste0("raw_data/", gene, "_allele_definition_table.xlsx"), 
                                    gene, "allele definition table")
    
    diplotype_success <- download_file(diplotype_phenotype_url, 
                                       paste0("raw_data/", gene, "_diplotype_phenotype_table.xlsx"), 
                                       gene, "diplotype phenotype table")
  }
  
  # Descargar y extraer archivo ZIP desde PharmVar
  pharmvar_url <- paste0("https://www.pharmvar.org/get-download-file?name=", gene, "&refSeq=ALL&fileType=zip&version=current")
  zip_success <- download_and_extract_pharmvar_zip(pharmvar_url, gene)
  
  # Mostrar mensaje si algún archivo no se pudo procesar
  if (!allele_success || !diplotype_success || !zip_success) {
    message("Hubo errores procesando el gen ", gene, ". Verifique los mensajes anteriores.")
  } else {
    message("Gen ", gene, " procesado con éxito.")
  }
}

# Procesar cada gen
lapply(c(genes, missing_genes), process_gene)

# Limpieza final: eliminar carpeta temporal de archivos descargados
unlink("temp_files", recursive = TRUE)

# Fin del script