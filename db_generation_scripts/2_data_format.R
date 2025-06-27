# Cargar librerías necesarias
library(readr)  # Para leer y escribir archivos delimitados
library(dplyr)  # Para manipulación de datos (opcional aquí)
library(data.table)

# Lista de genes a procesar
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP3A4", "CYP1A2", "CYP2B6")
#genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP2B6")  # Cambiar o agregar genes según sea necesario

# Crear directorio de salida para CSV convertidos
dir.create("processed_data", showWarnings = FALSE)

# Función para convertir tablas a CSV
convert_to_csv <- function(gene) {
  message("Convirtiendo tablas para el gen: ", gene)
  
  # Definir rutas de entrada y salida
  allele_definition_input <- paste0("raw_data/", gene, "_allele_definition_table.xlsx")
  diplotype_phenotype_input <- paste0("raw_data/", gene, "_diplotype_phenotype_table.xlsx")
  haplotypes_input <- paste0("raw_data/", gene, "_haplotypes_table.tsv")
  
  allele_definition_output <- paste0("processed_data/", gene, "_allele_definition_table.csv")
  diplotype_phenotype_output <- paste0("processed_data/", gene, "_diplotype_phenotype_table.csv")
  haplotypes_output <- paste0("processed_data/", gene, "_haplotypes_table.csv")
  
  # Verificar y convertir cada archivo
  tryCatch({
    # Leer y escribir la tabla de definición de alelos (XLSX a CSV)
    if (file.exists(allele_definition_input)) {
      allele_definition_table <- suppressMessages(
        readxl::read_excel(allele_definition_input)
      )
      write_csv(allele_definition_table, allele_definition_output)
      message("Tabla de definición de alelos procesada para el gen ", gene)
    } else {
      message("No se encontró la tabla de definición de alelos para el gen ", gene)
    }
    
    # Leer y escribir la tabla de diplotipos/fenotipos (XLSX a CSV)
    if (file.exists(diplotype_phenotype_input)) {
      diplotype_phenotype_table <- suppressMessages(
        readxl::read_excel(diplotype_phenotype_input)
      )
      write_csv(diplotype_phenotype_table, diplotype_phenotype_output)
      system(paste("head -n 5", diplotype_phenotype_output))
      message("Tabla de diplotipos/fenotipos procesada para el gen ", gene)
    } else {
      message("No se encontró la tabla de diplotipos/fenotipos para el gen ", gene)
    }
    
    if (file.exists(haplotypes_input)) {
      # Saltar la primera línea (comentario) y leer la tabla con encabezados correctos
      haplotypes_table <- suppressMessages(
        read_tsv(haplotypes_input, skip = 1, col_types = cols(.default = "c"))
      )
      
      # Reemplazar tabuladores dentro de los valores por comas
      haplotypes_table <- haplotypes_table %>%
        mutate(across(everything(), ~ gsub("\t", ",", .)))
      
      # Guardar como CSV sin comillas, con separador coma
      write.table(haplotypes_table, haplotypes_output, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
      
      system(paste("head -n 5", haplotypes_output))
      
      message("Tabla de haplotipos procesada para el gen ", gene)
    } else {
      message("No se encontró la tabla de haplotipos para el gen ", gene)
    }
    
  }, error = function(e) {
    message("Ocurrió un error al procesar las tablas para el gen ", gene, ": ", e$message)
  })
}


# Procesar cada gen
lapply(genes, convert_to_csv)

# Fin del script