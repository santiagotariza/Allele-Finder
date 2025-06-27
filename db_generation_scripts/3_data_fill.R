# Cargar librerías necesarias
library(dplyr)
library(readr)

# Lista de genes a procesar
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP3A4", "CYP1A2", "CYP2B6")
#genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP2B6")  # Agregar más genes si es necesario

# Crear directorio de salida para los datos actualizados
dir.create("updated_data", showWarnings = FALSE)

# Función para procesar los archivos CSV de _allele_definition_table
process_allele_definition_table <- function(gene) {
  # Rutas de entrada y salida
  input_file <- paste0("processed_data/", gene, "_allele_definition_table.csv")
  output_file <- paste0("updated_data/", gene, "_allele_definition_table.csv")
  
  # Verificar si el archivo existe
  if (!file.exists(input_file)) {
    message("No se encontró el archivo para el gen: ", gene)
    return(NULL)
  }
  
  # Leer el archivo CSV
  allele_table <- read_csv(input_file, col_types = cols(.default = "c"))
  
  # Reemplazar "NA" por valores NA reales en todo el dataframe
  allele_table[allele_table == "NA"] <- NA
  
  # Verificar si hay suficientes filas para procesar
  if (nrow(allele_table) < 8) {
    message("El archivo para el gen ", gene, " tiene menos de 8 filas. No se procesará.")
    return(NULL)
  }
  
  # Obtener los valores de la fila 8 como vector de referencia
  reference_values <- unlist(allele_table[7, -1], use.names = FALSE)  # Excluir la primera columna
  
  # Reemplazar NA desde la fila 9 en adelante usando los valores de la fila 8
  for (col_idx in seq_along(reference_values)) {
    # Subconjunto de la columna actual desde la fila 9 en adelante como vector
    current_column <- allele_table[8:nrow(allele_table), col_idx + 1, drop = TRUE]  # Selección correcta
    
    # Reemplazar NA con los valores de referencia
    allele_table[8:nrow(allele_table), col_idx + 1] <- ifelse(
      is.na(current_column),
      reference_values[col_idx],
      current_column
    )
  }
  print(reference_values)
  print(current_column)
  print(allele_table[9:nrow(allele_table), col_idx + 1])
  # Guardar el archivo actualizado
  write_csv(allele_table, output_file)
  message("Archivo procesado y guardado para el gen: ", gene)
}

# Procesar cada gen
lapply(genes, process_allele_definition_table)

# Fin del script