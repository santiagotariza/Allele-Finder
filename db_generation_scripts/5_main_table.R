# Configuración inicial
library(dplyr)
library(tidyverse)

# Rutas de los archivos
input_folder_definitions <- "merged_data"
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP3A4", "CYP1A2", "CYP2B6")
output_folder <- "the_table"

# Crear carpeta de salida si no existe
if (!dir.exists(output_folder)) dir.create(output_folder)

# Inicializar main_table
main_table <- data.frame()

for (gene in genes) {
  # Función principal
  cat("\nProcesando gen:", gene, "\n")
  
  # Leer tablas
  basic_file <- file.path(input_folder_definitions, paste0(gene, "_merged_table.csv"))
  basic_table <- read.csv(basic_file, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Paso 1: Preparar la tabla de alelos
  basic_table_filtered <- basic_table[-c(1:4), ]  # Excluye las primeras 4 filas
  colnames(basic_table_filtered) <- as.character(unlist(basic_table_filtered[1, ]))  # Asigna nombres de columnas
  basic_table_filtered <- basic_table_filtered[-c(1:2), ]  # Excluye la fila que usaste como encabezado
  
  # Eliminar columnas con encabezados NA o vacíos
  basic_table_filtered <- basic_table_filtered[, !is.na(colnames(basic_table_filtered)) & colnames(basic_table_filtered) != ""]
  
  # Crear encabezado vacío
  basic_table_head <- basic_table_filtered[0, ]
  
  # Crear contenido de la tabla
  basic_table_content <- basic_table_filtered
  
  # Agregar columna con el nombre del gen
  basic_table_content <- basic_table_content %>%
    mutate(Gene = gene, .before = 1)  # Agrega columna 'Gene' al inicio
  
  # Combinar con main_table
  main_table <- bind_rows(main_table, basic_table_content)
}

# Escribir la tabla final en un archivo CSV
output_file <- file.path(output_folder, "main_table.csv")
write.csv(main_table, output_file, row.names = FALSE)

cat("\nTabla principal creada en:", output_file, "\n")

# Encontrar duplicados en el encabezado
duplicated_columns <- duplicated(colnames(main_table))

# Mostrar los nombres de las columnas duplicadas, si existen
if (any(duplicated_columns)) {
  cat("Hay columnas duplicadas en el encabezado de 'main_table':\n")
  print(colnames(main_table)[duplicated_columns])
} else {
  cat("No hay columnas duplicadas en el encabezado de 'main_table'.\n")
}