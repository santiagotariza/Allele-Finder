# Configuración inicial
library(dplyr)
library(tidyverse)

# Rutas de los archivos
input_folder_definitions <- "updated_data"
input_folder_haplotypes <- "processed_data"
genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP3A4", "CYP1A2", "CYP2B6")
#genes <- c("CYP2C19", "CYP2C9", "CYP2D6", "CYP2B6")

output_folder <- "merged_data"

# Crear carpeta de salida si no existe
if (!dir.exists(output_folder)) dir.create(output_folder)

# Bases
valid_bases <- c("A", "T", "C", "G")

# Función principal
for (gene in genes) {
  cat("\nProcesando gen:", gene, "\n")
  
  # Leer tablas
  definition_file <- file.path(input_folder_definitions, paste0(gene, "_allele_definition_table.csv"))
  haplotype_file <- file.path(input_folder_haplotypes, paste0(gene, "_haplotypes_table.csv"))
  
  definition_data <- read.csv(definition_file, stringsAsFactors = FALSE, check.names = FALSE)
  haplotype_data <- read.csv(haplotype_file, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Paso 1: Preparar la tabla de alelos
  definition_data_filtered <- definition_data[ -c(0:4),] 
  colnames(definition_data_filtered) <- as.character(unlist(definition_data_filtered[1, ]))
  definition_data_filtered <- definition_data_filtered[ -c(1,2),]
  definition_data_head <- definition_data_filtered[1,]
  
  # Paso 2: Preparar la tabla de haplotipos
  haplotype_data_slimmed <- haplotype_data[, c(1,3,8)] %>%
    filter(rsID != "-")  # Excluir filas donde rsID es "-"
  
  # Identificar duplicados
  duplicates <- haplotype_data_slimmed %>%
    group_by(`Haplotype Name`, rsID) %>%
    filter(n() > 1)
  
  if (nrow(duplicates) > 0) {
    cat("Duplicados encontrados:\n")
    print(duplicates)
  }
  
  # Transformar la tabla a formato ancho
  haplotype_shifted <- haplotype_data_slimmed %>%
    pivot_wider(
      names_from = rsID,       # Los rsID serán las columnas
      values_from = 'Variant Allele'  # Los valores en las celdas serán los valores de Variant Allele
    )
  
  cat("Nombres de las columnas de haplotype_data:\n")
  print(colnames(haplotype_data))
  
  # Renombrar una columna
  haplotype_shifted <- haplotype_shifted %>% rename( rsID = 'Haplotype Name')
  
  # Crear una expresión regular que combine todos los genes
  pattern <- paste0("(", paste(genes, collapse = "|"), ")")
  
  # Eliminar el patrón
  haplotype_shifted$rsID <- gsub(pattern, "", haplotype_shifted$rsID)
  
  # Obtener el orden de columnas deseado
  rsid_order <- colnames(definition_data_head)
  
  # Columnas de haplotype_shifted que existen en rsid_order
  cols_in_haplo <- intersect(rsid_order, colnames(haplotype_shifted))
  
  # Seleccionar solo esas columnas en haplotype_shifted
  haplo_subset <- haplotype_shifted %>% select(all_of(cols_in_haplo))
  
  # Crear un data.frame vacío con columnas en el orden de rsid_order y el número de filas de haplo_subset
  empty_df <- as.data.frame(matrix(NA, nrow = nrow(haplo_subset), ncol = length(rsid_order)))
  colnames(empty_df) <- rsid_order
  
  # Rellenar las columnas existentes con datos de haplo_subset
  empty_df[ , cols_in_haplo] <- haplo_subset
  
  # Finalmente, unir filas
  combined <- bind_rows(definition_data_head, empty_df)
  
  # Recorremos cada columna
  for (col in colnames(combined)) {
    # Tomamos el valor de la primera fila para esa columna
    fill_value <- combined[[col]][1]
    
    # Reemplazamos NA por ese valor
    combined[[col]][is.na(combined[[col]])] <- fill_value
  }
  
  # 1. Reemplazar el nombre de columnas (header) de combined por el de definition_data_filtered
  colnames(combined) <- colnames(definition_data)
  
  # 2. Tomar las primeras 6 filas de definition_data_filtered
  first_six_rows <- definition_data[1:6, ]
  
  # 3. Agregar esas filas arriba de combined
  combined_final <- bind_rows(first_six_rows, combined)
  
  # Buscar filas en definition_data que no están en combined_final
  # Asegúrate de usar el nombre de la columna correctamente
  missing_rows <- definition_data %>%
    filter(!(definition_data[[1]] %in% combined_final[[1]]))
  
  # Agregar las filas faltantes al final de combined_final
  combined_final <- bind_rows(combined_final, missing_rows)
  
  # Nombre de la primera columna
  column_name <- colnames(combined_final)[1]  # Nombre de la primera columna
  
  # Dividir las filas
  first_six_rows <- combined_final[1:6, ]  # Las primeras 6 filas
  remaining_rows <- combined_final[7:nrow(combined_final), ]  # Filas a partir de la fila 7
  
  # Extraer la parte numérica para ordenación
  remaining_rows <- remaining_rows %>%
    mutate(NumericPart = as.numeric(gsub("[^0-9.]", "", .data[[column_name]]))) %>%  # Extraer número
    arrange(NumericPart) %>%  # Ordenar por la parte numérica
    select(-NumericPart)  # Eliminar columna auxiliar
  
  # Combinar nuevamente las tablas
  combined_final <- bind_rows(first_six_rows, remaining_rows)
  
  # Resultado
  print("Tabla combinada y ordenada:")
  print(combined_final)
  
  # Paso 4: Guardar la tabla actualizada
  output_file <- file.path(output_folder, paste0(gene, "_merged_table.csv"))
  write.csv(combined_final, output_file, row.names = FALSE)
  cat("Tabla mergeada guardada en:", output_file, "\n")
}