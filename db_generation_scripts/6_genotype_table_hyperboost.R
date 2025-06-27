start_time <- Sys.time()

# Cargar librerías necesarias
library(data.table)

# Leer la tabla de haplotipos
input_folder_definitions <- "the_table"
output_folder <- "the_table"

basic_file <- file.path(input_folder_definitions, "main_table.csv")
data <- fread(basic_file)

# Función para generar genotipos por gen
generate_genotypes <- function(gene_data) {
  # Generar todas las combinaciones posibles de haplotipos
  comb <- t(combn(nrow(gene_data), 2))
  
  # Seleccionar haplotipos combinados
  hap1 <- gene_data[comb[, 1], ]
  hap2 <- gene_data[comb[, 2], ]
  
  # Crear tabla de genotipos
  result <- data.table(
    Gene = hap1$Gene,
    Haplotype1 = hap1$rsID,
    Haplotype2 = hap2$rsID
  )
  
  # Combinar los alelos para cada rsID
  for (col in names(gene_data)[3:ncol(gene_data)]) {
    result[[col]] <- paste(hap1[[col]], hap2[[col]], sep = "|")
  }
  
  return(result)
}

# Generar genotipos por cada gen y combinar resultados
genotypes <- rbindlist(lapply(split(data, data$Gene), generate_genotypes))

# Guardar la tabla de genotipos
output_file <- file.path(output_folder, "genotypes_table_hyperboost.csv")
fwrite(genotypes, output_file)

cat("\nTabla de genotipos generada y guardada como 'genotypes_table_hyperboost.csv'.\n")



end_time <- Sys.time()
print(end_time - start_time)


# Verificar que no hay duplicados genotipicos en la tabla
# duplicated_combinations <- genotypes[, .N, by = .(Gene, Haplotype1, Haplotype2)][N > 1]
# print(duplicated_combinations)