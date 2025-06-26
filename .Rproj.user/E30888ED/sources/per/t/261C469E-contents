# # create_db.R (Modificado)
# library(readr)
# library(dplyr)
# library(stringr)

#' Crea la base de datos a partir de un archivo de entrada específico.
#'
#' @param input_folder Carpeta que contiene el archivo de entrada.
#' @param output_folder Carpeta donde se guardará el archivo de salida.
#' @param output_file_name Nombre del archivo de salida.
#' @return Un mensaje de éxito o un mensaje de error si falla.
create_database <- function(input_folder = "input_db", output_folder = "db", output_file_name = "db.csv") {
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  input_file_path <- list.files(path = input_folder, pattern = "^AT.*\\.csv$", full.names = TRUE)
  
  if (length(input_file_path) == 0) {
    return(list(success = FALSE, message = paste0("ERROR: No se encontró ningún archivo que coincida con 'AT*.csv' en la carpeta '", input_folder, "'.")))
  } else if (length(input_file_path) > 1) {
    warning(paste0("ADVERTENCIA: Se encontraron varios archivos que coinciden con 'AT*.csv'. Usando el primero: ", input_file_path[1]))
    input_file_path <- input_file_path[1]
  }
  
  output_file_path <- file.path(output_folder, output_file_name)
  
  all_lines <- readLines(input_file_path)
  
  if (length(all_lines) < 18) {
    return(list(success = FALSE, message = "ERROR: El archivo de entrada no tiene suficientes líneas para el procesamiento."))
  }
  
  raw_df <- read_csv(
    input_file_path,
    col_names = FALSE,
    col_types = cols(.default = "c"),
    skip = 0,
    n_max = -1
  )
  
  raw_df[9, 1] <- "Gene"
  raw_df[9, 2:5] <- raw_df[10, 2:5]
  
  new_header_row_vec <- as.character(raw_df[9, ])
  data_rows <- raw_df[c(11:nrow(raw_df)), ]
  
  clean_header_names <- str_replace_all(new_header_row_vec, "_.*$", "")
  unique_col_indices <- !duplicated(clean_header_names)
  final_header_names <- clean_header_names[unique_col_indices]
  final_df <- data_rows[, unique_col_indices]
  colnames(final_df) <- final_header_names
  
  normalize_allele <- function(value) {
    if (is.na(value) || !str_detect(value, "/")) {
      return(value)
    }
    parts <- str_split(value, "/", simplify = TRUE)
    sorted_parts <- sort(parts)
    return(paste(sorted_parts, collapse = "/"))
  }
  
  final_df <- final_df %>%
    mutate(across(6:ncol(.), ~sapply(., normalize_allele)))
  
  write_csv(final_df, output_file_path)
  
  return(list(success = TRUE, message = paste0("Base de datos creada y guardada en '", output_file_path, "' (", ncol(final_df), " columnas).")))
}
