# # search.R (Modificado)
# library(dplyr)

#' Realiza la búsqueda de coincidencias entre muestras y la base de datos.
#'
#' @param samples_dir Ruta a la carpeta que contiene los archivos de muestras procesados.
#' @param db_file Ruta al archivo de la base de datos (db.csv).
#' @param output_dir Carpeta donde se guardarán los archivos de salida.
#' @return Un mensaje de éxito o un mensaje de error si falla.
perform_search <- function(samples_dir = "samples", db_file = "db/db.csv", output_dir = "output") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  if (!file.exists(db_file)) {
    return(list(success = FALSE, message = paste("ERROR: No se encontró el archivo de la base de datos:", db_file)))
  }
  
  db <- read.csv(db_file, stringsAsFactors = FALSE)
  for (col_name in names(db)[-c(1:5)]) {
    db[[col_name]] <- as.character(db[[col_name]])
    db[[col_name]][db[[col_name]] == ""] <- NA
  }
  
  sample_files <- list.files(samples_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(sample_files) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No se encontraron archivos .csv de muestras en la carpeta:", samples_dir)))
  }
  
  search_results <- list()
  for (sample_file in sample_files) {
    tryCatch({
      sample <- read.csv(sample_file, stringsAsFactors = FALSE)
      for (col_name in names(sample)[-1]) {
        sample[[col_name]] <- as.character(sample[[col_name]])
      }
      
      sample_name <- tools::file_path_sans_ext(basename(sample_file))
      sample_rsIDs <- names(sample)[-1]
      db_rsIDs <- names(db)[-c(1:5)]
      common_rsIDs <- intersect(sample_rsIDs, db_rsIDs)
      
      matches <- data.frame()
      
      for (rsID_col in sample_rsIDs) {
        sample[[rsID_col]][sample[[rsID_col]] %in% c("NOAMP", "UND")] <- NA
      }
      
      for (i in 1:nrow(sample)) {
        for (j in 1:nrow(db)) {
          matched_rsIDs_for_this_comparison <- c()
          non_na_db_rsIDs_to_match <- 0
          actual_matches_count <- 0
          
          for (rsID in common_rsIDs) {
            sample_val <- sample[i, rsID]
            db_val <- db[j, rsID]
            
            if (!is.na(db_val)) {
              non_na_db_rsIDs_to_match <- non_na_db_rsIDs_to_match + 1
              if (identical(sample_val, db_val) || (is.na(sample_val) && is.na(db_val))) {
                actual_matches_count <- actual_matches_count + 1
                matched_rsIDs_for_this_comparison <- c(matched_rsIDs_for_this_comparison, rsID)
              }
            }
          }
          
          if (non_na_db_rsIDs_to_match > 0 && actual_matches_count == non_na_db_rsIDs_to_match) {
            matches <- bind_rows(matches, data.frame(
              Sample_ID = sample[i, 1],
              db[j, 1:5],
              rsIDs_matched = paste(matched_rsIDs_for_this_comparison, collapse = ", "),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      if (nrow(matches) > 0) {
        write.csv(matches, file.path(output_dir, paste0(sample_name, "_matches.csv")), row.names = FALSE)
        search_results <- c(search_results, paste0("Archivo generado: ", paste0(sample_name, "_matches.csv"), " con ", nrow(matches), " coincidencias."))
      } else {
        search_results <- c(search_results, paste0("No se encontraron coincidencias para ", sample_name, "."))
      }
    }, error = function(e) {
      search_results <- c(search_results, paste0("ERROR procesando ", basename(sample_file), ": ", e$message))
    })
  }
  if (length(search_results) > 0) {
    return(list(success = TRUE, message = paste("Búsqueda completada.\n", paste(search_results, collapse = "\n"))))
  } else {
    return(list(success = FALSE, message = "ERROR: No se pudo realizar la búsqueda en ninguna muestra."))
  }
}
