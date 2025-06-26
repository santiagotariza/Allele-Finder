# # filter.R (Modificado: Corregido la identificación de columnas Hs)
# library(dplyr)
# library(stringr) # Asegurarse de cargar para str_replace

#' Aplica filtros a los archivos de coincidencias basándose en la base de datos CNVs.
#'
#' @param output_dir Carpeta que contiene los archivos de coincidencias.
#' @param cnvs_file Ruta al archivo de la base de datos CNVs (cnvs.csv).
#' @param output_processed_dir Carpeta donde se guardarán los archivos filtrados.
#' @return Un mensaje de éxito o un mensaje de error si falla.
apply_filter <- function(output_dir = "output", cnvs_file = "cnvs/cnvs.csv", output_processed_dir = "output_processed") {
  # Crear la carpeta de salida procesada si no existe
  if (!dir.exists(output_processed_dir)) {
    dir.create(output_processed_dir)
  }
  
  # Verificar si el archivo CNVs existe
  if (!file.exists(cnvs_file)) {
    return(list(success = FALSE, message = paste("ERROR: No se encontró el archivo CNVs:", cnvs_file)))
  }
  
  # Cargar el archivo cnvs.csv
  # Usar check.names = FALSE para asegurar que los nombres de las columnas 'HsXXXX_cn' se mantengan intactos
  cnvs_db <- read.csv(cnvs_file, stringsAsFactors = FALSE, check.names = FALSE)
  # Renombrar la primera columna de cnvs_db para facilitar la comparación
  names(cnvs_db)[1] <- "Sample_ID"
  
  # --- DEPURACIÓN: Nombres de columnas en cnvs_db ---
  message("DEBUG: Columnas en cnvs_db: ", paste(names(cnvs_db), collapse = ", "))
  
  # Obtener la lista de archivos de salida en la carpeta 'output'
  output_files <- list.files(output_dir, pattern = "_matches\\.csv$", full.names = TRUE)
  
  if (length(output_files) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No se encontraron archivos de coincidencias en la carpeta:", output_dir)))
  }
  
  filter_results <- list()
  for (file_path in output_files) {
    tryCatch({
      matches_df <- read.csv(file_path, stringsAsFactors = FALSE)
      
      # Dataframe para almacenar las filas que pasan el filtro
      filtered_matches_df <- data.frame()
      
      # --- DEPURACIÓN: Nombres de columnas en matches_df ---
      message(paste0("DEBUG: Columnas en matches_df (", basename(file_path), "): ", paste(names(matches_df), collapse = ", ")))
      
      # Iterar sobre cada fila de matches_df
      for (i in 1:nrow(matches_df)) {
        current_row_matches <- matches_df[i, ]
        sample_id_to_check <- current_row_matches$Sample_ID
        
        # Identificar las columnas 'Hs' en matches_df (sin _cn)
        hs_cols_in_matches_base <- grep("^Hs\\d+(_\\d+)?$", names(current_row_matches), value = TRUE)
        
        # --- DEPURACIÓN: Columnas 'Hs' identificadas en matches_df para esta fila ---
        message(paste0("DEBUG: Hs cols en matches_df (base) para Sample_ID ", sample_id_to_check, ": ", paste(hs_cols_in_matches_base, collapse = ", ")))
        
        # Extraer los valores de las columnas 'Hs' de la fila actual de matches_df
        values_from_matches <- as.character(current_row_matches[hs_cols_in_matches_base]) 
        
        # --- Lógica de filtrado ---
        
        # Condición de Conservación 1: Si hay algún NA en los valores de Hs de matches_df
        if (any(is.na(values_from_matches))) {
          filtered_matches_df <- bind_rows(filtered_matches_df, current_row_matches)
          next # Pasar a la siguiente fila
        }
        
        # Buscar el Sample_ID en cnvs_db
        cnv_row_reference <- cnvs_db %>%
          filter(Sample_ID == sample_id_to_check)
        
        # Condición de Conservación 2: Si el Sample_ID no se encuentra en cnvs_db
        if (nrow(cnv_row_reference) == 0) {
          filtered_matches_df <- bind_rows(filtered_matches_df, current_row_matches)
          next # Pasar a la siguiente fila
        }
        
        # Manejo de duplicados en cnvs_db (usar la primera fila)
        if (nrow(cnv_row_reference) > 1) {
          warning(paste("Múltiples entradas para Sample_ID", sample_id_to_check, "en cnvs.csv. Usando la primera para el filtro."))
          cnv_row_reference <- cnv_row_reference[1, ]
        }
        
        # Identificar las columnas 'Hs' en cnvs_db (con _cn)
        cnv_hs_cols_with_cn <- grep("^Hs\\d+(_\\d+)?_cn$", names(cnv_row_reference), value = TRUE)
        
        # --- DEPURACIÓN: Columnas 'Hs' identificadas en cnvs_db (con _cn) para Sample_ID ---
        message(paste0("DEBUG: Hs cols en cnvs_db (con _cn) para Sample_ID ", sample_id_to_check, ": ", paste(cnv_hs_cols_with_cn, collapse = ", ")))
        
        # Extraer los nombres base de las columnas Hs de cnvs_db (sin _cn)
        cnv_hs_cols_base <- str_replace(cnv_hs_cols_with_cn, "_cn$", "")
        
        # Obtener las columnas Hs comunes por su nombre base
        common_hs_cols_base <- intersect(hs_cols_in_matches_base, cnv_hs_cols_base)
        
        # --- DEPURACIÓN: Columnas 'Hs' comunes (base) ---
        message(paste0("DEBUG: Hs cols comunes (base) para Sample_ID ", sample_id_to_check, ": ", paste(common_hs_cols_base, collapse = ", ")))
        
        # Condición de Conservación 3: Si no se encontraron columnas Hs comunes (por nombre base)
        if (length(common_hs_cols_base) == 0) {
          filter_results <- c(filter_results, paste0("ADVERTENCIA: No se encontraron columnas 'Hs' coincidentes entre matches_df y cnvs.csv para Sample_ID ", sample_id_to_check, ". Fila conservada por precaución."))
          filtered_matches_df <- bind_rows(filtered_matches_df, current_row_matches)
          next
        }
        
        # Verificar si *TODOS* los valores de 'matches_df' son diferentes de sus homólogos en 'cnvs_db'
        all_different <- TRUE 
        for (base_col_name in common_hs_cols_base) {
          val_matches <- as.character(current_row_matches[[base_col_name]])
          val_cnvs <- as.character(cnv_row_reference[[paste0(base_col_name, "_cn")]]) # Usar el nombre con _cn para cnvs_db
          
          # --- DEPURACIÓN: Valores comparados ---
          message(paste0("DEBUG: Comparando Sample_ID ", sample_id_to_check, ", Columna: ", base_col_name, 
                         " | matches_df: '", val_matches, "' | cnvs_db: '", val_cnvs, "'"))
          
          # Si alguno de los valores es NA O si son idénticos, entonces NO son "todos diferentes".
          if (is.na(val_matches) || is.na(val_cnvs) || val_matches == val_cnvs) {
            all_different <- FALSE
            break # No es necesario seguir revisando, la condición "todos diferentes" ya es falsa.
          }
        }
        
        # Aplicar la regla de filtrado específica:
        # Si 'all_different' es TRUE, significa que todos los valores 'Hs' comparados son diferentes y no-NA.
        # En este caso, la fila debe ser FILTRADA (no se añade a filtered_matches_df).
        if (!all_different) { # Si NO son todos diferentes (es decir, al menos uno coincide o es NA), CONSERVAR la fila.
          filtered_matches_df <- bind_rows(filtered_matches_df, current_row_matches)
        }
        # Si 'all_different' es TRUE, la fila no se añade, por lo tanto, se filtra.
      }
      
      # --- Guardar el archivo procesado si hay resultados ---
      if (nrow(filtered_matches_df) > 0) {
        output_file_name <- basename(file_path)
        output_file_name_filtered <- paste0(tools::file_path_sans_ext(output_file_name), "_filtered.csv")
        
        write.csv(filtered_matches_df, file.path(output_processed_dir, output_file_name_filtered), row.names = FALSE)
        filter_results <- c(filter_results, paste0("Archivo procesado generado: ", output_file_name_filtered, " con ", nrow(filtered_matches_df), " filas totales."))
      } else {
        filter_results <- c(filter_results, paste0("No se encontraron filas que cumplan los criterios de filtrado para ", basename(file_path), ". Archivo no generado."))
      }
    }, error = function(e) {
      filter_results <- c(filter_results, paste0("ERROR procesando ", basename(file_path), ": ", e$message))
    })
  }
  
  if (length(filter_results) > 0) {
    return(list(success = TRUE, message = paste("Procesamiento de archivos de salida completado.\n", paste(filter_results, collapse = "\n"))))
  } else {
    return(list(success = FALSE, message = "ERROR: No se pudo aplicar el filtro a ningún archivo de salida."))
  }
}
