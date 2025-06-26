# # cnvs_processing.R (Modificado)
# library(dplyr)
# library(tidyr)
# library(magrittr)

#' Procesa archivos CNVs, los consolida y valida.
#'
#' @param input_dir Carpeta que contiene los archivos .csv de entrada.
#' @param output_file Archivo de salida consolidado.
#' @param header_pattern Patrón para identificar el inicio de una tabla.
#' @return Un mensaje de éxito o un mensaje de error si falla.
process_cnvs <- function(input_dir = "input_cnvs", output_file = "cnvs/cnvs.csv", header_pattern = "^Sample Name,Target,Reference") {
  if (!dir.exists("cnvs")) {
    dir.create("cnvs")
  }
  if (!dir.exists(input_dir)) {
    return(list(success = FALSE, message = paste("ERROR: La carpeta de entrada '", input_dir, "' no existe. Asegúrate de que los archivos .csv estén ahí.", sep = "")))
  }
  
  input_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(input_files) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No se encontraron archivos .csv en la carpeta:", input_dir)))
  }
  
  all_long_data_frames <- list()
  error_messages <- c()
  
  for (current_input_file in input_files) {
    current_file_name <- basename(current_input_file)
    tryCatch({
      all_lines <- readLines(current_input_file, warn = FALSE)
      start_indices <- grep(header_pattern, all_lines)
      
      if (length(start_indices) == 0) {
        stop(sprintf("No se encontró ninguna sección que comenzara con el encabezado '%s' en el archivo '%s'. Revise el formato del archivo.", header_pattern, current_file_name))
      }
      
      list_of_dataframes_for_current_file <- list()
      for (i in 1:length(start_indices)) {
        current_start <- start_indices[i]
        section_end_line <- length(all_lines)
        if (length(all_lines) >= 3) {
          for (j in (current_start + 1):(length(all_lines) - 2)) {
            is_line1_empty <- trimws(all_lines[j]) == ""
            is_line2_empty <- trimws(all_lines[j + 1]) == ""
            is_line3_empty <- trimws(all_lines[j + 2]) == ""
            if (is_line1_empty && is_line2_empty && is_line3_empty) {
              section_end_line <- j - 1
              break
            }
          }
        } else {
          section_end_line <- length(all_lines)
        }
        
        header_line_text <- all_lines[current_start]
        data_lines_text <- if (section_end_line >= (current_start + 1)) {
          all_lines[(current_start + 1):section_end_line]
        } else {
          NULL
        }
        
        header_df_temp <- read.csv(text = header_line_text, header = TRUE, stringsAsFactors = FALSE)
        col_names_in_r <- names(header_df_temp)
        col_types_list <- rep("character", length(col_names_in_r))
        names(col_types_list) <- col_names_in_r
        
        cn_col_name_in_r <- NULL
        if ("CN.Predicted" %in% col_names_in_r) {
          col_types_list["CN.Predicted"] <- "numeric"
          cn_col_name_in_r <- "CN.Predicted"
        } else if ("CN.Calculated" %in% col_names_in_r) {
          col_types_list["CN.Calculated"] <- "numeric"
          cn_col_name_in_r <- "CN.Calculated"
        } else {
          stop(sprintf("Ni 'CN Predicted' ni 'CN Calculated' encontrados en el encabezado de la sección %d del archivo '%s' (línea de encabezado: %d). Revise el encabezado.", i, current_file_name, current_start))
        }
        
        if (is.null(data_lines_text) || length(data_lines_text) == 0) {
          empty_df <- data.frame(matrix(ncol = length(col_names_in_r), nrow = 0))
          names(empty_df) <- col_names_in_r
          empty_df$OriginalLineNumber <- integer(0)
          list_of_dataframes_for_current_file[[i]] <- empty_df
          next
        }
        
        temp_df <- suppressWarnings(
          read.csv(text = data_lines_text, header = FALSE, stringsAsFactors = FALSE, colClasses = col_types_list)
        )
        names(temp_df) <- col_names_in_r
        
        if (!is.null(cn_col_name_in_r) && !is.numeric(temp_df[[cn_col_name_in_r]]) && any(!is.na(temp_df[[cn_col_name_in_r]]))) {
          stop(sprintf("La columna '%s' en la sección %d del archivo '%s' contiene valores no numéricos que no pudieron ser convertidos a NA. Revise las líneas %d a %d.", cn_col_name_in_r, i, current_file_name, current_start + 1, section_end_line))
        }
        
        temp_df$OriginalLineNumber <- (current_start + 1) + (0:(nrow(temp_df) - 1))
        list_of_dataframes_for_current_file[[i]] <- temp_df
      }
      
      combined_data <- dplyr::bind_rows(list_of_dataframes_for_current_file)
      
      cn_value_col <- NULL
      if ("CN.Predicted" %in% names(combined_data)) {
        cn_value_col <- "CN.Predicted"
      } else if ("CN.Calculated" %in% names(combined_data)) {
        cn_value_col <- "CN.Calculated"
      } else {
        stop(sprintf("Ni 'CN.Predicted' ni 'CN.Calculated' encontrados después de la combinación de secciones del archivo '%s'.", current_file_name))
      }
      
      selected_data <- combined_data %>%
        select(
          `Sample.Name`,
          Target,
          !!sym(cn_value_col),
          OriginalLineNumber
        ) %>%
        rename(`Sample ID` = `Sample.Name`, `CN.Value` = !!sym(cn_value_col))
      selected_data$SourceFile <- current_file_name
      all_long_data_frames[[current_file_name]] <- selected_data
      
    }, error = function(e) {
      error_messages <<- c(error_messages, sprintf("ERROR en el archivo '%s': %s", current_input_file, e$message))
    })
  }
  
  if (length(all_long_data_frames) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No se pudo procesar ningún archivo CNV exitosamente.\n", paste(error_messages, collapse = "\n"))))
  }
  
  consolidated_long_data <- bind_rows(all_long_data_frames)
  
  inconsistent_duplicates <- consolidated_long_data %>%
    group_by(`Sample ID`, Target) %>%
    filter(n_distinct(`CN.Value`) > 1) %>%
    ungroup()
  
  if (nrow(inconsistent_duplicates) > 0) {
    inconsistency_report <- inconsistent_duplicates %>%
      select(`Sample ID`, Target, `CN.Value`, SourceFile, OriginalLineNumber) %>%
      arrange(`Sample ID`, Target, SourceFile, OriginalLineNumber)
    return(list(success = FALSE, message = sprintf("ERROR: Se encontraron inconsistencias en 'CN.Value' para 'Sample ID' y 'Target' duplicados. Revise los datos fuente.\nDetalles:\n%s", paste(capture.output(as.data.frame(inconsistency_report)), collapse = "\n"))))
  }
  
  processed_long_data <- consolidated_long_data %>%
    distinct(`Sample ID`, Target, `CN.Value`, .keep_all = FALSE)
  
  final_cnvs_db <- processed_long_data %>%
    pivot_wider(
      names_from = Target,
      values_from = `CN.Value`,
      id_cols = `Sample ID`,
      names_repair = "minimal"
    )
  
  final_cnvs_db <- final_cnvs_db %>%
    filter(rowSums(!is.na(select(., -`Sample ID`))) > 0)
  
  write.csv(final_cnvs_db, output_file, row.names = FALSE, na = "")
  
  return(list(success = TRUE, message = paste0("Procesamiento de CNVs completado con éxito. Tabla guardada en '", output_file, "' (", nrow(final_cnvs_db), " filas).")))
}
