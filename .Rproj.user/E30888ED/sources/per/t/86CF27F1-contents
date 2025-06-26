# # samples_processing.R (Modificado)
# library(tidyverse)
# library(fs)
# library(data.table) # Para usar fwrite

#' Procesa archivos ZIP de muestras, extrae datos de genotipos, los fusiona y guarda el resultado.
#'
#' @param samples_dir Ruta a la carpeta que contiene los archivos ZIP de muestras.
#' @return Un mensaje de éxito o un mensaje de error si falla.
process_samples <- function(samples_dir = "samples") {
  # Crear una lista de todos los archivos .zip en la carpeta 'samples'
  zip_files <- dir_ls(samples_dir, glob = "*.zip")
  
  if (length(zip_files) == 0) {
    return(list(success = FALSE, message = paste("ERROR: No se encontraron archivos .zip en la carpeta:", samples_dir)))
  }
  
  results <- list()
  for (zip_file in zip_files) {
    tryCatch({
      zip_name <- path_file(zip_file)
      folder_name <- path_ext_remove(zip_name)
      folder_path <- file.path(samples_dir, folder_name)
      
      if (!dir.exists(folder_path)) {
        dir.create(folder_path)
      }
      
      unzip(zip_file, exdir = folder_path)
      csv_file <- file.path(folder_path, "Genotype Matrix.csv")
      
      if (!file.exists(csv_file)) {
        stop(paste("No se encontró el archivo 'Genotype Matrix.csv' en", folder_path))
      }
      
      genotype_data <- read.csv(csv_file, comment.char = "#", stringsAsFactors = FALSE)
      
      if (nrow(genotype_data) == 0) {
        warning(paste("El archivo está vacío o no contiene datos:", csv_file))
        next
      }
      
      empty_row <- which(genotype_data == "Empty", arr.ind = TRUE)[1]
      if (!is.na(empty_row)) {
        genotype_data <- genotype_data[1:(empty_row - 1), ]
      }
      
      genotype_data <- data.frame(lapply(genotype_data, as.character), stringsAsFactors = FALSE)
      colnames(genotype_data) <- sub("_.*", "", colnames(genotype_data))
      genotype_data <- genotype_data[, c(1, order(colnames(genotype_data)[-1]) + 1)]
      
      base_rsids_with_variants <- unique(
        sub("\\.1$", "", grep("^rs[0-9]+\\.1$", colnames(genotype_data), value = TRUE))
      )
      
      merged_genotypes <- genotype_data[, 1, drop = FALSE]
      cols_to_remove_from_genotype_data <- c()
      
      merge_genotypes <- function(val_rsid, val_rsid_dot1) {
        is_valid_genotype <- function(val) {
          !(val %in% c("NOAMP", "UND", "") || is.na(val))
        }
        
        val_rsid_is_valid <- is_valid_genotype(val_rsid)
        val_rsid_dot1_is_valid <- is_valid_genotype(val_rsid_dot1)
        
        if (val_rsid_is_valid && val_rsid_dot1_is_valid) {
          if (val_rsid == val_rsid_dot1) {
            return(val_rsid)
          } else {
            return(val_rsid)
          }
        } else if (val_rsid_is_valid) {
          return(val_rsid)
        } else if (val_rsid_dot1_is_valid) {
          return(val_rsid_dot1)
        } else {
          return(val_rsid)
        }
      }
      
      for (base_col in base_rsids_with_variants) {
        variant_col <- paste0(base_col, ".1")
        if (base_col %in% colnames(genotype_data) && variant_col %in% colnames(genotype_data)) {
          merged_result <- mapply(merge_genotypes,
                                  genotype_data[[base_col]],
                                  genotype_data[[variant_col]],
                                  SIMPLIFY = TRUE, USE.NAMES = FALSE)
          merged_genotypes[[base_col]] <- merged_result
          cols_to_remove_from_genotype_data <- c(cols_to_remove_from_genotype_data, variant_col)
        } else if (base_col %in% colnames(genotype_data)) {
          merged_genotypes[[base_col]] <- genotype_data[[base_col]]
        }
      }
      
      all_rs_base_cols <- grep("^rs[0-9]+$", colnames(genotype_data), value = TRUE)
      rs_cols_without_variants <- setdiff(all_rs_base_cols, base_rsids_with_variants)
      
      for(col in rs_cols_without_variants) {
        merged_genotypes[[col]] <- genotype_data[[col]]
      }
      
      other_non_rs_cols <- setdiff(colnames(genotype_data), c(all_rs_base_cols, cols_to_remove_from_genotype_data, colnames(merged_genotypes)))
      for(col in other_non_rs_cols) {
        merged_genotypes[[col]] <- genotype_data[[col]]
      }
      
      non_rs_columns_final <- grep("^rs[0-9]+", colnames(genotype_data), invert = TRUE, value = TRUE)
      final_data <- genotype_data[, non_rs_columns_final, drop = FALSE]
      
      if ("Sample.Assay" %in% colnames(final_data) && "Sample.Assay" %in% colnames(merged_genotypes)) {
        merged_genotypes <- merged_genotypes %>% select(-`Sample.Assay`)
      }
      
      genotype_data <- bind_cols(final_data, merged_genotypes)
      first_col_name <- colnames(genotype_data)[1]
      other_cols <- setdiff(colnames(genotype_data), first_col_name)
      genotype_data <- genotype_data[, c(first_col_name, sort(other_cols))]
      
      output_csv <- file.path(samples_dir, paste0(folder_name, "_Genotype_Matrix_short.csv"))
      fwrite(genotype_data, output_csv)
      results <- c(results, paste0(" - Genotype Data: ", output_csv))
      
    }, error = function(e) {
      warning(paste("Error procesando", zip_file, ":", e$message))
    }, finally = {
      if (exists("folder_path") && dir.exists(folder_path)) {
        unlink(folder_path, recursive = TRUE)
      }
    })
  }
  
  if (length(results) > 0) {
    return(list(success = TRUE, message = paste("Procesamiento de muestras completado con éxito. Archivos generados:", paste(results, collapse = "\n"))))
  } else {
    return(list(success = FALSE, message = "ERROR: No se pudo procesar ninguna muestra exitosamente."))
  }
}
