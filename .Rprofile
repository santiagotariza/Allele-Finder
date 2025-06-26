# Asegura que el entorno de paquetes del proyecto de renv esté activo.
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# Verifica si hay paquetes faltantes o desactualizados y los restaura automáticamente.
if (requireNamespace("renv", quietly = TRUE) && renv::is_renv_activated()) {
  message("Verificando el estado del entorno de paquetes de Allele-Finder...")
  # renv::status()$synchronized verifica si la biblioteca del proyecto coincide con renv.lock
  if (!renv::status(quiet = TRUE)$synchronized) {
    message("Se detectan paquetes faltantes o desactualizados. Iniciando restauración automática de renv...")
    tryCatch({
      renv::restore()
      message("Restauración de renv completada con éxito.")
    }, error = function(e) {
      message("Error durante la restauración de renv: ", e$message)
      message("Por favor, intente ejecutar 'renv::restore()' manualmente en la consola de R si el problema persiste.")
    })
  } else {
    message("El entorno de Allele-Finder ya está sincronizado. Listo para usar.")
  }
} else if (!requireNamespace("renv", quietly = TRUE)) {
  message("El paquete 'renv' no está instalado globalmente. Intentando instalarlo para Allele-Finder...")
  tryCatch({
    install.packages("renv")
    message("El paquete 'renv' se instaló. Por favor, reinicie RStudio o el proyecto para activar renv y restaurar el entorno.")
  }, error = function(e) {
    message("Falló la instalación de 'renv'. Por favor, instálelo manualmente con install.packages('renv').")
  })
}
