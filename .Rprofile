# .Rprofile
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# 1. Check if renv package is available after activation attempt.
if (requireNamespace("renv", quietly = TRUE)) {
  # 2. Check if the project library is synchronized with the lockfile.
  message("\n[Allele-Finder] Checking and restoring project package environment with renv::restore()...")
  tryCatch({
    # Attempt to restore packages. 'prompt = FALSE' makes it non-interactive,
    # which is crucial for automated startup without user intervention.
    renv::restore(prompt = FALSE)
    message("[Allele-Finder] renv environment restored/synchronized successfully.")
  }, error = function(e) {
    message("[Allele-Finder] ERROR during automated renv restore: ", e$message)
    message("[Allele-Finder] Please run 'renv::restore()' manually in the R console to fix this.")
  })
} else {
  # This block runs if renv::activate.R failed to load renv, or renv is genuinely missing.
  message("\n[Allele-Finder] The 'renv' package is not loaded.")
  message("[Allele-Finder] Attempting to install 'renv' package globally...")
  tryCatch({
    install.packages("renv")
    message("[Allele-Finder] 'renv' installed. Please RESTART RStudio or REOPEN the project to activate it and restore dependencies.")
  }, error = function(e) {
    message("[Allele-Finder] FAILED to install 'renv' globally: ", e$message)
    message("[Allele-Finder] Please install 'renv' manually (install.packages('renv')) and then reopen the project.")
  })
}
