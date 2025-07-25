# main.R
# Centralized execution script for the Allele-Finder pipeline.
# This orchestrates the full sequence of steps from raw input to filtered outputs.

# Load processing scripts
source("scripts/samples_processing.R")
source("scripts/cnvs_processing.R")
source("scripts/create_db.R")
source("scripts/search.R")
source("scripts/filter.R")

#' Run the complete Allele-Finder analysis pipeline.
#'
#' This function executes the full workflow of the Allele-Finder tool,
#' including:
#'  1. Sample data preprocessing
#'  2. CNV file consolidation and validation
#'  3. Optional allele database generation
#'  4. Match searching between sample and database
#'  5. Filtering of matches based on CNV profiles
#'
#' Each step includes error handling and status messaging.
#'
#' @param create_db_optional Logical flag. If TRUE, recreates the allele database using create_db.R.
#' @return A named list:
#'   \item{success}{TRUE if all steps completed successfully; FALSE otherwise}
#'   \item{message}{A detailed multi-line status message of the workflow execution}
run_workflow <- function(create_db_optional = FALSE) {
  all_messages <- c()
  
  # Step 1: Sample preprocessing
  message("Starting Step 1: Processing samples...")
  result_samples <- tryCatch({
    process_samples()
  }, error = function(e) {
    list(success = FALSE, message = paste("Sample processing failed:", e$message))
  })
  all_messages <- c(all_messages, paste("Step 1 (Samples):", result_samples$message))
  if (!result_samples$success)
    return(list(success = FALSE, message = paste(all_messages, collapse = "\n")))
  
  # Step 2: CNV consolidation
  message("Starting Step 2: Processing CNVs...")
  result_cnvs <- tryCatch({
    process_cnvs()
  }, error = function(e) {
    list(success = FALSE, message = paste("CNV processing failed:", e$message))
  })
  all_messages <- c(all_messages, paste("Step 2 (CNVs):", result_cnvs$message))
  if (!result_cnvs$success)
    return(list(success = FALSE, message = paste(all_messages, collapse = "\n")))
  
  # Step 3 (optional): Recreate allele database
  if (create_db_optional) {
    message("Starting Step 3 (Optional): Creating the database...")
    result_db <- tryCatch({
      create_database()
    }, error = function(e) {
      list(success = FALSE, message = paste("DB creation failed:", e$message))
    })
    all_messages <- c(all_messages, paste("Step 3 (DB):", result_db$message))
    if (!result_db$success)
      return(list(success = FALSE, message = paste(all_messages, collapse = "\n")))
  } else {
    all_messages <- c(all_messages, "Step 3 (DB): Skipped (regeneration not requested).")
  }
  
  # Step 4: Match search
  message("Starting Step 4: Performing search...")
  result_search <- tryCatch({
    perform_search()
  }, error = function(e) {
    list(success = FALSE, message = paste("Search failed:", e$message))
  })
  all_messages <- c(all_messages, paste("Step 4 (Search):", result_search$message))
  if (!result_search$success)
    return(list(success = FALSE, message = paste(all_messages, collapse = "\n")))
  
  # Step 5: Filter results based on CNVs
  message("Starting Step 5: Applying filter...")
  result_filter <- tryCatch({
    apply_filter()
  }, error = function(e) {
    list(success = FALSE, message = paste("Filter failed:", e$message))
  })
  all_messages <- c(all_messages, paste("Step 5 (Filter):", result_filter$message))
  if (!result_filter$success)
    return(list(success = FALSE, message = paste(all_messages, collapse = "\n")))
  
  return(list(
    success = TRUE,
    message = paste("Workflow completed successfully!\n\nDetails:\n", paste(all_messages, collapse = "\n"))
  ))
}
