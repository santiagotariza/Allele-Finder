# app.R

# Libraries for the Shiny user interface
library(shiny)
library(shinydashboard) # For a structured dashboard layout
library(shinyjs)        # For JavaScript functionalities

# Libraries for data manipulation and processing
library(tidyverse)      # Includes dplyr, tidyr, ggplot2, readr, stringr, forcats, purrr
# Note: dplyr, tidyr, readr, stringr are already covered by tidyverse
# Magrittr is also part of tidyverse and its pipes are exported.

# Specific libraries that might not be in tidyverse
library(magrittr)       # For the pipe operator if not using all of tidyverse
library(fs)             # For file system operations
library(data.table)     # For efficient data reading/writing (e.g., fwrite)


# Load functions from processing scripts
# Make sure scripts are in a 'scripts' folder or in the same directory.
source("main.R")

# Define the user interface
ui <- dashboardPage(
  dashboardHeader(title = "Allele-Matcher"), # Updated title to "Allele-Finder"
  dashboardSidebar(
    sidebarMenu(
      menuItem("Processing", tabName = "processing", icon = icon("dna")),
      menuItem("Folder Configuration", tabName = "folders", icon = icon("folder"))
    )
  ),
  dashboardBody(
    useShinyjs(), # Initialize shinyjs
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f0f2f5; /* Light gray background */
        }
        .box {
          border-radius: 15px; /* Rounded corners for boxes */
          box-shadow: 0 4px 8px rgba(0,0,0,0.1); /* Subtle shadow */
        }
        .btn-custom {
          background-color: #4CAF50; /* Green */
          color: white;
          padding: 10px 20px;
          border: none;
          border-radius: 8px;
          cursor: pointer;
          font-size: 16px;
          transition: background-color 0.3s ease;
          margin-bottom: 10px;
        }
        .btn-custom:hover {
          background-color: #45a049;
        }
        .btn-custom:disabled {
            background-color: #cccccc;
            cursor: not-allowed;
        }
        .status-message {
            margin-top: 20px;
            padding: 15px;
            border-radius: 8px;
            border: 1px solid #ddd;
            background-color: #e9e9e9;
            white-space: pre-wrap; /* Preserve newlines */
            font-family: 'Courier New', Courier, monospace;
            max-height: 400px; /* Limit height */
            overflow-y: auto; /* Add scroll if content exceeds height */
        }
        .status-success {
            background-color: #d4edda;
            color: #155724;
            border-color: #c3e6cb;
        }
        .status-error {
            background-color: #f8d7da;
            color: #721c24;
            border-color: #f5c6cb;
        }
        .status-info {
            background-color: #d1ecf1;
            color: #0c5460;
            border-color: #bee5eb;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "processing",
              h2("Run Processing Workflow"),
              fluidRow(
                box(
                  width = 12,
                  title = "Execution Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  checkboxInput("create_db_opt", "Regenerate Database (create_db.R)", value = FALSE),
                  actionButton("run_all", "Run Complete Workflow", class = "btn-custom"),
                  hr(),
                  h3("Process Status"),
                  div(id = "status_box", class = "status-message status-info", "Awaiting execution...")
                )
              )
      ),
      tabItem(tabName = "folders",
              h2("Folder Configuration"),
              fluidRow(
                box(
                  width = 12,
                  title = "Folder Paths",
                  status = "info",
                  solidHeader = TRUE,
                  p("Ensure the following folders exist at the same level as your 'app.R' and 'main.R':"),
                  tags$ul(
                    tags$li("scripts/ (contains samples_processing.R, cnvs_processing.R, etc.)"),
                    tags$li("input_db/ (for the DB AT*.csv file)"),
                    tags$li("input_cnvs/ (for CNVs .csv files)"),
                    tags$li("samples/ (for sample .zip files)"),
                    tags$li("db/ (output of create_db.R)"),
                    tags$li("cnvs/ (output of cnvs_processing.R)"),
                    tags$li("output/ (output of search.R)"),
                    tags$li("output_processed/ (output of filter.R)")
                  ),
                  p("The program will automatically create output folders (db, cnvs, output, output_processed) if they do not exist.")
                )
              )
      )
    )
  )
)

# Run the application
server <- function(input, output, session) {
  
  observeEvent(input$run_all, {
    # Disable the button and show a "processing" message
    shinyjs::disable("run_all")
    shinyjs::removeClass("status_box", "status-success")
    shinyjs::removeClass("status_box", "status-error")
    shinyjs::addClass("status_box", "status-info")
    shinyjs::html("status_box", "Processing... This may take several minutes. Please wait.")
    
    # Execute the workflow
    result <- run_workflow(create_db_optional = input$create_db_opt)
    
    # Update the UI with the result
    if (result$success) {
      shinyjs::removeClass("status_box", "status-info")
      shinyjs::addClass("status_box", "status-success")
      shinyjs::html("status_box", paste("SUCCESS!\n", result$message))
    } else {
      shinyjs::removeClass("status_box", "status-info")
      shinyjs::addClass("status_box", "status-error")
      shinyjs::html("status_box", paste("ERROR:\n", result$message))
    }
    
    # Re-enable the button
    shinyjs::enable("run_all")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
