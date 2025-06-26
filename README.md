# **Allele-Finder**

## **Genetic Data Processing and Analysis Tool**

Allele-Finder is a robust R-based application designed to streamline the processing, analysis, and filtering of genetic data, specifically focusing on sample genotypes and Copy Number Variations (CNVs). It provides a structured pipeline from raw input files to filtered analytical outputs, all managed through an intuitive Shiny user interface.

## **Features**

* **Sample Processing**: Prepares raw sample data, extracting and merging genotype information from .zip archives.
* **CNV Processing**: Consolidates and validates Copy Number Variation data from various .csv files.
* **Database Creation (Optional)**: Generates a reference database (db.csv) from a specified input file, allowing on-demand regeneration.
* **Allele Search**: Performs searches against the processed sample data and the reference database to identify genotype matches.
* **Data Filtering**: Applies a sophisticated filtering logic to the search results, comparing Hs columns between matched data and CNV database entries.
* **User-Friendly Interface**: A Shiny application provides an easy-to-use graphical interface for controlling the workflow and viewing progress.

## **Folder Structure**
```bash
.
├── app.R                # The Shiny application (user interface)
├── main.R               # Main script that orchestrates the execution flow
├── scripts/             # Folder for individual processing functions
│   ├── samples_processing.R
│   ├── cnvs_processing.R
│   ├── create_db.R
│   ├── search.R
│   └── filter.R
├── input_db/            # Folder for the database input file (e.g., AT*.csv)
├── input_cnvs/          # Folder for CNVs input files (e.g., .csv files)
├── samples/             # Folder for sample .zip files (and where processed sample files are saved)
├── db/                  # Folder where db.csv is saved (output of create_db.R)
├── cnvs/                # Folder where cnvs.csv is saved (output of cnvs_processing.R)
├── output/              # Folder where search files (_matches.csv) are saved
└── output_processed/    # Folder where filtered files (_filtered.csv) are saved
├── .Rprofile            # R configuration file
├── Allele-Finder.Rproj  # RStudio project file
├── renv/                # renv environment folder
│   ├── activate.R
│   ├── settings.json
├── renv.lock            # renv lockfile
└── README.md            # This file
```

* **Root Directory (.):** This is the main project folder where app.R and main.R should be located. All other paths are relative to this root.
* **scripts/:** Contains the modular R scripts for each processing step, enhancing code organization and reusability.
* **input_* directories:** Designated for your raw input data files.
* **output* directories:** Automatically created by the program to store intermediate and final processed results.

## **Installation & Setup (Automated)**

Allele-Finder utilizes `renv` for robust package management, ensuring a reproducible environment across different machines.

1.  **Install R and RStudio:** If you don't have them, download and install the latest versions of [R](https://cran.r-project.org/) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/) for your operating system.
2.  **Clone the Repository:** Clone this GitHub repository to your local machine:
    ```bash
    git clone https://github.com/santiagotariza/Allele-Finder/Allele-Finder.git
    ```
    ```bash
    cd Allele-Finder
    ```
3.  **Open Project and Restore Dependencies:**
    * Open the `.Rproj` file (`Allele-Finder.Rproj`) in RStudio.
    * **That's all for dependency setup!** The project is configured to automatically install `renv` (if needed) and restore all required packages when you open the project. You will see messages in the RStudio console indicating the restoration progress.
    * Once the console process indicates that the environment is synchronized, you are ready to use the application.

## **Usage**

1.  **Prepare Input Data:**
    * Place your `AT*.csv` file(s) for the database in the `input_db/` folder.
    * Place your CNV `.csv` files in the `input_cnvs/` folder.
    * Place your sample `.zip` files in the `samples/` folder.
2.  **Run the AlleleFinder Application:**
    * In RStudio, open the `app.R` file.
    * Click the "Run App" button in RStudio, or execute the following command in the R console:
        ```R
        shiny::runApp()
        ```
3.  **Interact with the App:**
    * The AlleleFinder application will open in your default web browser.
    * In the "Processing" tab, you can check the "Regenerate Database (create_db.R)" box if you want to rebuild `db.csv` from scratch.
    * Click the "Run Complete Workflow" button to start the data processing pipeline.
    * Monitor the "Processing Status" box for real-time updates and success/error messages.
    * The "Folder Configuration" tab provides a quick reference to the expected directory structure.

## **Output Files**

Upon successful execution, the following output files will be generated in their respective directories:

* **samples/**: Processed sample files (e.g., `XMV33_Genotype_Matrix_short.csv`)
* **cnvs/cnvs.csv**: The consolidated CNV database.
* **db/db.csv**: The main genetic reference database.
* **output/**: Search result files (e.g., `XMV33_Genotype_Matrix_short_matches.csv`).
* **output_processed/**: Filtered output files (e.g., `XMV33_Genotype_Matrix_short_filtered.csv`).

## **Troubleshooting**

* **"ERROR: Folder '...' does not exist."**: Ensure all input folders (`input_db`, `input_cnvs`, `samples`) exist and contain the necessary files. The output folders (`db`, `cnvs`, `output`, `output_processed`) will be created automatically.
* **R Packages / `renv` Issues**: If the application fails to start or throws errors about missing functions despite the automated restoration process appearing complete, try running `renv::restore()` manually in the R console. If `renv` could not be installed automatically, follow the message in the console to install it manually (`install.packages("renv")`) and then reopen the project.
* **File Format Issues**: The scripts rely on specific CSV formats. If errors persist, verify that your input `.csv` files (especially `AT*.csv` and CNV files) match the expected structure as processed by `create_db.R` and `cnvs_processing.R`.
* **"Hs" Column Discrepancies**: If the filtering step gives unexpected results, double-check that the Hs column names (e.g., `Hs04083572`, `Hs00010001110_14`) in both your `_matches.csv` files and `cnvs.csv` are as expected. The script now handles `_cn` suffixes in `cnvs_db`.

**Allele-Finder** is designed to simplify your genetic data analysis workflow. For any issues or suggestions, please open an issue on this repository.
