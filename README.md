# 🧬 Allele-Finder

![R](https://img.shields.io/badge/R-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/shiny-%23007BC2.svg?style=for-the-badge&logo=rstudio&logoColor=white)
![RenV](https://img.shields.io/badge/renv-%23E34F26.svg?style=for-the-badge&logo=r&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green?style=for-the-badge)

**Allele-Finder** is a robust **R-based application** designed to streamline the processing, analysis, and filtering of genetic data, specifically focusing on sample genotypes and **Copy Number Variations (CNVs)**. It provides a structured pipeline from raw input files to filtered analytical outputs, all managed through an intuitive **Shiny user interface**.

## 📋 Features

- **🔹 Sample Processing:** Prepares raw sample data, extracting and merging genotype information from `.zip` archives.
- **🔹 CNV Processing:** Consolidates and validates Copy Number Variation data from various `.csv` files.
- **🔹 Database Creation (Optional):** Generates a reference database (`db.csv`) from a specified input file, allowing on-demand regeneration.
- **🔹 Allele Search:** Performs searches against the processed sample data and the reference database to identify genotype matches.
- **🔹 Data Filtering:** Applies a sophisticated filtering logic to the search results, comparing `Hs` columns between matched data and CNV database entries.
- **🔹 User-Friendly Interface:** A **Shiny application** provides an easy-to-use graphical interface for controlling the workflow and viewing progress.

## 📂 Folder Structure

```text
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
├── db_generation_scripts/ # Scripts for future db auto-generation (In progress :D)
│   ├── 1_data_retrival.R
│   ├── 2_data_format.R
│   ├── 3_data_fill.R
│   ├── 4_data_merge.R
│   ├── 5_main_table.R
│   └── 6_genotype_table_hyperboost
├── cnvs/                # Folder where cnvs.csv is saved (output of cnvs_processing.R)
├── output/              # Folder where search files (_matches.csv) are saved
└── output_processed/    # Folder where filtered files (_filtered.csv) are saved
├── .Rprofile            # R configuration file
├── Allele-Finder.Rproj  # RStudio project file
├── renv/                # renv environment folder
│   ├── activate.R
│   └── settings.json
├── renv.lock            # renv lockfile
└── README.md            # This file
```

* **Root Directory (`.`):** Contains `app.R` and `main.R`. All other paths are relative to this root.
* **`scripts/`:** Modular R scripts for each processing step, enhancing code organization and re-usability.
* **`input_*` directories:** Designated for your raw input data files.
* **`output*` directories:** Automatically created by the program to store intermediate and final processed results.
* **`db_generation_scripts/`:** Experimental scripts for self-generating the DB implementation.

## ⚙️ Installation & Setup (Automated)

Allele-Finder utilizes **`renv`** for robust package management, ensuring a reproducible environment across different machines.

### 1. Install R and RStudio
If you don't have them, download and install the latest versions of [R](https://cran.r-project.org/) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/).

> **Note for Windows users:** If you plan to install R packages that require compilation (e.g., `curl` or `tidyverse`), you must install **RTools**. Download the appropriate version from [CRAN RTools](https://cran.r-project.org/bin/windows/Rtools/).

### 2. Clone the Repository

```bash
git clone https://github.com/santiagotariza/Allele-Finder.git
cd Allele-Finder
```

### 3. Open Project and Restore Dependencies
1. Open the `.Rproj` file (`Allele-Finder.Rproj`) in RStudio.
2. **That's it!** The project is configured to automatically install `renv` and restore all required packages upon opening.
3. Wait for the console to indicate that the environment is synchronized.

## 🚀 Usage

### 1. Prepare Input Data
* Place your `AT*.csv` file(s) for the database in **`input_db/`**.
* Place your CNV `.csv` files in **`input_cnvs/`**.
* Place your sample `.zip` files in **`samples/`**.

### 2. Run the Application
In RStudio, open `app.R` and click the **"Run App"** button, or execute:

```r
shiny::runApp()
```

### 3. Interact with the App
* The application will open in your default web browser.
* In the **"Processing"** tab, verify options (e.g., check "Regenerate Database" if needed).
* Click **"Run Complete Workflow"** to start the pipeline.
* Monitor the **"Processing Status"** box for real-time updates.

## 📄 Output Files

Upon successful execution, the following files will be generated:

| Directory | File Example | Description |
| :--- | :--- | :--- |
| **samples/** | `XMV33_Genotype_Matrix_short.csv` | Processed sample files. |
| **cnvs/** | `cnvs.csv` | Consolidated CNV database. |
| **db/** | `db.csv` | Main genetic reference database. |
| **output/** | `..._matches.csv` | Search result files. |
| **output_processed/** | `..._filtered.csv` | Final filtered output files. |

## 🛠️ Troubleshooting

* **"ERROR: Folder '...' does not exist"**: Ensure all input folders (`input_db`, `input_cnvs`, `samples`) exist. Output folders are created automatically.
* **`renv` Issues**: If dependencies fail to load, try running `renv::restore()` manually in the R console.
* **File Format Issues**: Verify that input `.csv` files match the expected structure required by `create_db.R` and `cnvs_processing.R`.
* **"Hs" Column Discrepancies**: Ensure `Hs` column names (e.g., `Hs04083572`) match between matched data and `cnvs.csv`. The script handles `_cn` suffixes automatically.

---
&copy; 2025 Santiago T. Ariza - For issues or suggestions, please open an issue on this repository.
