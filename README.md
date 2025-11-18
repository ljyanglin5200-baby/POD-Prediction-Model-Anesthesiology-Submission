# POD-Prediction-Model-Anesthesiology-Submission
# Reproducibility Materials for "Development and Validation of a Prediction Model for De novo Postoperative Delirium in Adult Cardiac Surgery Patients Free of Preoperative Cognitive Impairment: A Prospective, Dual-center Cohort Study"

This repository provides the full dataset and R analysis code required to reproduce the findings of our study, submitted for publication in *Anesthesiology*.

We are committed to the highest standards of scientific transparency and reproducibility. The code is organized into logical scripts, and this guide provides a step-by-step walkthrough of the entire analytical workflow.

**Note:** The official supplementary materials, including the TRIPOD checklist (Appendix 1), supplementary tables (Appendix 2), and supplementary figures (Appendix 3), have been submitted directly to the journal as separate appendix files and should be consulted alongside the materials in this repository.

---

### **1. File Manifest**

This repository contains the following core files:

*   **`my_data.csv`**: The raw, anonymized dataset (N=384) before any data cleaning or preprocessing steps.
*   **`my_data_cleaned_for_analysis.csv`**: The final, cleaned, and anonymized analytical cohort (N=308) used directly by the R script to generate all results.
*   **`Delirium_Analysis_Script.R`**: The main R script that performs all data processing, statistical modeling, validation, and generation of manuscript results.
*   **`app.R`**: The R script for the interactive Shiny web application (the online risk calculator).
*   **`README.md`**: This instruction file.

---

### **2. Software and Package Requirements**

The analysis was performed using **R version 4.4.1**. The following R packages are required to execute the scripts. The analysis script includes commands to install these packages if they are not already present.

#### Core Packages for Analysis:
*   `tidyverse` (v2.0.0): For data manipulation, exploration, and visualization (includes `dplyr`, `ggplot2`, `readr`, etc.).
*   `brms` (v2.23.0): For Bayesian multilevel modeling.
*   `mice` (v3.18.0): For multiple imputation of missing data.
*   `gtsummary` (v2.4.0): For creating publication-ready summary tables.
*   `tableone` (v0.13.2): Also used for creating descriptive statistics tables.
*   `rstatix` (v0.7.2): For basic statistical tests.
*   `naniar` (v1.1.0): For visualizing missing data patterns.
*   `future` (v1.34.0): For enabling parallel processing during model fitting.

#### Package for the Web Application:
*   `shiny`: For running the interactive risk calculator (`app.R`).

---

### **3. Step-by-Step Execution Guide**

To reproduce the entire analysis, please follow these steps:

1.  **Download the Repository:** Download all files from this repository (e.g., via "Code" -> "Download ZIP") and place them in a single project folder on your local machine.

2.  **Open the R Script:** Open the `Delirium_Analysis_Script.R` file in RStudio.

3.  **Install Packages:** The script is designed to be self-contained. If you run it from top to bottom, it will prompt the installation of any missing R packages.

4.  **Execute the Script:** Run the entire `Delirium_Analysis_Script.R` script sequentially. The script is heavily commented and organized into logical sections corresponding to the analyses in the manuscript:
    *   **Data Preparation:** Creates the final analytical cohort and generates Table 1.
    *   **Missing Data Imputation:** Performs multiple imputation using MICE.
    *   **Model Fitting:** Details the iterative process of building and confirming the final, robust 4-predictor Bayesian model.
    *   **Results Generation:** Generates the primary results (Table 2, Figure 2) and model diagnostics (Supplementary Figure S1).
    *   **Landmark Analysis:** Creates the landmark cohort and fits the dynamic update model (Table 4).
    *   **Validation Trilogy:** Performs the internal, external, and reverse external validations (Figures 3A, 3B, 3C).
    *   **Sensitivity Analyses:** Conducts the pre-specified analyses for propofol dose rate and the age-propofol interaction (Supplementary Tables S6-S7, Supplementary Figure S3).

5.  **Run the Web Application (Optional):** To launch the online risk calculator locally, open the `app.R` file in RStudio and click the "Run App" button in the top-right corner of the editor pane.
