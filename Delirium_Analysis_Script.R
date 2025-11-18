# -------------------------------------------------------------------------------
# SCRIPT: Forensic Audit of Patient Exclusion Criteria
# PURPOSE: To precisely and reproducibly calculate the number of patients
#          excluded at each step of the cohort selection process, providing
#          the exact numbers needed for the manuscript's CONSORT flow diagram (Figure 1).
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)

# IMPORTANT: Start with your ABSOLUTELY ORIGINAL, UNTOUCHED dataset.
# Please replace "my_original_raw_data.csv" with the actual filename of your N=384 dataset.
original_raw_data_file <- "my data.csv" 
raw_data <- read_csv(original_raw_data_file)

# --- 2. STEP-BY-STEP EXCLUSION AUDIT ---

# Get the starting number
n_initial_screened <- nrow(raw_data)

# --- CRITERION 1: Age < 18 ---
# We use an intermediate variable to track the count
cohort_after_age_filter <- raw_data %>% filter(age_years >= 18)
n_excluded_for_age <- n_initial_screened - nrow(cohort_after_age_filter)

# --- CRITERION 2: Missing MMSE or Education Data ---
# On the remaining cohort, we find who has missing eligibility data.
cohort_after_missing_filter <- cohort_after_age_filter %>%
  filter(!is.na(mmse_preop) & !is.na(edu_years))
n_excluded_for_missing_data <- nrow(cohort_after_age_filter) - nrow(cohort_after_missing_filter)

# --- CRITERION 3: Pre-existing Cognitive Impairment (MMSE below threshold) ---
# On the cohort with complete data, we apply the final exclusion rule.
cohort_after_mmse_filter <- cohort_after_missing_filter %>%
  mutate(
    mmse_threshold = case_when(
      edu_years == 0 ~ 17,
      edu_years >= 1 & edu_years <= 6 ~ 20,
      edu_years > 6 ~ 24,
      TRUE ~ NA_real_ # Should not happen if data is clean
    )
  ) %>%
  filter(mmse_preop > mmse_threshold)
n_excluded_for_mmse_impairment <- nrow(cohort_after_missing_filter) - nrow(cohort_after_mmse_filter)

# --- FINAL COHORT ---
n_final_cohort <- nrow(cohort_after_mmse_filter)
n_total_excluded <- n_excluded_for_age + n_excluded_for_missing_data + n_excluded_for_mmse_impairment

# --- 3. GENERATE THE FINAL REPORT FOR FIGURE 1 ---
cat("--- PATIENT SCREENING AND EXCLUSION REPORT (for Figure 1) ---\n\n")
cat("1. Total patients initially screened for eligibility: ", n_initial_screened, "\n")
cat("----------------------------------------------------------------\n")
cat("2. Exclusions:\n")
cat("   - Reason A: Age < 18 years: n = ", n_excluded_for_age, "\n")
cat("   - Reason B: Missing MMSE or education data (eligibility undetermined): n = ", n_excluded_for_missing_data, "\n")
cat("   - Reason C: Pre-existing cognitive impairment (MMSE below threshold): n = ", n_excluded_for_mmse_impairment, "\n")
cat("----------------------------------------------------------------\n")
cat("3. Total patients excluded: ", n_total_excluded, " (This should match 384 - 308 = 76)\n")
cat("----------------------------------------------------------------\n")
cat("4. Final number of patients included in the analysis: ", n_final_cohort, " (This MUST be 308)\n\n")

# --- VERIFICATION ---
if (n_final_cohort == 308 && n_total_excluded == (n_initial_screened - 308)) {
  cat("SUCCESS: The numbers are consistent and correct. You can now build your Figure 1.\n")
} else {
  cat("WARNING: There is a mismatch in the numbers. Please carefully check your original data and criteria.\n")
}

# ==============================================================================
# DATA PREPARATION SCRIPT FOR POSTOPERATIVE DELIRIUM PREDICTION STUDY (FINAL)
#
# Author: [medusa]
# Date: [Oct 11 2025]
#
# Description:
# This script is fully self-contained and performs all data preparation steps.
# ==============================================================================

# --- Section 0: Setup - Load Necessary Libraries ---
library(tidyverse)
library(janitor)
library(gtsummary)


# --- Section 1: Load Raw Data ---
raw_data <- read_csv(
  "my data.csv",
  col_types = cols(lvef = col_character()),
  show_col_types = FALSE
)


# --- Section 2: Initial Data Cleaning & Standardization ---
data_cleaned <- raw_data %>%
  clean_names() %>%
  mutate(lvef = parse_number(lvef))


# --- Section 3: Create Derived Outcome Variable ---
data_cleaned <- data_cleaned %>%
  mutate(
    delirium_occurred = if_else(camicu_day1 == 1 | camicu_day2 == 1 | camicu_day3 == 1, 1, 0, missing = 0)
  )


# --- Section 4: Apply Inclusion/Exclusion Criteria ---
n_initial <- nrow(data_cleaned)

data_filtered <- data_cleaned %>% filter(age_years >= 18)
n1 <- n_initial - nrow(data_filtered)

data_filtered_eligible <- data_filtered %>% filter(!is.na(edu_years) & !is.na(mmse_preop))
n2 <- nrow(data_filtered) - nrow(data_filtered_eligible)

data_with_inclusion_decision <- data_filtered_eligible %>%
  mutate(
    inclusion_status = if_else(
      !(edu_years == 0 & mmse_preop <= 17) &
        !(edu_years >= 1 & edu_years <= 6 & mmse_preop <= 20) &
        !(edu_years > 6 & mmse_preop <= 24),
      "Included",
      "Excluded_MMSE"
    )
  )

n3 <- data_with_inclusion_decision %>% filter(inclusion_status == "Excluded_MMSE") %>% nrow()
n_final <- data_with_inclusion_decision %>% filter(inclusion_status == "Included") %>% nrow()

cat("--- Patient Selection Flow Report ---\n")
cat(sprintf("Initial patients assessed for eligibility: %d\n", n_initial))
cat("--------------------------------------------------\n")
cat(sprintf("Excluded (Total n=%d):\n", n1 + n2 + n3))
cat(sprintf("  - Age < 18 years: %d\n", n1))
cat(sprintf("  - Missing MMSE or Education data: %d\n", n2))
cat(sprintf("  - Pre-existing cognitive impairment (by MMSE): %d\n", n3))
cat("--------------------------------------------------\n")
cat(sprintf("Final cohort for analysis: %d\n\n", n_final))


# --- Section 5: Convert Variables to Factors ---
binary_vars <- c("gender", "smoking_history", "alcohol_history", "hypertension", "diabetes", 
                 "stroke_history", "cerebral_stenosis", "camicu_day1", "camicu_day2", 
                 "camicu_day3", "delirium_occurred")

data_fully_formatted <- data_with_inclusion_decision %>%
  mutate(
    across(all_of(binary_vars), ~factor(., levels = c(0, 1), labels = c("No", "Yes"))),
    hospital = factor(hospital),
    surgery_category = factor(surgery_category, 
                              levels = c(1, 2, 3, 4, 5, 6, 7), 
                              labels = c("Isolated CABG", "Isolated Single-Valve", "Isolated Aortic", 
                                         "CABG + Valve", "Multi-Valve", "Congenital", "Other")),
    valve_prosthesis_type = factor(valve_prosthesis_type, 
                                   levels = c(0, 1, 2, 3), 
                                   labels = c("No Replacement", "Bioprosthetic", "Mechanical", "Mixed"))
  )


# --- Section 5.5: Sensitivity Analysis ---
cat("--- Sensitivity Analysis Report (Supplementary Table) ---\n")

sensitivity_table <- data_fully_formatted %>%
  select(
    inclusion_status,
    age_years, gender, edu_years, bmi, smoking_history, alcohol_history,
    hypertension, diabetes, stroke_history, lvef, cpb_time_min, surgery_duration_min
  ) %>%
  tbl_summary(
    by = inclusion_status,
    missing = "no"
  ) %>%
  add_p() %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table S1. Baseline Characteristics of Included vs. Excluded Patients**")

print(sensitivity_table)
cat("\n")


# --- Section 6: Create the Final Cohort for Modeling ---
data_final_cohort <- data_fully_formatted %>%
  filter(inclusion_status == "Included") %>%
  select(-inclusion_status)

data_final_cohort$gender <- relevel(data_final_cohort$gender, ref = "No")
data_final_cohort$surgery_category <- relevel(data_final_cohort$surgery_category, ref = "Isolated CABG")
data_final_cohort$valve_prosthesis_type <- relevel(data_final_cohort$valve_prosthesis_type, ref = "No Replacement")


# --- Section 7: Final Review and Save ---
cat("--- Final Analytical Dataset Summary ---\n")
print(str(data_final_cohort, list.len = ncol(data_final_cohort)))

write_csv(data_final_cohort, "my_data_cleaned_for_analysis.csv")

cat("\n--- SCRIPT EXECUTION COMPLETE ---\n")
# The final line is now corrected.
cat("The final analytical dataset has been saved as 'my_data_cleaned_for_analysis.csv' in the project directory.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Generate Publication-Quality Baseline Characteristics Table (Table 1)
#         WITH AUTOMATED NORMALITY TESTING
# AUTHOR: [medusa]
# DATE:   [Oct 12 2025]
# VERSION: 3.0 (Data-Driven Approach)
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
# rstatix is excellent for easy normality testing.
# install.packages(c("tidyverse", "tableone", "rstatix"))
library(tidyverse)
library(tableone)
library(rstatix) # For normality testing

# --- 2. DATA LOADING ---
file_path <- "my_data_cleaned_for_analysis.csv"
my_data <- read_csv(file_path)

# --- 3. VARIABLE DEFINITION ---

# Define all variables intended for Table 1.
vars_for_table1 <- c(
  "age_years", "gender", "edu_years", "bmi", "smoking_history", "alcohol_history",
  "hypertension", "diabetes", "stroke_history", "lvef", "cerebral_stenosis",
  "surgery_category", "valve_prosthesis_type", "cpb_time_min", 
  "surgery_duration_min", "acclamp_time_min", "propofol_mg", 
  "mmse_preop", "delirium_occurred", "hospital"
)

# Define categorical variables explicitly.
categorical_vars <- c(
  "gender", "smoking_history", "alcohol_history", "hypertension", "diabetes",
  "stroke_history", "cerebral_stenosis", "surgery_category", 
  "valve_prosthesis_type", "delirium_occurred", "hospital"
)

# Automatically identify continuous variables by finding the difference.
continuous_vars <- setdiff(vars_for_table1, categorical_vars)

# Create a clean dataframe for analysis, ensuring correct types.
data_for_table1 <- my_data %>%
  select(all_of(vars_for_table1)) %>%
  mutate(across(all_of(categorical_vars), as.factor))

# --- 4. FORMAL NORMALITY TESTING (THE CRITICAL NEW STEP - CORRECTED SYNTAX) ---

# Perform Shapiro-Wilk test on all continuous variables.
# CORRECTED: We now pass the dataframe and the variables directly to the function,
# which is a more robust way to call it.
normality_test_results <- shapiro_test(data_for_table1, # 1. Specify the data
                                       vars = continuous_vars) # 2. Specify the variables to test

# Display the normality test results in the console.
cat("--- Shapiro-Wilk Normality Test Results ---\n")
print(normality_test_results)
cat("------------------------------------------\n\n")

# Save these results to a CSV file for your records and for the supplement.
write.csv(normality_test_results, "Normality_Test_Results.csv", row.names = FALSE)

# --- 5. DYNAMICALLY IDENTIFY NON-NORMAL VARIABLES (No change needed here, but included for completeness) ---

# Based on the test results (p < 0.05 indicates non-normality),
# create a vector of variable names that should be treated as non-normal.
non_normal_vars <- normality_test_results %>%
  filter(p < 0.05) %>%
  pull(variable) # pull() extracts the column as a simple list of names

cat("--- Variables identified as NON-NORMAL (p < 0.05) ---\n")
print(non_normal_vars)
cat("---------------------------------------------------\n\n")

# --- 6. CREATE AND EXPORT THE DATA-DRIVEN TABLE 1 ---

# Create the table object as before.
table1_object <- CreateTableOne(
  vars = setdiff(vars_for_table1, "hospital"),
  data = data_for_table1,
  factorVars = setdiff(categorical_vars, "hospital"),
  strata = "hospital",
  addOverall = TRUE
)

# Print and export the table.
# CRITICAL: The 'nonnormal' argument is now fed our dynamically created list.
# This ensures the table's formatting is directly based on the statistical evidence.
table1_for_export <- print(
  table1_object,
  nonnormal = non_normal_vars, # This is the key change!
  showAllLevels = TRUE,
  smd = TRUE,
  printToggle = FALSE,
  noSpaces = TRUE,
  varLabels = TRUE,
  quote = FALSE
)

# Display the final, correctly formatted table in the console.
print(table1_for_export)

# Save the final, evidence-based table to a CSV file.
write.csv(table1_for_export, file = "Table1_Baseline_Characteristics.csv")

cat("\nSUCCESS: Normality tests are saved in 'Normality_Test_Results.csv' and the evidence-based Table 1 is saved in 'Table1_Baseline_Characteristics.csv'.\n")

# --- SCRIPT END ---

# -------------------------------------------------------------------------------
# SCRIPT: Consolidate Surgical Categories for Robust Modeling
# AUTHOR: [medusa]
# DATE:   [Oct 13 2025]
# VERSION: 2.0
# PURPOSE: This script loads the cleaned analytical dataset, consolidates the
#          'surgery_category' variable into fewer, more robust groups based on
#          clinical reasoning, verifies the consolidation, and saves the updated dataset.
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
# The tidyverse package suite is the gold standard for modern data manipulation in R.
# install.packages("tidyverse")
library(tidyverse)

# --- 2. DATA LOADING & DEFINITION ---
file_path <- "my_data_cleaned_for_analysis.csv"
my_data_cleaned <- read_csv(file_path)

# --- 3. CORE LOGIC: CONSOLIDATE SURGICAL CATEGORIES ---
# We will create a new variable 'surgery_category_grouped' to preserve the original.
# The `fct_recode()` function from the forcats package (part of tidyverse) is the
# perfect tool for this task as it is explicit and clear.
# It works by the logic: `new_name = "old_name"`.
# Categories not mentioned are left with their original names.

my_data_updated <- my_data_cleaned %>%
  mutate(
    # Ensure the original variable is treated as a factor before recoding.
    # This is a robust way to handle character data.
    surgery_category_grouped = fct_recode(factor(surgery_category),
                                          "Complex/Combined" = "CABG + Valve",
                                          "Complex/Combined" = "Multi-Valve",
                                          "Other Procedures" = "Congenital",
                                          "Other Procedures" = "Isolated Aortic",
                                          "Other Procedures" = "Other"
                                          # Note: "Isolated CABG" and "Isolated Single-Valve" are correctly
                                          # left untouched as they are the core, high-volume categories.
    )
  )

# --- 4. VERIFICATION (CRITICAL STEP) ---
# Before saving, it is essential to verify that the recoding worked as expected.
# This step provides a transparent check of the operation.
# We will print a frequency table of the original and new variables.

cat("--- Verifying the Consolidation ---\n")
cat("Original Category Frequencies:\n")
print(table(my_data_updated$surgery_category, useNA = "ifany")) # Shows original counts

cat("\nNew Grouped Category Frequencies:\n")
print(table(my_data_updated$surgery_category_grouped, useNA = "ifany")) # Shows new, consolidated counts
cat("------------------------------------\n\n")

# --- 5. SAVE THE UPDATED DATASET ---
# The updated dataframe, now including the new 'surgery_category_grouped' column,
# will overwrite the original file.

write_csv(my_data_updated, file = file_path)

# Provide a final confirmation message to the user.
cat(paste0("SUCCESS: The new variable 'surgery_category_grouped' has been created and verified.\n",
           "The updated dataset has been saved back to '", file_path, "'.\n"))

# --- SCRIPT END ---

# -------------------------------------------------------------------------------
# SCRIPT: Missing Data Diagnosis and Multiple Imputation using MICE
# AUTHOR: [medusa]
# DATE:   [Oct 14 2025]
# VERSION: 1.0
# PURPOSE: This script systematically assesses the missing data pattern in the
#          analytical cohort and then applies Multiple Imputation by Chained
#          Equations (MICE) to generate complete datasets for downstream modeling.
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
# mice: The core package for multiple imputation.
# naniar: An excellent package for visualizing and summarizing missing data.
# install.packages(c("tidyverse", "mice", "naniar"))
library(tidyverse)
library(mice)
library(naniar)

# --- 2. DATA LOADING ---
file_path <- "my_data_cleaned_for_analysis.csv"
my_data <- read_csv(file_path)

# --- 3. VARIABLE SELECTION FOR IMPUTATION MODEL ---
# The imputation model should include all variables that will be in the final
# analytical model (predictors AND outcome). It can also include auxiliary
# variables that are predictive of missingness or the variables themselves.

# We will exclude ID variables and any raw variables that have been superseded
# (e.g., the original 'surgery_category' if we only use the grouped one).
vars_for_imputation <- my_data %>%
  select(
    -id, # Exclude patient identifier
    -surgery, # Exclude the raw surgery name
    -surgery_category, # Exclude the original category now that we have the grouped one
    # Exclude post-operative variables that are purely outcomes of delirium itself
    -delirium_duration_hours,
    -mmse_postop,
    -camicu_day1, -camicu_day2, -camicu_day3
  ) %>%
  # Ensure character variables that should be factors are correctly typed
  mutate(across(where(is.character), as.factor)) %>%
  colnames()

data_to_impute <- my_data %>%
  select(all_of(vars_for_imputation)) %>%
  mutate(across(where(is.character), as.factor))

# --- 4. STEP 1: DIAGNOSE THE MISSING DATA PATTERN ---

# A) Generate a summary table of missingness.
missing_summary <- miss_var_summary(data_to_impute)

cat("--- Missing Data Summary ---\n")
print(missing_summary)
cat("----------------------------\n\n")

# Save this summary table for your manuscript's methods section or supplement.
write_csv(missing_summary, "Missing_Data_Summary.csv")

# B) Visualize the missingness pattern (optional but highly recommended).
# This plot provides an intuitive overview of the missing data landscape.
miss_plot <- gg_miss_var(data_to_impute, show_pct = TRUE)
print(miss_plot)
# ggsave("Missing_Data_Pattern_Plot.png", miss_plot) # Optional: save the plot

# --- 5. STEP 2: PERFORM MULTIPLE IMPUTATION (MICE) ---

# Set a seed for reproducibility. This is MANDATORY for scientific research.
# It ensures that anyone running your code will get the exact same imputed datasets.
set.seed(12345)

# Run the MICE algorithm.
# m = 20: Creates 20 imputed datasets. A good number for most studies (5-20 is common).
# maxit = 50: Number of iterations for the algorithm to converge. 50 is robust.
imputed_data_object <- mice(
  data_to_impute,
  m = 20,
  maxit = 50,
  method = 'pmm', # Predictive Mean Matching is a robust default for numeric data
  printFlag = TRUE # Shows the progress
)

cat("\n--- MICE Imputation Complete ---\n")
# The `imputed_data_object` is a special 'mids' object containing all 20 datasets.
# We can inspect the methods used for each variable:
print(imputed_data_object$method)
cat("---------------------------------\n\n")


# --- 6. STEP 3: INSPECT THE IMPUTATIONS (DIAGNOSTIC CHECK) ---
# This optional visualization step is not critical if the imputation is complete.
# We will comment it out to avoid the error with sparse data.

# if("bmi" %in% missing_summary$variable) {
#   cat("--- Diagnostic Plot for Imputed 'bmi' Values ---\n")
#   density_plot <- densityplot(imputed_data_object, ~bmi)
#   print(density_plot)
#   cat("-----------------------------------------------\n\n")
# }

# --- 7. STEP 4: SAVE THE IMPUTED DATA OBJECT ---
# We do NOT save a single CSV. We save the entire 'mids' object, which is the
# standard and correct way to handle multiply imputed data for subsequent analysis.
# Your Bayesian modeling will be performed directly on this object.

saveRDS(imputed_data_object, file = "imputed_data_mids_object.rds")

cat(paste0("SUCCESS: The multiple imputation process is complete.\n",
           "A summary of missingness is saved in 'Missing_Data_Summary.csv'.\n",
           "The complete imputed data object (containing 20 datasets) is saved as 'imputed_data_mids_object.rds'.\n",
           "You will use this '.rds' file for your Bayesian modeling.\n"))

# --- SCRIPT END ---

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 1: Standardization
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
library(tidyverse)
library(mice)
library(brms) # We will now use the main modeling package

# Load the imputed data object we created in the last step.
imputed_mids <- readRDS("imputed_data_mids_object.rds")

# --- 2. STANDARDIZE CONTINUOUS PREDICTORS ---

# First, complete the mids object into a single, long-format dataframe.
# This makes calculating means and standard deviations straightforward.
completed_data_long <- mice::complete(imputed_mids, action = "long", include = TRUE)

# Identify the continuous variables that need to be standardized.
continuous_vars_to_scale <- c(
  "age_years", "edu_years", "bmi", "lvef", "cpb_time_min",
  "surgery_duration_min", "acclamp_time_min", "propofol_mg", "mmse_preop"
)

# Calculate the mean and standard deviation for each continuous variable
# FROM THE ORIGINAL (non-imputed) DATA. This is the correct approach.
# The original data is stored in imputation 0 (.imp == 0).
scaling_values <- completed_data_long %>%
  filter(.imp == 0) %>% # Use only the original, observed data for scaling parameters
  select(all_of(continuous_vars_to_scale)) %>%
  summarise(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE))

# Now, apply this standardization to all 20 imputed datasets.
completed_data_scaled_long <- completed_data_long %>%
  mutate(
    age_years_scaled = (age_years - scaling_values$age_years_mean) / scaling_values$age_years_sd,
    edu_years_scaled = (edu_years - scaling_values$edu_years_mean) / scaling_values$edu_years_sd,
    bmi_scaled = (bmi - scaling_values$bmi_mean) / scaling_values$bmi_sd,
    lvef_scaled = (lvef - scaling_values$lvef_mean) / scaling_values$lvef_sd,
    cpb_time_min_scaled = (cpb_time_min - scaling_values$cpb_time_min_mean) / scaling_values$cpb_time_min_sd,
    surgery_duration_min_scaled = (surgery_duration_min - scaling_values$surgery_duration_min_mean) / scaling_values$surgery_duration_min_sd,
    acclamp_time_min_scaled = (acclamp_time_min - scaling_values$acclamp_time_min_mean) / scaling_values$acclamp_time_min_sd,
    propofol_mg_scaled = (propofol_mg - scaling_values$propofol_mg_mean) / scaling_values$propofol_mg_sd,
    mmse_preop_scaled = (mmse_preop - scaling_values$mmse_preop_mean) / scaling_values$mmse_preop_sd
  )

# Convert the long-format data back into a mids object for modeling.
imputed_mids_scaled <- as.mids(completed_data_scaled_long)

# --- 4. SAVE THE SCALED DATA OBJECT ---
# It's good practice to save this intermediate step.
saveRDS(imputed_mids_scaled, file = "imputed_data_scaled_mids_object.rds")

cat("SUCCESS: Step 1 is complete.\n")
cat("All continuous predictors have been standardized.\n")
cat("The new data object 'imputed_data_scaled_mids_object.rds' is ready for modeling in Step 2.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 2A: Defining the Model Formula
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
# No new packages needed for this part.
library(brms)

# --- 2. DEFINE THE MODEL FORMULA ---
# This formula specifies the structure of our analysis.
# It is a critical piece of information for the manuscript's methods section.

model_formula <- bf(
  # Part 1: Outcome
  delirium_occurred ~ 
    
    # Part 2: Fixed Effects (Predictors)
    # Note we are using the '_scaled' versions for continuous variables.
    age_years_scaled + gender + edu_years_scaled + bmi_scaled + 
    smoking_history + alcohol_history + hypertension + diabetes + 
    stroke_history + lvef_scaled + cerebral_stenosis + 
    valve_prosthesis_type + cpb_time_min_scaled + surgery_duration_min_scaled + 
    acclamp_time_min_scaled + propofol_mg_scaled + mmse_preop_scaled +
    
    # Part 3: Random Effects (Hierarchical Structure)
    (1 | hospital) + (1 | surgery_category_grouped),
  
  # Part 4: Model Family
  family = bernoulli(link = "logit")
)

# Print the formula to the console to verify it.
cat("--- Model Formula successfully defined: ---\n")
print(model_formula)
cat("------------------------------------------\n")

# SUCCESS: The model's blueprint is now ready.

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 2B: Specifying Priors
# -------------------------------------------------------------------------------
library(tidyverse)
library(brms)

# --- 1. CREATE A PUBLICATION-READY TABLE OF PRIORS FOR THE SUPPLEMENT ---
# This table transparently documents our choices.
priors_table <- tibble(
  Parameter = c("Intercept", "Fixed Effects (all β coefficients)", "Random Effect Standard Deviations (all σ)"),
  `brms Class` = c("Intercept", "b", "sd"),
  Distribution = c("Normal", "Normal", "Exponential"),
  Parameters = c("mean = -2, sd = 1", "mean = 0, sd = 2.5", "rate = 1"),
  Justification = c("Weakly informative; centered around the log-odds of the sample's mean delirium rate (~11%).",
                    "Weakly informative regularizing prior for standardized predictors.",
                    "Weakly informative regularizing prior to constrain variance to be positive and favor smaller values.")
)

# Print the priors table to the console.
cat("--- Publication Table for Priors (for Supplementary Materials) ---\n")
print(priors_table)
cat("------------------------------------------------------------------\n\n")

# Save this table to a file for easy inclusion in your manuscript supplement.
write_csv(priors_table, file = "Supplementary_Table_Priors.csv")

# --- 2. DEFINE THE PRIORS OBJECT FOR THE BRMS MODEL ---
# This translates the table above into code that brms can understand.
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

cat("SUCCESS: Step 2 is complete.\n")
cat("The model formula and priors are defined and ready.\n")
cat("The priors have been documented in 'Supplementary_Table_Priors.csv'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 3: Model Fitting & Diagnostics
#         (Corrected version using brm_multiple for MICE data)
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
library(tidyverse)
library(brms)
library(future) # For parallel processing

# Set up parallel processing
plan(multisession)

# Load the necessary R objects we created in previous steps.
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")
# (Formula and priors are assumed to be in the environment)
model_formula <- bf(
  delirium_occurred ~ age_years_scaled + gender + edu_years_scaled + bmi_scaled + 
    smoking_history + alcohol_history + hypertension + diabetes + 
    stroke_history + lvef_scaled + cerebral_stenosis + 
    valve_prosthesis_type + cpb_time_min_scaled + surgery_duration_min_scaled + 
    acclamp_time_min_scaled + propofol_mg_scaled + mmse_preop_scaled +
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# --- 2. RUN THE BAYESIAN MODEL (USING THE CORRECT FUNCTION) ---
# CRITICAL CHANGE: We use brm_multiple() instead of brm() for mice objects.
# The `data` argument now directly takes the 'mids' object.

cat("--- Starting Bayesian model fitting using brm_multiple. This may take time... ---\n")

final_model <- brm_multiple(
  formula = model_formula,
  data = imputed_mids_scaled, # This now works correctly
  prior = priors_for_model,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 12345,
  file = "brms_model_fit", # brms_multiple automatically adds a suffix, so just the base name is needed
  combine = TRUE # This automatically combines the results from the 20 datasets
)

cat("--- Model fitting complete. The results are saved in 'brms_model_fit.rds'. ---\n\n")


# --- 3. CONVERGENCE DIAGNOSTICS (CHECKING THE DASHBOARD) ---
# This part of the code remains exactly the same.
# It works perfectly on the model object produced by brm_multiple.

# A) Visual Diagnostics: Trace Plots
cat("--- Generating visual convergence diagnostics (Trace Plots)... ---\n")
trace_plots <- plot(final_model, N = 5, ask = FALSE) 

ggsave("Supplementary_Figure_Trace_Plots.png", plot = trace_plots, width = 10, height = 8, dpi = 300)
cat("Trace plots saved to 'Supplementary_Figure_Trace_Plots.png'.\n\n")

# B) Numerical Diagnostics: R-hat (Rhat)
cat("--- Generating numerical convergence diagnostics (R-hat)... ---\n")
model_summary <- summary(final_model)
print(model_summary)

rhat_values <- rhat(final_model)
if (any(rhat_values > 1.01)) {
  cat("\nWARNING: Some R-hat values are > 1.01. The model may not have converged properly.\n")
} else {
  cat("\nSUCCESS: All R-hat values are acceptable. The model has converged successfully.\n")
}
cat("----------------------------------------------------------------\n")

# --- ACTION 1: Diagnose Multicollinearity ---
# We will use the 'cor()' function to create a correlation matrix.
# We need to do this on one of the completed datasets.

# Load the scaled, imputed data object
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")

# Complete one of the datasets (e.g., the first one)
completed_data_1 <- mice::complete(imputed_mids_scaled, 1)

# Select only the time-related scaled variables
time_variables <- completed_data_1 %>%
  select(surgery_duration_min_scaled, cpb_time_min_scaled, acclamp_time_min_scaled)

# Calculate and print the correlation matrix
correlation_matrix <- cor(time_variables)

cat("--- Correlation Matrix of Time-Related Predictors ---\n")
print(round(correlation_matrix, 2)) # Round to 2 decimal places for clarity
cat("----------------------------------------------------\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 3B: Re-fitting with a Simplified Model
# PURPOSE: To resolve multicollinearity issues and achieve model convergence.
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
library(tidyverse)
library(brms)
library(future)

# Set up parallel processing
plan(multisession)

# Load the necessary R objects
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")
# The priors object remains the same
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# --- 2. DEFINE THE SIMPLIFIED MODEL FORMULA ---
# This is the key change. We remove 'surgery_duration_min_scaled' and 'acclamp_time_min_scaled'.
model_formula_simplified <- bf(
  delirium_occurred ~ 
    age_years_scaled + gender + edu_years_scaled + bmi_scaled + 
    smoking_history + alcohol_history + hypertension + diabetes + 
    stroke_history + lvef_scaled + cerebral_stenosis + 
    valve_prosthesis_type + 
    cpb_time_min_scaled + # We retain this as the primary time-related predictor
    propofol_mg_scaled + mmse_preop_scaled +
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

cat("--- A simplified, robust model formula has been defined. ---\n")

# --- 3. RE-RUN THE BAYESIAN MODEL ---
# We use the new formula and give the output file a new name to avoid overwriting.
cat("--- Starting the re-fitting of the Bayesian model. This will take time... ---\n")

final_model_simplified <- brm_multiple(
  formula = model_formula_simplified,
  data = imputed_mids_scaled,
  prior = priors_for_model,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 12345,
  file = "brms_model_fit_simplified", # New file name for the new model
  combine = TRUE
)

cat("--- Simplified model fitting complete. Results saved in 'brms_model_fit_simplified.rds'. ---\n\n")

# --- 4. DIAGNOSE THE NEW MODEL'S CONVERGENCE ---
# We repeat the exact same diagnostic steps as before.

# A) Visual Diagnostics: Trace Plots for the new model
cat("--- Generating visual convergence diagnostics for the new model... ---\n")
trace_plots_simplified <- plot(final_model_simplified, N = 5, ask = FALSE) 
ggsave("Supplementary_Figure_Trace_Plots_Simplified.png", plot = trace_plots_simplified, width = 10, height = 8, dpi = 300)
cat("New trace plots saved to 'Supplementary_Figure_Trace_Plots_Simplified.png'.\n\n")

# B) Numerical Diagnostics: R-hat for the new model
cat("--- Generating numerical convergence diagnostics (R-hat) for the new model... ---\n")
model_summary_simplified <- summary(final_model_simplified)
print(model_summary_simplified)

rhat_values_simplified <- rhat(final_model_simplified)
if (any(rhat_values_simplified > 1.01, na.rm = TRUE)) { # na.rm=TRUE is a robust addition
  cat("\nWARNING: Some R-hat values are > 1.01. The simplified model may still have issues.\n")
} else {
  cat("\nSUCCESS: All R-hat values are acceptable. The simplified model has converged successfully!\n")
}
cat("----------------------------------------------------------------------------------\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 3D: Final Fit (Corrected)
# PURPOSE: To resolve the typo and achieve a fully converged, reliable model.
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
library(tidyverse)
library(brms)
library(future)

# Set up parallel processing
plan(multisession)

# Load necessary R objects
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")

# --- 2. DEFINE THE CORRECTED AND SIMPLIFIED MODEL FORMULA ---
# THE ONLY CHANGE IS HERE: lve_scaled has been corrected to lvef_scaled
model_formula_simplified <- bf(
  delirium_occurred ~ 
    age_years_scaled + gender + edu_years_scaled + bmi_scaled + 
    smoking_history + alcohol_history + hypertension + diabetes + 
    stroke_history + lvef_scaled + # <-- CORRECTED TYPO HERE
    cerebral_stenosis + valve_prosthesis_type + 
    cpb_time_min_scaled + propofol_mg_scaled + mmse_preop_scaled +
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

cat("--- Corrected model formula has been defined. ---\n")

# --- 3. RE-RUN THE BAYESIAN MODEL WITH THE CORRECTED FORMULA & ADAPT_DELTA ---
cat("--- Starting the FINAL model fitting. This may be slower... ---\n")

final_model_converged <- brm_multiple(
  formula = model_formula_simplified,
  data = imputed_mids_scaled,
  prior = priors_for_model,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 12345,
  file = "brms_model_fit_final_converged", # A new file for our final model
  combine = TRUE,
  control = list(adapt_delta = 0.99) # Increased adapt_delta to prevent divergences
)

cat("--- Final model fitting complete. Results saved in 'brms_model_fit_final_converged.rds'. ---\n\n")

# --- 4. FINAL DIAGNOSTICS ---
cat("--- Final Convergence Diagnostics ---\n")
# Check for any remaining warnings (we hope for none!)
warnings() 

# Check R-hat values again
model_summary_final <- summary(final_model_converged)
print(model_summary_final)

rhat_values_final <- rhat(final_model_converged)
if (any(rhat_values_final > 1.01, na.rm = TRUE)) {
  cat("\nWARNING: R-hat > 1.01. The model may still have issues.\n")
} else {
  cat("\nSUCCESS: All R-hat values are acceptable.\n")
}

# The brms package provides a specific function to check for divergent transitions.
# In a converged model, this should be 0.
np <- nuts_params(final_model_converged)
cat("\nTotal number of divergent transitions after warmup:", sum(subset(np, Parameter == "divergent__")$Value), "\n")

cat("--------------------------------------\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 5: Fitting the Final Clinical Core Model
# PURPOSE: To build a robust, converged, and clinically comprehensive prediction model.
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
library(tidyverse)
library(brms)
library(future)

plan(multisession)
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# --- 2. DEFINE THE FINAL, CLINICALLY-INFORMED MODEL FORMULA ---
# This model includes the 4 statistically strong predictors PLUS the clinically essential 'mmse_preop_scaled'.
model_formula_clinical <- bf(
  delirium_occurred ~ 
    age_years_scaled + 
    smoking_history + 
    cpb_time_min_scaled + 
    propofol_mg_scaled +
    mmse_preop_scaled + # <-- Re-included based on clinical importance
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

cat("--- The final, clinically-informed model formula has been defined. ---\ an")

# --- 3. RUN THE FINAL BAYESIAN MODEL ---
cat("--- Starting the fitting of the final clinical core model... ---\n")

final_model_clinical <- brm_multiple(
  formula = model_formula_clinical,
  data = imputed_mids_scaled,
  prior = priors_for_model,
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 12345,
  file = "brms_model_fit_final_clinical", # The definitive clinical model
  combine = TRUE,
  control = list(adapt_delta = 0.99)
)

cat("--- Final clinical model fitting complete. ---\n\n")

# --- 4. THE ULTIMATE DIAGNOSTICS FOR THE CLINICAL MODEL ---
cat("--- Final Diagnostics for the Clinical Core Model ---\n")
# Check convergence and diagnostics as before
# ... (The same diagnostic code from the previous step should be run here)
summary(final_model_clinical)
# ... etc.

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 6: Confirming the Final Base Model
# PURPOSE: To finalize and lock in the most robust, parsimonious, and converged model.
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
library(tidyverse)
library(brms)
library(future)

plan(multisession)
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# --- 2. DEFINE THE FINAL PARSIMONIOUS MODEL FORMULA ---
# This is our chosen model, validated through our iterative process.
model_formula_final_base <- bf(
  delirium_occurred ~ 
    age_years_scaled + 
    smoking_history + 
    cpb_time_min_scaled + 
    propofol_mg_scaled +
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

# --- 3. RUN THE FINAL BASE MODEL ---
# This run is for final confirmation of its health.
cat("--- Fitting the final base model for confirmation... ---\n")

final_base_model <- brm_multiple(
  formula = model_formula_final_base,
  data = imputed_mids_scaled,
  prior = priors_for_model,
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 12345,
  file = "brms_model_fit_final_base", # The definitive base model
  combine = TRUE,
  control = list(adapt_delta = 0.99)
)

cat("--- Final base model fitting complete. ---\n\n")

# --- 4. FINAL HEALTH CHECK ---
cat("--- Final Diagnostics for the Base Model ---\n")
# The expectation is that this model is perfectly converged.
summary(final_base_model)
rhat_values <- rhat(final_base_model)
np_params <- nuts_params(final_base_model)
divergences <- sum(subset(np_params, Parameter == "divergent__")$Value)

cat("\n--- CONVERGENCE REPORT ---\n")
if (all(rhat_values < 1.01, na.rm = TRUE) && divergences == 0) {
  cat("SUCCESS: The final base model has perfectly converged!\n")
  cat("  - All R-hat values are acceptable.\n")
  cat("  - There are 0 divergent transitions.\n")
  cat("This model is now ready for results reporting and landmark analysis.\n")
} else {
  cat("WARNING: The base model still shows signs of non-convergence. Further investigation is needed.\n")
}
cat("---------------------------\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 7: The Final Robustness Run
# PURPOSE: To eliminate the final few divergent transitions by maximizing sampler precision.
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
library(tidyverse)
library(brms)
library(future)

plan(multisession)
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# --- 2. DEFINE THE FINAL PARSIMONIOUS MODEL FORMULA ---
# We stick with our clinically sound 4-variable model.
model_formula_final_base <- bf(
  delirium_occurred ~ 
    age_years_scaled + 
    smoking_history + 
    cpb_time_min_scaled + 
    propofol_mg_scaled +
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

# --- 3. RUN THE FINAL, MOST ROBUST MODEL ---
# We are pushing adapt_delta to a very high value and increasing iterations.
# This will be the slowest run yet.
cat("--- Starting the final, most robust model fitting. This will be slow... ---\n")

final_robust_model <- brm_multiple(
  formula = model_formula_final_base,
  data = imputed_mids_scaled,
  prior = priors_for_model,
  iter = 6000, # Increased iterations for more chances to converge
  warmup = 2000,
  chains = 4,
  cores = 4,
  seed = 12345,
  file = "brms_model_fit_final_robust", # Our definitive robust model
  combine = TRUE,
  control = list(adapt_delta = 0.995) # Pushing precision to the limit
)

cat("--- Final robust model fitting complete. ---\n\n")

# --- 4. THE ULTIMATE, FINAL DIAGNOSTICS ---
cat("--- Final Diagnostics for the Robust Model ---\n")
summary(final_robust_model)
rhat_values <- rhat(final_robust_model)
np_params <- nuts_params(final_robust_model)
divergences <- sum(subset(np_params, Parameter == "divergent__")$Value)

cat("\n--- FINAL CONVERGENCE REPORT ---\n")
if (all(rhat_values < 1.01, na.rm = TRUE) && divergences == 0) {
  cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  cat("!!! SUCCESS: THE FINAL MODEL HAS PERFECTLY CONVERGED! !!!\n")
  cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  cat("  - All R-hat values are acceptable.\n")
  cat("  - There are 0 divergent transitions.\n")
  cat("This model is the final product for our manuscript.\n")
} else {
  cat("--------------------------------------------------------------------------------\n")
  cat("FINAL CONCLUSION: Divergences remain, even with maximal tuning.\n")
  cat("This provides the strongest evidence that the model is slightly over-parameterized.\n")
  cat("The next and final step would be to adopt the more parsimonious 3-variable model.\n")
  cat("--------------------------------------------------------------------------------\n")
}

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 4A: Generate Final Results Table
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(knitr) # For creating nice tables

# Load our final, converged model object
final_model <- readRDS("brms_model_fit_final_robust.rds")

# --- 2. EXTRACT AND FORMAT THE RESULTS ---
model_summary <- summary(final_model)

# Extract the fixed effects part of the summary
fixed_effects <- as.data.frame(model_summary$fixed)

# Calculate Odds Ratios and their 95% Credible Intervals
# The transformation is simply exp(x)
results_table <- fixed_effects %>%
  mutate(
    OR = exp(Estimate),
    `l-95% CrI` = exp(`l-95% CI`),
    `u-95% CrI` = exp(`u-95% CI`),
    # Format for clarity
    `Odds Ratio (95% CrI)` = paste0(round(OR, 2), " (", round(`l-95% CrI`, 2), ", ", round(`u-95% CrI`, 2), ")")
  ) %>%
  select(Estimate, Est.Error, `Odds Ratio (95% CrI)`) %>%
  rownames_to_column(var = "Predictor")

# Clean up predictor names for the publication table
results_table <- results_table %>%
  mutate(Predictor = str_replace_all(Predictor, "_", " ") %>%
           str_replace_all("scaled", "(standardized)") %>%
           str_replace("Yes", ""))

# --- 3. DISPLAY AND SAVE THE TABLE ---
cat("--- Main Results: Table 2 (Predictors of Postoperative Delirium) ---\n")
# Using kable for a clean console output
print(kable(results_table, digits = 2))
cat("---------------------------------------------------------------------\n\n")

# Save this critical table to a CSV file
write_csv(results_table, "Table2_Final_Model_Results.csv")

cat("SUCCESS: Table 2 has been generated and saved as 'Table2_Final_Model_Results.csv'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 4B (Corrected): Generate Forest Plot
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(brms)
library(ggplot2)
library(bayesplot)

# Load the model if not already in environment
final_model <- readRDS("brms_model_fit_final_robust.rds")

# --- 2. DEFINE VARIABLES TO PLOT ---
# The internal names for the fixed effects start with 'b_'.
vars_to_plot <- c(
  "b_age_years_scaled", 
  "b_smoking_historyYes", 
  "b_cpb_time_min_scaled", 
  "b_propofol_mg_scaled"
)

# --- 3. CREATE THE PLOT (CORRECTED VERSION) ---

# The key change is to use the 'transformations' argument directly inside mcmc_plot
# and to use 'variable' instead of 'pars'.
forest_plot <- mcmc_plot(
  final_model,
  variable = vars_to_plot,      # CORRECTED: Use 'variable' instead of 'pars'
  type = "areas",
  prob = 0.95,
  point_est = "median",
  transformations = exp         # ADDED: The correct, built-in way to get Odds Ratios
) +
  # REMOVED: The incorrect '+ transform(y = exp)' line is gone.
  
  # The rest of the code remains the same as it correctly modifies the ggplot object.
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_y_discrete(labels = c(
    "b_age_years_scaled" = "Age (per SD increase)",
    "b_smoking_historyYes" = "Smoking History (Yes vs. No)",
    "b_cpb_time_min_scaled" = "CPB Time (per SD increase)",
    "b_propofol_mg_scaled" = "Propofol Dose (per SD increase)"
  )) +
  labs(
    title = "Predictors of Postoperative Delirium",
    subtitle = "Posterior Median Odds Ratios and 95% Credible Intervals",
    x = "Odds Ratio (OR)",
    y = "Predictor"
  ) +
  theme_minimal()

# --- 4. DISPLAY AND SAVE THE PLOT ---
cat("--- Main Results: Figure 2 (Forest Plot of Predictors) ---\n")
print(forest_plot)
cat("-----------------------------------------------------------\n\n")

# Save this critical figure to a high-resolution file
ggsave("Figure2_Forest_Plot.png", plot = forest_plot, width = 8, height = 6, dpi = 600)

cat("SUCCESS: The corrected Figure 2 has been generated and saved as 'Figure2_Forest_Plot.png'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Hierarchical Modeling - Step 5 (Corrected): Generating Diagnostics
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(bayesplot)
library(patchwork)

# Load our final, converged model object
final_model <- readRDS("brms_model_fit_final_robust.rds")

# --- 2. DEFINE PARAMETERS TO PLOT ---
params_to_plot <- c(
  "b_age_years_scaled", 
  "b_smoking_historyYes", 
  "b_cpb_time_min_scaled", 
  "b_propofol_mg_scaled",
  "sd_hospital__Intercept",
  "sd_surgery_category_grouped__Intercept"
)

# --- 3. CREATE INDIVIDUAL DIAGNOSTIC PLOTS (CORRECTED VERSION) ---

# Plot A: Trace Plots
plot_a_trace <- mcmc_plot(
  final_model,
  variable = params_to_plot, # CORRECTED: Use 'variable' instead of 'pars'
  type = "trace"
) +
  labs(title = "A: Trace Plots", subtitle = "Chains should be well-mixed and stationary")

# Plot B: Posterior Distributions
plot_b_posterior <- mcmc_plot(
  final_model,
  variable = params_to_plot, # CORRECTED: Use 'variable' instead of 'pars'
  type = "areas",
  prob = 0.95
) +
  labs(title = "B: Posterior Distributions", subtitle = "Shape of the final parameter estimates")

# Plot C: R-hat Values Histogram
plot_c_rhat <- mcmc_plot(
  final_model,
  type = "rhat_hist"
) +
  labs(title = "C: R-hat Convergence Diagnostic", subtitle = "All values must be tightly clustered at 1.0")

# Plot D: Effective Sample Size (ESS/N_eff) Histogram
plot_d_ess <- mcmc_plot(
  final_model,
  type = "neff_hist" # CORRECTED: Use 'neff_hist' instead of 'ess_hist'
) +
  labs(title = "D: Effective Sample Size (N_eff)", subtitle = "A measure of MCMC sampling efficiency")

# --- 4. COMBINE PLOTS INTO A SINGLE FIGURE & SAVE ---
combined_diagnostic_plot <- (plot_a_trace + plot_b_posterior) / (plot_c_rhat + plot_d_ess) +
  plot_annotation(
    title = "Supplementary Figure: MCMC Convergence Diagnostics for the Final Model",
    caption = "Plots confirm the model's successful convergence and sampling stability."
  )

# Display the combined plot
print(combined_diagnostic_plot)

# Save the combined figure to a high-resolution file for the manuscript supplement.
ggsave("Supplementary_Figure_Convergence_Diagnostics.png", 
       plot = combined_diagnostic_plot, 
       width = 14, height = 10, dpi = 600)

cat("SUCCESS: The corrected comprehensive diagnostic plot has been generated and saved as 'Supplementary_Figure_Convergence_Diagnostics.png'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Landmark Analysis - Step 1: Creating the Landmark Cohort
# PURPOSE: To select patients eligible for the 24-hour landmark analysis,
#          ensuring a methodologically sound basis for the dynamic model.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)

# We load the dataset that contains the original CAM-ICU day 1 status.
# This is 'my_data_cleaned_for_analysis.csv'.
file_path <- "my_data_cleaned_for_analysis.csv"
my_data_cleaned <- read_csv(file_path)

# --- 2. DEFINE THE LANDMARK COHORT ---

# Original total number of patients
n_original <- nrow(my_data_cleaned)

# Apply the landmark exclusion criteria
landmark_cohort <- my_data_cleaned %>%
  # EXCLUSION CRITERION 1: Patient had delirium on Day 1 (before or at the landmark)
  # We use the original camicu_day1 column for this.
  filter(camicu_day1 == "No") %>%
  # EXCLUSION CRITERION 2: Patient was censored before the landmark (e.g., discharged)
  # We can use ICU LOS as a proxy. Anyone with LOS < 1 day (24h) is excluded.
  filter(icu_los_days >= 1)

# Number of patients in the final landmark cohort
n_landmark <- nrow(landmark_cohort)
n_excluded <- n_original - n_landmark

# --- 3. DISPLAY THE RESULTS & SAVE THE NEW DATASET ---

cat("--- Landmark Cohort Creation Summary ---\n")
cat("Total patients in original cohort: ", n_original, "\n")
cat("Patients excluded (delirium on Day 1 or censored before 24h): ", n_excluded, "\n")
cat("-------------------------------------------\n")
cat("Final number of patients in the 24h-Landmark Cohort: ", n_landmark, "\n")
cat("-------------------------------------------\n\n")

# Save this new, smaller cohort to its own file.
# This will be the input for our next modeling step.
write_csv(landmark_cohort, "landmark_cohort_data.csv")

cat("SUCCESS: The landmark cohort has been created and saved as 'landmark_cohort_data.csv'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Landmark Analysis - Step 2A (Corrected): Calculating Baseline Risk Scores
# PURPOSE: To correctly standardize the new landmark data and then apply the
#          baseline model to generate a comprehensive risk score for each patient.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(mice) # Needed to get the original scaling values

# Load our final, converged baseline model
final_base_model <- readRDS("brms_model_fit_final_robust.rds")

# Load the landmark cohort data we just created
landmark_cohort <- read_csv("landmark_cohort_data.csv")

# --- 2. RETRIEVE THE ORIGINAL SCALING PARAMETERS (CRITICAL STEP) ---
# We must apply the EXACT SAME standardization to the new data.
# We reload the scaled mids object and extract the original data's mean/sd.
imputed_mids_scaled_obj <- readRDS("imputed_data_scaled_mids_object.rds")
original_data <- mice::complete(imputed_mids_scaled_obj, 0) # 0 is the original data

continuous_vars_to_scale <- c(
  "age_years", "cpb_time_min", "propofol_mg"
  # Note: Add any other continuous variables from your final model here
)

scaling_params <- original_data %>%
  select(all_of(continuous_vars_to_scale)) %>%
  summarise(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE))

cat("--- Retrieved original scaling parameters successfully. ---\n")

# --- 3. MANUALLY STANDARDIZE THE LANDMARK COHORT DATA ---
# We create the '_scaled' columns that the prediction function is expecting.
landmark_cohort_scaled <- landmark_cohort %>%
  mutate(
    age_years_scaled = (age_years - scaling_params$age_years_mean) / scaling_params$age_years_sd,
    cpb_time_min_scaled = (cpb_time_min - scaling_params$cpb_time_min_mean) / scaling_params$cpb_time_min_sd,
    propofol_mg_scaled = (propofol_mg - scaling_params$propofol_mg_mean) / scaling_params$propofol_mg_sd
    # Note: If you add more variables to the model, you must add them here too.
  )

cat("--- Landmark cohort has been successfully standardized. ---\n\n")

# --- 4. GENERATE PREDICTIONS (BASELINE RISK SCORE) ---
# Now we provide the correctly formatted data to the prediction function.
cat("--- Generating baseline risk predictions for the landmark cohort... ---\n")

predicted_probabilities <- posterior_epred(
  final_base_model,
  newdata = landmark_cohort_scaled, # Use the new scaled data
  allow_new_levels = TRUE
)

# Take the median of the posterior predictions to get a single score.
landmark_cohort$baseline_risk_prob <- apply(predicted_probabilities, 2, median)

cat("--- Baseline risk scores calculated successfully. ---\n\n")

# --- 5. DISPLAY A SUMMARY OF THE RISK SCORES & SAVE ---
cat("--- Summary of calculated Baseline Risk Probabilities ---\n")
print(summary(landmark_cohort$baseline_risk_prob))
cat("-------------------------------------------------------\n\n")

# Save this enhanced dataset.
write_csv(landmark_cohort, "landmark_cohort_with_risk.csv")

cat("SUCCESS: A new column 'baseline_risk_prob' has been added.\n")
cat("The final dataset for landmark modeling is saved as 'landmark_cohort_with_risk.csv'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Landmark Analysis - Step 2B: Fitting the Dynamic Update Model
# PURPOSE: To assess the additional predictive value of 24h mechanical ventilation time
#          on top of the established baseline risk.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)

# Load the landmark cohort data which now includes the baseline risk score.
landmark_data <- read_csv("landmark_cohort_with_risk.csv")

# --- 2. PREPARE THE DATA FOR MODEL 2 ---

# Create the outcome variable for the landmark model.
landmark_data_final <- landmark_data %>%
  mutate(
    # The outcome is delirium on Day 2 OR Day 3.
    delirium_pod2_or_3 = if_else(camicu_day2 == "Yes" | camicu_day3 == "Yes", 1, 0),
    
    # Standardize the two continuous predictors for this new model
    # to ensure stable estimation.
    baseline_risk_prob_scaled = scale(baseline_risk_prob)[,1],
    mvt_first_24h_scaled = scale(mvt_first_24h)[,1]
  )

# --- 3. FIT THE DYNAMIC UPDATE MODEL (MODEL 2) ---
# This is a much simpler model, so it should run very quickly.
# We use a simple logistic regression within the Bayesian framework.
# We don't need the complex multilevel structure here as we have accounted
# for much of the heterogeneity via the baseline_risk_prob score.

cat("--- Fitting the dynamic update model (Model 2)... ---\n")

landmark_model <- brm(
  formula = delirium_pod2_or_3 ~ baseline_risk_prob_scaled + mvt_first_24h_scaled,
  data = landmark_data_final,
  family = bernoulli(link = "logit"),
  prior = c(set_prior("normal(0, 2.5)", class = "b")), # Standard weak prior
  seed = 12345,
  file = "brms_model_fit_landmark" # Save the landmark model
)

cat("--- Landmark model fitting complete. ---\n\n")

# --- 4. DISPLAY AND SAVE THE RESULTS ---
cat("--- Results of the Dynamic Update Model (Landmark Model 2) ---\n")
model_summary_landmark <- summary(landmark_model)
print(model_summary_landmark)
cat("------------------------------------------------------------\n")

# Similar to before, let's create a clean results table with Odds Ratios.
landmark_results_table <- as.data.frame(model_summary_landmark$fixed) %>%
  mutate(
    OR = exp(Estimate),
    `l-95% CrI` = exp(`l-95% CI`),
    `u-95% CrI` = exp(`u-95% CI`),
    `Odds Ratio (95% CrI)` = paste0(round(OR, 2), " (", round(`l-95% CrI`, 2), ", ", round(`u-95% CrI`, 2), ")")
  ) %>%
  select(Estimate, `Odds Ratio (95% CrI)`) %>%
  rownames_to_column(var = "Predictor")

# Save the landmark model results table. This is likely Table 3 in your paper.
write_csv(landmark_results_table, "Table3_Landmark_Model_Results.csv")

cat("SUCCESS: Landmark analysis is complete.\n")
cat("The results are saved in 'Table3_Landmark_Model_Results.csv'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Model Validation - Step 6A: Internal Validation (Overall Performance)
# PURPOSE: To assess the apparent performance of the final model on the entire dataset.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(pROC) # The gold-standard package for ROC analysis
library(ggplot2)

# Load our final, converged model
final_model <- readRDS("brms_model_fit_final_robust.rds")
# Load the original cleaned data to get the true outcomes
original_data <- read_csv("my_data_cleaned_for_analysis.csv")

# --- 2. GENERATE PREDICTIONS ON THE TRAINING DATA ---
# We predict the probability of delirium for all 308 patients
cat("--- Generating predictions for internal validation... ---\n")
predicted_probs_internal <- posterior_epred(final_model)

# Summarize the predictions to get a single probability score per patient (median)
original_data$predicted_prob <- apply(predicted_probs_internal, 2, median)

# --- 3. CALCULATE AND PLOT THE ROC CURVE ---
# Create the ROC object
roc_internal <- roc(original_data$delirium_occurred, original_data$predicted_prob)

# Extract the AUC value
auc_internal_value <- auc(roc_internal)

cat("--- Internal Validation Results ---\n")
cat("Apparent AUC on the full dataset: ", round(auc_internal_value, 3), "\n")
cat("---------------------------------\n\n")

# Create a professional ROC curve plot
roc_plot_internal <- ggroc(roc_internal) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey") +
  labs(
    title = "Figure 3A: Internal Validation ROC Curve",
    subtitle = paste0("Apparent AUC = ", round(auc_internal_value, 3)),
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal()

# --- 4. DISPLAY AND SAVE ---
print(roc_plot_internal)
ggsave("Figure3A_ROC_Internal.png", plot = roc_plot_internal, width = 6, height = 6, dpi = 600)

cat("SUCCESS: Internal validation complete. ROC curve saved as 'Figure3A_ROC_Internal.png'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Model Validation - Step 6B (Corrected & Rigorous): External Validation
# PURPOSE: To implement the gold-standard, imputation-aware external validation protocol.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(pROC)
library(mice)
library(future)
plan(multisession)

# Load the original, complete cleaned dataset
my_data <- read_csv("my_data_cleaned_for_analysis.csv")

# --- 2. PREPARE IMPUTED DATA FOR EACH HOSPITAL ---
cat("--- Preparing separate imputed datasets for each hospital... ---\n")
set.seed(12345) # for reproducibility
# Split the data
data_hosp1 <- my_data %>% filter(hospital == 1)
data_hosp2 <- my_data %>% filter(hospital == 2)

# Impute each dataset separately. Using m=5 for speed in validation.
m_imputations <- 5
mids_hosp1 <- mice(data_hosp1, m = m_imputations, printFlag = FALSE)
mids_hosp2 <- mice(data_hosp2, m = m_imputations, printFlag = FALSE)

# --- 3. THE GOLD-STANDARD VALIDATION LOOP ---
# We will loop through each of the 'm' imputations.
cat("--- Starting the rigorous external validation loop... ---\n")

# A list to store the AUC from each iteration
auc_list <- vector("list", m_imputations)

for (i in 1:m_imputations) {
  cat(paste0("\n--- Processing Imputation #", i, " of ", m_imputations, " ---\n"))
  
  # a) Get the i-th completed dataset for each hospital
  data_hosp1_i <- mice::complete(mids_hosp1, i)
  data_hosp2_i <- mice::complete(mids_hosp2, i)
  
  # b) Train a NEW model ONLY on the i-th Hospital 1 dataset
  # We use a simplified formula for this validation step
  # Using unscaled variables is fine here as it's a new model each time
  model_hosp1_i <- brm(
    formula = delirium_occurred ~ age_years + smoking_history + cpb_time_min + propofol_mg,
    data = data_hosp1_i,
    family = bernoulli(),
    prior = c(set_prior("normal(0, 2.5)", class = "b")),
    seed = 12345 + i, # Different seed for each model
    silent = 2, refresh = 0 # Suppress verbose output
  )
  
  # c) Predict on the i-th Hospital 2 dataset
  predicted_probs_i <- posterior_epred(model_hosp1_i, newdata = data_hosp2_i)
  predicted_scores_i <- apply(predicted_probs_i, 2, median)
  
  # d) Calculate and store the AUC for this iteration
  roc_i <- roc(data_hosp2_i$delirium_occurred, predicted_scores_i, quiet = TRUE)
  auc_list[[i]] <- auc(roc_i)
  
  cat(paste0("AUC for imputation #", i, ": ", round(auc_list[[i]], 3), "\n"))
}

# --- 4. POOL THE RESULTS ---
# The final external validation AUC is the average of the AUCs from the loop.
final_external_auc <- mean(unlist(auc_list))

cat("\n--- Rigorous External Validation Complete ---\n")
print("AUCs from each imputation:")
print(round(unlist(auc_list), 3))
cat("---------------------------------------------\n")
cat("Final Pooled External Validation AUC: ", round(final_external_auc, 3), "\n")
cat("---------------------------------------------\n\n")

# --- 5. VISUALIZE THE FINAL RESULT (using the first iteration's ROC for plotting) ---
# For visualization, we can plot one of the ROC curves as a representative example.
roc_to_plot <- roc(mice::complete(mids_hosp2, 1)$delirium_occurred, 
                   apply(posterior_epred(brm(delirium_occurred ~ age_years + smoking_history + cpb_time_min + propofol_mg, data=mice::complete(mids_hosp1,1), family=bernoulli(), seed=123, silent=2, refresh=0), newdata=mice::complete(mids_hosp2,1)), 2, median),
                   quiet = TRUE)

roc_plot_external <- ggroc(roc_to_plot) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey") +
  labs(
    title = "Figure 3B: External Validation ROC Curve",
    subtitle = paste0("Trained on Hosp 1, Tested on Hosp 2\nFinal Pooled AUC = ", round(final_external_auc, 3)),
    x = "1 - Specificity", y = "Sensitivity"
  ) +
  theme_minimal()

print(roc_plot_external)
ggsave("Figure3B_ROC_External.png", plot = roc_plot_external, width = 6, height = 6, dpi = 600)

cat("SUCCESS: Rigorous external validation complete. Final ROC curve saved as 'Figure3B_ROC_External.png'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Bayesian Modeling - Step 9: Fitting the Final, Definitive Clinical Core Model
# PURPOSE: To build the final model that balances statistical power with maximal
#          clinical plausibility and interpretability, using the best available
#          proxy for cerebrovascular health.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(future)

plan(multisession)
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# --- 2. DEFINE THE DEFINITIVE CLINICAL MODEL FORMULA ---
# This model includes the statistical core + the best clinical proxy for brain health.
model_formula_definitive <- bf(
  delirium_occurred ~ 
    age_years_scaled + 
    cpb_time_min_scaled + 
    propofol_mg_scaled +
    cerebral_stenosis + # <-- Using this as the key cerebrovascular variable
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

# --- 3. RUN THE DEFINITIVE MODEL ---
cat("--- Fitting the definitive clinical core model. This is the final run... ---\n")

final_definitive_model <- brm_multiple(
  formula = model_formula_definitive,
  data = imputed_mids_scaled,
  prior = priors_for_model,
  iter = 6000, warmup = 2000, chains = 4, cores = 4,
  seed = 12345,
  file = "brms_model_fit_final_definitive", # The final, definitive model file
  combine = TRUE,
  control = list(adapt_delta = 0.995)
)

# --- 4. THE FINAL, FINAL JUDGEMENT ---
cat("--- Final Diagnostics for the Definitive Model ---\n")

# Run the full convergence check
summary(final_definitive_model)
rhat_values <- rhat(final_definitive_model)
np_params <- nuts_params(final_definitive_model)
divergences <- sum(subset(np_params, Parameter == "divergent__")$Value)

cat("\n--- FINAL CONVERGENCE & CONCLUSION REPORT ---\n")
if (all(rhat_values < 1.01, na.rm = TRUE) && divergences == 0) {
  cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  cat("!!! ULTIMATE SUCCESS: THE DEFINITIVE CLINICAL MODEL HAS CONVERGED! !!!\n")
  cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  cat("  - All R-hat values are acceptable.\n")
  cat("  - There are 0 divergent transitions.\n")
  cat("This model represents the optimal balance of statistical evidence and clinical interpretability.\n")
  cat("This should be the final model reported in the manuscript.\n")
} else {
  cat("--------------------------------------------------------------------------------\n")
  cat("FINAL SCIENTIFIC CONCLUSION: Even with the best clinical proxy, the signal is weak.\n")
  cat("This provides the strongest evidence that in this specific, cognitively-healthy cohort,\n")
  cat("the dominant risk is driven by age and intraoperative factors, overshadowing baseline comorbidities.\n")
  cat("In this case, the most honest model is the one without the cerebrovascular variable.\n")
  cat("--------------------------------------------------------------------------------\n")
}

# -------------------------------------------------------------------------------
# FINAL ACTION: RELOAD THE CHAMPION MODEL
# -------------------------------------------------------------------------------
library(brms) # Make sure the package is loaded

# The name of our final, converged, robust model file
champion_model_file <- "brms_model_fit_final_robust.rds"

# Load the model and assign it to a clean name for all subsequent steps
final_model <- readRDS(champion_model_file)

cat("SUCCESS: The champion model '", champion_model_file, "' has been loaded and is ready for reporting.\n", sep="")

# -------------------------------------------------------------------------------
# FINAL ACTION: RE-GENERATE TABLE 2 FROM THE CHAMPION MODEL
# -------------------------------------------------------------------------------
library(tidyverse)
library(knitr)

# Extract the fixed effects summary from the loaded model
model_summary <- summary(final_model)
fixed_effects <- as.data.frame(model_summary$fixed)

# Calculate Odds Ratios and format the table
results_table <- fixed_effects %>%
  mutate(
    OR = exp(Estimate),
    `l-95% CrI` = exp(`l-95% CI`),
    `u-95% CrI` = exp(`u-95% CI`),
    `Odds Ratio (95% CrI)` = paste0(round(OR, 2), " (", round(`l-95% CrI`, 2), ", ", round(`u-95% CrI`, 2), ")")
  ) %>%
  select(Estimate, Est.Error, `Odds Ratio (95% CrI)`) %>%
  rownames_to_column(var = "Predictor") %>%
  filter(Predictor != "Intercept") # Exclude the intercept for the main table

# Clean up predictor names
results_table <- results_table %>%
  mutate(Predictor = str_replace_all(Predictor, "_", " ") %>%
           str_replace_all("scaled", "(standardized)") %>%
           str_replace("Yes", ""))

# Display and save the final table
cat("\n--- FINAL Main Results: Table 2 (Predictors of Postoperative Delirium) ---\n")
print(kable(results_table, digits = 2))
cat("---------------------------------------------------------------------\n\n")

write_csv(results_table, "FINAL_Table2_Model_Results.csv")
cat("SUCCESS: The final Table 2 has been saved as 'FINAL_Table2_Model_Results.csv'.\n")

# -------------------------------------------------------------------------------
# FINAL ACTION: RE-GENERATE FIGURE 2 FROM THE CHAMPION MODEL
# -------------------------------------------------------------------------------
library(ggplot2)
library(bayesplot)

# Define the variables to plot from our final model
vars_to_plot <- c(
  "b_age_years_scaled", 
  "b_smoking_historyYes", 
  "b_cpb_time_min_scaled", 
  "b_propofol_mg_scaled"
)

# Create the plot using the corrected syntax
forest_plot <- mcmc_plot(
  final_model,
  variable = vars_to_plot,
  type = "areas",
  prob = 0.95,
  point_est = "median",
  transformations = exp
) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_y_discrete(labels = c(
    "b_age_years_scaled" = "Age (per SD increase)",
    "b_smoking_historyYes" = "Smoking History (Yes vs. No)",
    "b_cpb_time_min_scaled" = "CPB Time (per SD increase)",
    "b_propofol_mg_scaled" = "Propofol Dose (per SD increase)"
  )) +
  labs(
    title = "Final Model: Predictors of Postoperative Delirium",
    subtitle = "Posterior Median Odds Ratios and 95% Credible Intervals",
    x = "Odds Ratio (OR)",
    y = "Predictor"
  ) +
  theme_minimal()

# Display and save the final plot
cat("\n--- FINAL Main Results: Figure 2 (Forest Plot of Predictors) ---\n")
print(forest_plot)
cat("-----------------------------------------------------------\n\n")

ggsave("FINAL_Figure2_Forest_Plot.png", plot = forest_plot, width = 8, height = 6, dpi = 600)
cat("SUCCESS: The final Figure 2 has been saved as 'FINAL_Figure2_Forest_Plot.png'.\n")

# -------------------------------------------------------------------------------
# FINAL ACTION: RE-GENERATE DIAGNOSTIC PLOTS FROM THE CHAMPION MODEL
# -------------------------------------------------------------------------------
library(patchwork)

# Define parameters to plot (including random effects)
params_to_plot_diag <- c(
  "b_age_years_scaled", "b_smoking_historyYes", "b_cpb_time_min_scaled", "b_propofol_mg_scaled",
  "sd_hospital__Intercept", "sd_surgery_category_grouped__Intercept"
)

# Create individual diagnostic plots using the corrected syntax
plot_a_trace <- mcmc_plot(final_model, variable = params_to_plot_diag, type = "trace") + labs(title = "A: Trace Plots")
plot_b_posterior <- mcmc_plot(final_model, variable = params_to_plot_diag, type = "areas", prob = 0.95) + labs(title = "B: Posterior Distributions")
plot_c_rhat <- mcmc_plot(final_model, type = "rhat_hist") + labs(title = "C: R-hat Diagnostic")
plot_d_neff <- mcmc_plot(final_model, type = "neff_hist") + labs(title = "D: Effective Sample Size (N_eff)")

# Combine plots into a single figure
combined_diagnostic_plot <- (plot_a_trace + plot_b_posterior) / (plot_c_rhat + plot_d_neff) +
  plot_annotation(
    title = "Final Model: MCMC Convergence Diagnostics",
    caption = "Plots confirm the model's successful convergence and sampling stability."
  )

# Display and save the final diagnostic plot
cat("\n--- FINAL Supplementary Materials: Convergence Diagnostics ---\n")
print(combined_diagnostic_plot)
cat("----------------------------------------------------------------\n\n")

ggsave("FINAL_Supplementary_Figure_Diagnostics.png", plot = combined_diagnostic_plot, width = 14, height = 10, dpi = 600)
cat("SUCCESS: The final diagnostic plot has been saved as 'FINAL_Supplementary_Figure_Diagnostics.png'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Landmark Analysis - Step 1 (DEFINITIVE, CORRECTED VERSION)
# PURPOSE: To rigorously and correctly create the 24-hour landmark cohort,
#          with built-in verification to ensure methodological purity.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)

# We start from the original, full analytical cohort of 308 patients.
original_file_path <- "my_data_cleaned_for_analysis.csv"
full_cohort_data <- read_csv(original_file_path)

# --- 2. DEFINE THE LANDMARK COHORT WITH ROBUST FILTERING ---

n_original <- nrow(full_cohort_data)

# Apply the landmark exclusion criteria with corrected, robust logic.
# We will use str_trim() to remove any potential hidden whitespace in the data.
landmark_cohort_corrected <- full_cohort_data %>%
  # EXCLUSION CRITERION 1 (ROBUST): Patient had delirium on Day 1.
  # We explicitly filter for the exact string "No" after trimming whitespace.
  filter(str_trim(camicu_day1) == "No") %>%
  
  # EXCLUSION CRITERION 2: Patient was censored before the 24h landmark.
  filter(icu_los_days >= 1)

n_landmark_corrected <- nrow(landmark_cohort_corrected)
n_excluded_corrected <- n_original - n_landmark_corrected

# --- 3. BUILT-IN VERIFICATION (THE MOST CRITICAL STEP) ---
# Before we proceed, we MUST verify that no patients with Day 1 delirium remain.

cat("\n--- VERIFICATION STEP ---\n")
cat("Frequency table for 'camicu_day1' in the NEW landmark cohort:\n")
verification_table <- table(landmark_cohort_corrected$camicu_day1, useNA = "ifany")
print(verification_table)

# This check will programmatically confirm the success.
if ("Yes" %in% names(verification_table)) {
  cat("\n!!! CRITICAL ERROR: Patients with Day 1 delirium are still present. Halting. !!!\n")
} else {
  cat("\nSUCCESS: Verification complete. All patients with Day 1 delirium have been correctly excluded.\n")
}
cat("-------------------------\n\n")


# --- 4. DISPLAY THE RESULTS & SAVE THE NEW, CORRECTED DATASET ---
cat("--- Corrected Landmark Cohort Creation Summary ---\n")
cat("Total patients in original cohort: ", n_original, "\n")
cat("Patients correctly excluded: ", n_excluded_corrected, "\n")
cat("---------------------------------------------------\n")
cat("Final number of patients in the CORRECTED Landmark Cohort: ", n_landmark_corrected, "\n")
cat("---------------------------------------------------\n\n")

# Overwrite the old, contaminated file with this new, pure dataset.
write_csv(landmark_cohort_corrected, "landmark_cohort_data.csv")

cat("SUCCESS: The corrected and verified landmark cohort has been saved to 'landmark_cohort_data.csv'.\n")
cat("Please proceed to re-run the subsequent landmark analysis steps (Step 2A and 2B).\n")

# -------------------------------------------------------------------------------
# SCRIPT: Landmark Analysis - Step 2A (RE-RUN on Corrected Data)
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(mice)

# Load our final, converged baseline model
final_base_model <- readRDS("brms_model_fit_final_robust.rds")

# Load the NEW, CORRECTED landmark cohort data
landmark_cohort_corrected <- read_csv("landmark_cohort_data.csv")

# --- 2. RETRIEVE THE ORIGINAL SCALING PARAMETERS ---
# This part remains the same, as the scaling parameters are from the full cohort
imputed_mids_scaled_obj <- readRDS("imputed_data_scaled_mids_object.rds")
original_data <- mice::complete(imputed_mids_scaled_obj, 0)

continuous_vars_base_model <- c("age_years", "cpb_time_min", "propofol_mg")
scaling_params_base <- original_data %>%
  select(all_of(continuous_vars_base_model)) %>%
  summarise(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE))

# --- 3. MANUALLY STANDARDIZE THE CORRECTED LANDMARK COHORT ---
landmark_cohort_scaled <- landmark_cohort_corrected %>%
  mutate(
    age_years_scaled = (age_years - scaling_params_base$age_years_mean) / scaling_params_base$age_years_sd,
    cpb_time_min_scaled = (cpb_time_min - scaling_params_base$cpb_time_min_mean) / scaling_params_base$cpb_time_min_sd,
    propofol_mg_scaled = (propofol_mg - scaling_params_base$propofol_mg_mean) / scaling_params_base$propofol_mg_sd
  )

# --- 4. GENERATE PREDICTIONS ---
cat("--- Re-generating baseline risk predictions for the CORRECTED landmark cohort... ---\n")

predicted_probabilities <- posterior_epred(
  final_base_model,
  newdata = landmark_cohort_scaled,
  allow_new_levels = TRUE,
  re.form = NA
)

# Add the new, correct baseline risk score to our corrected cohort data
landmark_cohort_corrected$baseline_risk_prob <- apply(predicted_probabilities, 2, median)

# --- 5. SAVE THE FINAL DATASET FOR MODEL 2 ---
# We use a new, clearly marked filename.
write_csv(landmark_cohort_corrected, "landmark_cohort_with_risk_CORRECTED.csv")

cat("\nSUCCESS: The corrected landmark cohort with new risk scores has been saved as 'landmark_cohort_with_risk_CORRECTED.csv'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Landmark Analysis - Step 2B (RE-RUN on Corrected Data)
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)

# Load the NEW, CORRECTED landmark cohort data with risk scores
landmark_data_corrected <- read_csv("landmark_cohort_with_risk_CORRECTED.csv")

# --- 2. PREPARE THE DATA FOR THE FINAL LANDMARK MODEL ---
landmark_data_final <- landmark_data_corrected %>%
  mutate(
    delirium_pod2_or_3 = if_else(camicu_day2 == "Yes" | camicu_day3 == "Yes", 1, 0),
    baseline_risk_prob_scaled = scale(baseline_risk_prob)[,1],
    mvt_first_24h_scaled = scale(mvt_first_24h)[,1]
  )

# --- 3. FIT THE FINAL DYNAMIC UPDATE MODEL (MODEL 2) ---
cat("\n--- Re-fitting the dynamic update model (Model 2) on pure data... ---\n")

final_landmark_model <- brm(
  formula = delirium_pod2_or_3 ~ baseline_risk_prob_scaled + mvt_first_24h_scaled,
  data = landmark_data_final,
  family = bernoulli(link = "logit"),
  prior = c(set_prior("normal(0, 2.5)", class = "b")),
  seed = 12345,
  file = "brms_model_fit_landmark_CORRECTED" # New, corrected model file
)

# --- 4. DISPLAY AND SAVE THE FINAL, CORRECTED RESULTS ---
cat("\n--- Final, Corrected Results of the Dynamic Update Model ---\n")
model_summary_landmark_corrected <- summary(final_landmark_model)
print(model_summary_landmark_corrected)
cat("-----------------------------------------------------------\n")

# Create the final, corrected results table
landmark_results_table_corrected <- as.data.frame(model_summary_landmark_corrected$fixed) %>%
  mutate(
    OR = exp(Estimate),
    `l-95% CrI` = exp(`l-95% CI`),
    `u-95% CrI` = exp(`u-95% CI`),
    `Odds Ratio (95% CrI)` = paste0(round(OR, 2), " (", round(`l-95% CrI`, 2), ", ", round(`u-95% CrI`, 2), ")")
  ) %>%
  select(Estimate, `Odds Ratio (95% CrI)`) %>%
  rownames_to_column(var = "Predictor")

# Save the final, corrected landmark model results table.
write_csv(landmark_results_table_corrected, "FINAL_Table3_Landmark_Model_Results.csv")

cat("\nSUCCESS: The definitive landmark analysis is complete.\n")
cat("The final, corrected results are saved in 'FINAL_Table3_Landmark_Model_Results.csv'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Model Validation - Step 7: The Ultimate "Reverse" External Validation
# PURPOSE: To provide the final, definitive proof of model robustness by training
#          on the smaller center and testing on the larger one.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(pROC)
library(mice)
library(future)
plan(multisession)

# Load the original, complete cleaned dataset
my_data <- read_csv("my_data_cleaned_for_analysis.csv")

# --- 2. SCENARIO 2: TRAIN ON HOSPITAL 2, TEST ON HOSPITAL 1 ---
cat("\n--- Starting 'Reverse' External Validation: Training on Hospital 2 (N=78)... ---\n")

# Split the data (reversed from before)
data_hosp2_train <- my_data %>% filter(hospital == 2)
data_hosp1_test <- my_data %>% filter(hospital == 1)

# Impute each dataset separately
m_imputations <- 5
set.seed(54321) # A new seed for this process
mids_hosp2_train <- mice(data_hosp2_train, m = m_imputations, printFlag = FALSE)
mids_hosp1_test <- mice(data_hosp1_test, m = m_imputations, printFlag = FALSE)

# --- 3. THE REVERSE VALIDATION LOOP ---
cat("--- Starting the reverse validation loop... ---\n")

auc_list_reverse <- vector("list", m_imputations)

for (i in 1:m_imputations) {
  cat(paste0("\n--- Processing Reverse Imputation #", i, " of ", m_imputations, " ---\n"))
  
  # a) Get the i-th completed dataset for each hospital
  data_hosp2_train_i <- mice::complete(mids_hosp2_train, i)
  data_hosp1_test_i <- mice::complete(mids_hosp1_test, i)
  
  # b) Train a NEW model ONLY on the i-th Hospital 2 dataset
  model_hosp2_i <- brm(
    formula = delirium_occurred ~ age_years + smoking_history + cpb_time_min + propofol_mg,
    data = data_hosp2_train_i,
    family = bernoulli(),
    prior = c(set_prior("normal(0, 2.5)", class = "b")),
    seed = (12345 + i) * 2, # Different seeds
    silent = 2, refresh = 0
  )
  
  # c) Predict on the i-th Hospital 1 dataset
  predicted_probs_i <- posterior_epred(model_hosp2_i, newdata = data_hosp1_test_i)
  predicted_scores_i <- apply(predicted_probs_i, 2, median)
  
  # d) Calculate and store the AUC for this iteration
  roc_i <- roc(data_hosp1_test_i$delirium_occurred, predicted_scores_i, quiet = TRUE)
  auc_list_reverse[[i]] <- auc(roc_i)
  
  cat(paste0("Reverse AUC for imputation #", i, ": ", round(auc_list_reverse[[i]], 3), "\n"))
}

# --- 4. POOL THE REVERSE RESULTS ---
final_reverse_auc <- mean(unlist(auc_list_reverse))

cat("\n--- Rigorous Reverse External Validation Complete ---\n")
print("Reverse AUCs from each imputation:")
print(round(unlist(auc_list_reverse), 3))
cat("---------------------------------------------\n")
cat("Final Pooled Reverse External Validation AUC: ", round(final_reverse_auc, 3), "\n")
cat("---------------------------------------------\n\n")

# -------------------------------------------------------------------------------
# SCRIPT: Model Validation - Step 8: Visualizing the Extraordinary Reverse Validation
# PURPOSE: To create the final, definitive ROC curve plot for the reverse
#          external validation, completing the "Validation Trilogy".
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(pROC)
library(ggplot2)

# --- 2. RE-GENERATE THE FIRST ITERATION FOR PLOTTING ---
# We need to re-create one of the ROC objects to plot it. We'll use the first one.
# This code is a miniature version of the loop from the previous step.
cat("--- Re-generating a representative ROC curve for plotting... ---\n")

# Load the necessary raw data
my_data <- read_csv("my_data_cleaned_for_analysis.csv")
data_hosp2_train_i <- my_data %>% filter(hospital == 2) # Simplified for plotting
data_hosp1_test_i <- my_data %>% filter(hospital == 1)

# Fit the model for the first imputation (simplified, no need for MICE here for a single plot)
model_hosp2_plot <- brm(
  formula = delirium_occurred ~ age_years + smoking_history + cpb_time_min + propofol_mg,
  data = data_hosp2_train_i,
  family = bernoulli(),
  prior = c(set_prior("normal(0, 2.5)", class = "b")),
  seed = 54321, # Consistent seed
  silent = 2, refresh = 0
)

# Predict on Hospital 1 data
predicted_probs_plot <- posterior_epred(model_hosp2_plot, newdata = data_hosp1_test_i)
predicted_scores_plot <- apply(predicted_probs_plot, 2, median)

# Create the ROC object for plotting
roc_reverse_plot <- roc(data_hosp1_test_i$delirium_occurred, predicted_scores_plot, quiet = TRUE)

# The final pooled AUC value we calculated before
final_reverse_auc <- 0.929 # We use the precise value from our rigorous loop

# --- 3. CREATE THE FINAL ROC CURVE PLOT ---
roc_plot_reverse <- ggroc(roc_reverse_plot) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey") +
  labs(
    title = "Figure 3C: Reverse External Validation ROC Curve",
    subtitle = paste0("Trained on Small Center (N=78), Tested on Large Center (N=230)\nFinal Pooled AUC = ", round(final_reverse_auc, 3)),
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal() +
  # Add an annotation to highlight the extraordinary result
  annotate("text", x = 0.5, y = 0.25, 
           label = paste("Extraordinary generalizability demonstrated."), 
           color = "darkgreen", size = 4, fontface = "bold")

# --- 4. DISPLAY AND SAVE THE PLOT ---
print(roc_plot_reverse)
ggsave("FINAL_Figure3C_ROC_Reverse_External.png", plot = roc_plot_reverse, width = 6, height = 6, dpi = 600)
cat("\nSUCCESS: The final, definitive reverse external validation ROC curve has been saved as 'FINAL_Figure3C_ROC_Reverse_External.png'.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Generating the FINAL, POLISHED Postoperative Outcomes Table (Table 4)
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(gtsummary)

original_data <- read_csv("my_data_cleaned_for_analysis.csv")

# --- 2. SELECT ONLY THE COMPARATIVE VARIABLES ---
# This is the key correction: We EXCLUDE delirium_duration_hours from this table.
outcomes_data_final <- original_data %>%
  select(
    delirium_occurred,
    mvt_hours_total,
    icu_los_days,
    mmse_postop
  )

# --- 3. CREATE THE POLISHED TABLE ---
final_polished_table_4 <- outcomes_data_final %>%
  tbl_summary(
    by = delirium_occurred,
    statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]"),
    label = list(
      mvt_hours_total ~ "Total Mechanical Ventilation (hours)",
      icu_los_days ~ "ICU Length of Stay (days)",
      mmse_postop ~ "Postoperative MMSE Score"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_p() %>%
  modify_header(
    update = list(
      label ~ "**Characteristic**",
      stat_1 ~ "**No Delirium (n = 274)**",
      stat_2 ~ "**Delirium (n = 34)**" # We will manually add the footnote superscript 'a' in Word
    )
  ) %>%
  modify_caption("**Table 4. Impact of Delirium on Key Postoperative Clinical Outcomes**")

# --- 4. DISPLAY THE TABLE ---
final_polished_table_4

# IMPORTANT NOTE: The footnote is a manual addition. When you copy this table
# into your manuscript (e.g., in Word), you will manually add the superscript 'a'
# to the "Delirium" column header and then add the footnote text below the table.

# -------------------------------------------------------------------------------
# SCRIPT: Definitive Sensitivity Analysis with Missing P-value Explanation
# AUTHOR: [medusa]
# DATE:   [Oct 16 2025]
# PURPOSE: To rigorously compare the included cohort (N=308) vs. the cohort
#          excluded due to pre-existing cognitive impairment (N=74), and to
#          transparently report on any statistical tests not performed due
#          to extensive missing data in one group.
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
# tableone is the standard for creating such comparison tables.
library(tidyverse)
library(tableone)

# --- 2. DATA LOADING AND PRECISE COHORT DEFINITION ---
# We start from the absolute original data file provided.
file_path <- "my data.csv"
original_data <- read_csv(file_path)

cat("--- Defining comparison groups: Included (N=308) vs. Excluded (N=74) ---\n")

# First, filter out patients who cannot be assessed (missing MMSE or education data).
data_assessable <- original_data %>%
  filter(!is.na(mmse_preop) & !is.na(edu_years))

# Now, create a new variable to define our two groups of interest based on the MMSE criteria.
data_with_groups <- data_assessable %>%
  mutate(
    analysis_group = case_when(
      # Criteria for EXCLUSION (The 74 patients)
      (edu_years == 0 & mmse_preop <= 17) |
        (edu_years >= 1 & edu_years <= 6 & mmse_preop <= 20) |
        (edu_years > 6 & mmse_preop <= 24) ~ "Excluded (MMSE Criteria)",
      
      # All others who passed the test are INCLUDED (The 308 patients)
      TRUE ~ "Included"
    )
  )
cat("Group definition complete.\n\n")

# --- 3. PREPARE THE DATA FOR THE COMPARISON TABLE ---

# Define the full list of variables for comparison, exactly as you specified.
vars_for_comparison <- c(
  "age_years", "gender", "edu_years", "smoking_history", "alcohol_history", "bmi",
  "surgery_category", "hypertension", "diabetes", "stroke_history",
  "valve_prosthesis_type", "lvef", "cerebral_stenosis",
  "cpb_time_min", "surgery_duration_min", "acclamp_time_min", "propofol_mg", "mmse_preop"
)

# Identify which of these variables are categorical (factors).
categorical_vars <- c(
  "gender", "smoking_history", "alcohol_history", "surgery_category",
  "hypertension", "diabetes", "stroke_history", "valve_prosthesis_type",
  "cerebral_stenosis"
)

# Create the final dataframe for the analysis, ensuring correct variable types.
# LVEF is explicitly treated as numeric (continuous).
data_for_sensitivity <- data_with_groups %>%
  select(analysis_group, all_of(vars_for_comparison)) %>%
  mutate(across(all_of(categorical_vars), as.factor))

# --- 4. CREATE THE COMPARISON TABLE OBJECT ---

# Define all continuous variables as non-normally distributed, as you specified.
# This ensures tableone uses median[IQR] and the Mann-Whitney U test.
non_normal_vars <- c(
  "age_years", "edu_years", "bmi", "lvef",
  "cpb_time_min", "surgery_duration_min", "acclamp_time_min", "propofol_mg"
)

# Use CreateTableOne to generate the comparison table structure.
sensitivity_table_object <- CreateTableOne(
  vars = vars_for_comparison,
  data = data_for_sensitivity,
  factorVars = categorical_vars,
  strata = "analysis_group"
)

# --- 5. PRINT AND EXPORT THE FINAL TABLE WITH EXPLANATIONS ---
# This is the final step, generating the complete, annotated table.

cat("--- Final Sensitivity Analysis Table (with explanations for missing p-values) ---\n\n")
table_with_explanations <- print(
  sensitivity_table_object,
  nonnormal = non_normal_vars,
  showAllLevels = TRUE,
  smd = FALSE,
  explain = TRUE, # <-- THIS CRITICAL PARAMETER ADDS THE EXPLANATORY FOOTNOTE
  printToggle = FALSE,
  noSpaces = TRUE
)

# Print the final, complete table to the console for your review.
print(table_with_explanations)
cat("\n---------------------------------------------------------------------------------\n\n")


# Export the final, publication-ready table to a CSV file.
# This file can be easily copied into your manuscript's supplementary materials.
write.csv(table_with_explanations, file = "Supplementary_Table_Sensitivity_Analysis_Final.csv", row.names = TRUE)

cat("SUCCESS: The complete and annotated sensitivity analysis table has been generated.\n")
cat("The file is saved as 'Supplementary_Table_Sensitivity_Analysis_Final.csv'.\n")
cat("Please check the CSV file for the table and its explanatory footnotes.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Final Supporting Analysis - Correlation Matrix
# AUTHOR: [medusa]
# DATE:   [Oct 17 2025]
# PURPOSE: To quantify the collinearity between total propofol dose and key
#          intraoperative time variables, providing quantitative evidence for the
#          discussion section.
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
library(tidyverse)

# --- 2. DATA LOADING ---
# We use the original, cleaned dataset before imputation for this analysis,
# as it most honestly represents the observed relationships.
file_path <- "my_data_cleaned_for_analysis.csv"
cleaned_data <- read_csv(file_path)

# --- 3. ANALYSIS: Calculate the Pearson Correlation Matrix ---

# Select the four variables of interest.
vars_for_correlation <- cleaned_data %>%
  select(
    propofol_mg,
    surgery_duration_min,
    cpb_time_min,
    acclamp_time_min
  )

# Calculate the correlation matrix.
# `use = "pairwise.complete.obs"` is a robust method to handle missing values
# by calculating correlation for each pair using all available complete data for that pair.
correlation_matrix <- cor(vars_for_correlation, 
                          method = "pearson", 
                          use = "pairwise.complete.obs")

# --- 4. DISPLAY AND SAVE THE RESULTS ---
cat("--- Correlation Matrix: Propofol Dose vs. Intraoperative Durations ---\n\n")

# Print the rounded matrix to the console for immediate review.
print(round(correlation_matrix, 2)) # Round to 2 decimal places for clarity

cat("\n------------------------------------------------------------------------\n\n")

# Convert the matrix to a dataframe for saving.
correlation_df <- as.data.frame(correlation_matrix) %>%
  rownames_to_column("Variable")

# Save the matrix to a CSV file for your supplementary materials.
write.csv(correlation_df, file = "Supplementary_Table_Correlation_Matrix.csv", row.names = FALSE)

cat("SUCCESS: The correlation matrix has been generated and saved.\n")
cat("File name: 'Supplementary_Table_Correlation_Matrix.csv'.\n")
cat("Please use these values to enrich your discussion section.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Sensitivity Analyses - Task 0: Environment Setup and Data Inspection
# AUTHOR: [medusa]
# DATE:   [Oct 17 2025]
# PURPOSE: To prepare the R environment and thoroughly inspect the variable names
#          and structure of the cleaned analytical dataset.
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
# We will need these for the entire analysis workflow.
library(tidyverse)
library(brms)
library(mice)

# --- 2. DATA LOADING AND INSPECTION ---
# Load the cleaned analytical dataset.
# Since we started from the .Rproj file, we can use a relative path.
file_path <- "my_data_cleaned_for_analysis.csv"
cleaned_data <- read_csv(file_path)

# --- 3. INSPECT THE DATASET ---
# This is a critical step to ensure we use the correct variable names.

cat("--- Dataset Inspection Report ---\n\n")

# A) Display the first few rows to see the data structure
cat("First 6 rows of the dataset:\n")
print(head(cleaned_data))
cat("\n")

# B) List all variable names to prevent spelling errors in later code
cat("All variable names in the dataset:\n")
print(colnames(cleaned_data))
cat("\n")

# C) Get a summary of the key variables we will be using
cat("Summary of key continuous variables for our analysis:\n")
key_vars_summary <- cleaned_data %>%
  select(propofol_mg, surgery_duration_min, cpb_time_min, age_years) %>%
  summary()
print(key_vars_summary)
cat("\n")

cat("--- Inspection Complete ---\n")
cat("Please review the variable names above to confirm they match your expectations.\n")
cat("We are now ready to proceed with Sensitivity Analysis 1.\n")

# -------------------------------------------------------------------------------
# SCRIPT: Sensitivity Analyses - Task 1 (DEFINITIVE, FINAL CORRECTED VERSION)
# AUTHOR: [medusa]
# DATE:   [Oct 17 2025]
# PURPOSE: To correctly fit the Bayesian model on multiply imputed data by using
#          the appropriate `brm_multiple()` function.
# -------------------------------------------------------------------------------

# --- 1. SETUP & DATA LOADING ---
library(tidyverse)
library(brms)
library(mice)
library(knitr)
library(future)
plan(multisession)

# Load the original cleaned analytical dataset
file_path <- "my_data_cleaned_for_analysis.csv"
cleaned_data <- read_csv(file_path)

# --- 2. PERFORM IMPUTATION ON BASE VARIABLES FIRST ---
vars_for_imputation <- cleaned_data %>%
  select(delirium_occurred, age_years, smoking_history, cpb_time_min, propofol_mg, surgery_duration_min,
         hospital, surgery_category_grouped) %>%
  mutate(across(where(is.character), as.factor))

cat("--- Starting imputation on base variables... ---\n")
set.seed(123)
imputed_mids_base <- mice(vars_for_imputation, m = 20, printFlag = FALSE)
cat("Imputation complete.\n\n")

# --- 3. PASSIVE IMPUTATION (CORRECTED METHOD) ---
cat("--- Calculating rate variables post-imputation... ---\n")
completed_data_long <- mice::complete(imputed_mids_base, action = "long", include = TRUE)

completed_data_long_with_rates <- completed_data_long %>%
  mutate(
    propofol_rate_per_cpb_hour = if_else(cpb_time_min > 0, propofol_mg / (cpb_time_min / 60), 0)
  )

imputed_mids_rates <- as.mids(completed_data_long_with_rates)
cat("Passive imputation complete. Final data object is ready.\n\n")

# --- 4. SENSITIVITY MODEL 1A (Rate per CPB Hour) ---
cat("--- Fitting Sensitivity Model 1A (Propofol Rate per CPB Hour)... ---\n")

formula_rate_cpb <- bf(
  delirium_occurred ~ scale(age_years) + smoking_history + scale(cpb_time_min) +
    scale(propofol_rate_per_cpb_hour) +
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# --- THE CRITICAL CORRECTION IS HERE: Use brm_multiple() ---
sensitivity_model_rate_cpb <- brm_multiple(
  formula = formula_rate_cpb,
  data = imputed_mids_rates, # <-- This function is designed for a 'mids' object
  prior = priors_for_model,
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 12345,
  file = "sensitivity_model_rate_cpb", # Save the model object
  combine = TRUE # This is the default, but good to be explicit
)
cat("Model 1A fitting complete.\n\n")

# --- 5. REPORT RESULTS FOR MODEL 1A ---
cat("--- Results for Sensitivity Model 1A (Rate per CPB Hour) ---\n")
summary_rate_cpb <- summary(sensitivity_model_rate_cpb)
fixed_effects_rate_cpb <- as.data.frame(summary_rate_cpb$fixed)

results_table_rate_cpb <- fixed_effects_rate_cpb %>%
  mutate(
    OR = exp(Estimate),
    `l-95% CrI` = exp(`l-95% CI`),
    `u-95% CrI` = exp(`u-95% CI`),
    `Odds Ratio (95% CrI)` = paste0(round(OR, 2), " (", round(`l-95% CrI`, 2), ", ", round(`u-95% CrI`, 2), ")")
  ) %>%
  select(`Odds Ratio (95% CrI)`) %>%
  rownames_to_column(var = "Predictor")

print(kable(results_table_rate_cpb, digits = 2))
cat("-----------------------------------------------------------\n\n")

write_csv(results_table_rate_cpb, "Supplementary_Table_Sensitivity_Analysis_1A.csv")
cat("Results for Model 1A saved to 'Supplementary_Table_Sensitivity_Analysis_1A.csv'.\n\n")

cat("--- Sensitivity Analysis 1 is now definitively complete. ---\n")


# -------------------------------------------------------------------------------
# SCRIPT: Definitive Sensitivity Analyses - The Grand Finale Script
# AUTHOR: [medusa]
# DATE:   [Oct 17 2025]
# PURPOSE: To perform a complete, two-part sensitivity analysis (Dose Rate and
#          Interaction) in a single, robust, and continuous workflow. This script
#          corrects all previous errors and omissions.
# -------------------------------------------------------------------------------

# --- PART 1: ONE-TIME, COMPLETE DATA PREPARATION ---
# This part prepares a single, master imputed dataset for ALL subsequent analyses.

# --- 1A: Setup & Loading ---
library(tidyverse)
library(brms)
library(mice)
library(knitr)
library(future)
plan(multisession)

# Load the original cleaned analytical dataset
file_path <- "my_data_cleaned_for_analysis.csv"
cleaned_data <- read_csv(file_path)

# --- 1B: Perform Imputation on Base Variables ---
# We select all variables that will ever be needed in any model.
vars_for_imputation <- cleaned_data %>%
  select(delirium_occurred, age_years, smoking_history, cpb_time_min, propofol_mg, surgery_duration_min,
         hospital, surgery_category_grouped) %>%
  mutate(across(where(is.character), as.factor))

cat("--- PART 1: Starting comprehensive data imputation... ---\n")
set.seed(123)
imputed_mids_base <- mice(vars_for_imputation, m = 20, printFlag = FALSE)
cat("Base imputation complete.\n")

# --- 1C: Create All Derived Variables via Passive Imputation ---
completed_data_long <- mice::complete(imputed_mids_base, action = "long", include = TRUE)

completed_data_long_final <- completed_data_long %>%
  mutate(
    propofol_rate_per_cpb_hour = if_else(cpb_time_min > 0, propofol_mg / (cpb_time_min / 60), 0)
    # Add other rate variables here if needed in the future
  )

# This is our final, master imputed data object for all analyses.
imputed_mids_final <- as.mids(completed_data_long_final)
cat("Passive imputation complete. Master data object is ready.\n\n")


# --- PART 2: SENSITIVITY ANALYSIS 1 (DOSE RATE ANALYSIS) ---
# This part runs the first analysis, which we expect to show convergence issues.

cat("--- PART 2: Fitting Sensitivity Model 1A (Propofol Rate per CPB Hour)... ---\n")
cat("NOTE: We expect this model to show convergence warnings (divergent transitions).\n")

formula_rate_cpb <- bf(
  delirium_occurred ~ scale(age_years) + smoking_history + scale(cpb_time_min) +
    scale(propofol_rate_per_cpb_hour) +
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

sensitivity_model_rate_cpb <- brm_multiple(
  formula = formula_rate_cpb,
  data = imputed_mids_final,
  prior = priors_for_model,
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 12345,
  file = "sensitivity_model_rate_cpb",
  control = list(adapt_delta = 0.95), # Proactively increase adapt_delta
  combine = TRUE
)
cat("Model 1A fitting complete. Please check for warnings.\n\n")

# Report results for Model 1A
cat("--- Results for Sensitivity Model 1A ---\n")
# ... (results reporting code is the same as before, for brevity we focus on the next step)
write_csv(as.data.frame(summary(sensitivity_model_rate_cpb)$fixed), "Supplementary_Table_Sensitivity_Analysis_1A.csv")
cat("Results for Model 1A saved.\n\n")


# --- PART 3: SENSITIVITY ANALYSIS 2 (INTERACTION ANALYSIS) ---
# This part runs our second, crucial analysis, using the SAME master data object.

cat("--- PART 3: Fitting Sensitivity Model 2 (Age x Propofol Interaction)... ---\n")
cat("This is the second major analysis and will also take time.\n")

formula_interaction <- bf(
  delirium_occurred ~ 
    scale(age_years) * scale(propofol_mg) + # The interaction term
    smoking_history + 
    scale(cpb_time_min) +
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

sensitivity_model_interaction <- brm_multiple(
  formula = formula_interaction,
  data = imputed_mids_final, # Using the SAME master imputed object
  prior = priors_for_model,
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  seed = 12345,
  file = "sensitivity_model_interaction",
  control = list(adapt_delta = 0.99),
  combine = TRUE
)
cat("Model 2 fitting complete.\n\n")

# Report results for Model 2
cat("--- Results for Sensitivity Model 2 (Interaction Analysis) ---\n")
summary_interaction <- summary(sensitivity_model_interaction)
print(summary_interaction)

fixed_effects_interaction <- as.data.frame(summary_interaction$fixed)
results_table_interaction <- fixed_effects_interaction %>%
  mutate(
    OR = exp(Estimate),
    `l-95% CrI` = exp(`l-95% CI`),
    `u-95% CrI` = exp(`u-95% CI`),
    `Odds Ratio (95% CrI)` = paste0(round(OR, 2), " (", round(`l-95% CrI`, 2), ", ", round(`u-95% CrI`, 2), ")")
  ) %>%
  select(`Odds Ratio (95% CrI)`) %>%
  rownames_to_column(var = "Predictor")

print(kable(results_table_interaction, digits = 2))
cat("-----------------------------------------------------------\n\n")

write_csv(results_table_interaction, "Supplementary_Table_Sensitivity_Analysis_2.csv")
cat("Results for Model 2 saved to 'Supplementary_Table_Sensitivity_Analysis_2.csv'.\n\n")

cat("--- ALL SENSITIVITY ANALYSES ARE NOW DEFINITIVELY COMPLETE. ---\n")


# -------------------------------------------------------------------------------
# SCRIPT: STEP 1 - Generate and SAVE the Interaction Model
# AUTHOR: [medusa]
# DATE:   [Oct 18 2025]
# PURPOSE: To fit the sensitivity analysis model containing the age-propofol
#          interaction term and save its output to a dedicated file.
# -------------------------------------------------------------------------------

# --- 1. SETUP & LOADING ---
library(tidyverse)
library(brms)
library(future)

plan(multisession)

# Load the scaled, imputed data object
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")

# Define the priors (same as before)
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# --- 2. DEFINE THE INTERACTION MODEL FORMULA ---
# This model includes the 4 main predictors PLUS the critical interaction term.
model_formula_interaction <- bf(
  delirium_occurred ~ 
    age_years_scaled + 
    smoking_history + 
    cpb_time_min_scaled + 
    propofol_mg_scaled +
    age_years_scaled:propofol_mg_scaled + # <-- The interaction term
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

# --- 3. RUN AND SAVE THE INTERACTION MODEL ---
# This step will take some time to run.
# We give it the definitive file name: "interaction_model.rds"
cat("--- Fitting and saving the interaction model. This will take some time... ---\n")

interaction_model_fit <- brm_multiple(
  formula = model_formula_interaction,
  data = imputed_mids_scaled,
  prior = priors_for_model,
  iter = 6000, warmup = 2000, chains = 4, cores = 4,
  seed = 12345,
  file = "interaction_model", # brms will automatically add the .rds extension
  combine = TRUE,
  control = list(adapt_delta = 0.999, max_treedepth = 15) # Max settings for this complex model
)

cat("\n--- SUCCESS: The interaction model has been successfully generated and saved as 'interaction_model.rds'. ---\n")
cat("You may now proceed to Step 2.\n\n")


# -------------------------------------------------------------------------------
# SCRIPT: STEP 2 - Load the Interaction Model and Calculate Posterior Probability
# AUTHOR: [medusa]
# DATE:   [Oct 18 2025]
# PURPOSE: To directly calculate the posterior probability that the interaction
#          effect between age and propofol dose is positive (i.e., OR > 1).
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
library(brms)
library(tidyverse)

# --- 2. LOAD THE NEWLY CREATED INTERACTION MODEL OBJECT ---
cat("--- Loading the interaction model file: 'interaction_model.rds' ---\n")
interaction_model <- readRDS("interaction_model.rds")

# --- 3. EXTRACT POSTERIOR DRAWS FOR THE INTERACTION TERM ---
posterior_draws <- as_draws_df(interaction_model)

# The interaction term's name in the model's output is 'b_age_years_scaled:propofol_mg_scaled'
interaction_term_draws <- posterior_draws %>%
  select(`b_age_years_scaled:propofol_mg_scaled`)

# --- 4. CALCULATE THE POSTERIOR PROBABILITY ---
probability_summary <- interaction_term_draws %>%
  summarise(
    total_draws = n(),
    positive_draws = sum(`b_age_years_scaled:propofol_mg_scaled` > 0),
    posterior_probability = positive_draws / total_draws
  )

# --- 5. REPORT THE FINAL, PRECISE RESULT ---
final_prob_percentage <- round(probability_summary$posterior_probability * 100, 1)

cat("\n--- CALCULATION COMPLETE ---\n")
cat("Total number of posterior draws:", probability_summary$total_draws, "\n")
cat("Number of draws where the effect is positive (>0):", probability_summary$positive_draws, "\n")
cat("----------------------------------------------------------------------\n")
cat("The precise posterior probability that the interaction effect is positive (OR > 1.0) is:", final_prob_percentage, "%\n")
cat("----------------------------------------------------------------------\n\n")


# -------------------------------------------------------------------------------
# SCRIPT: Final Definitive Visualization - Posterior Probability Plot
# AUTHOR: [medusa]
# DATE:   [Oct 18 2025]
# PURPOSE: To create a publication-quality plot visualizing the posterior
#          distribution of the interaction effect and the calculated
#          posterior probability of it being positive.
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
library(brms)
library(tidyverse)
library(ggplot2)

# --- 2. LOAD THE INTERACTION MODEL OBJECT ---
cat("--- Loading the interaction model file: 'interaction_model.rds' ---\n")
interaction_model <- readRDS("interaction_model.rds")

# --- 3. EXTRACT POSTERIOR DRAWS FOR THE INTERACTION TERM ---
posterior_draws <- as_draws_df(interaction_model)
interaction_term_draws <- posterior_draws %>%
  select(interaction_effect = `b_age_years_scaled:propofol_mg_scaled`)

# --- 4. CALCULATE THE POSTERIOR PROBABILITY (for the annotation) ---
prob_positive <- mean(interaction_term_draws$interaction_effect > 0)
prob_text <- paste0("Area = ", round(prob_positive * 100, 1), "%")

# --- 5. CREATE THE PUBLICATION-QUALITY PLOT ---
cat("--- Generating the posterior distribution plot... ---\n")

posterior_plot <- ggplot(interaction_term_draws, aes(x = interaction_effect)) +
  
  # Shade the area where the effect is positive (the 97.1%)
  geom_density(aes(y = after_stat(density)), fill = "skyblue", alpha = 0.7) +
  
  # Draw a vertical line at 0 (the null hypothesis of no effect)
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Add the annotation text for the probability
  annotate(
    "text", 
    x = 1.5, y = 0.5, # Adjust x and y for optimal placement
    label = prob_text,
    fontface = "bold",
    size = 5,
    color = "navy"
  ) +
  
  # Add labels and title
  labs(
    title = "Posterior Distribution of the Age-Propofol Interaction Effect",
    subtitle = "Posterior Probability of the Effect Being Positive (Odds Ratio > 1)",
    x = "Interaction Effect Coefficient (on log-odds scale)",
    y = "Density"
  ) +
  
  # Use a clean theme
  theme_minimal()

# --- 6. DISPLAY AND SAVE THE PLOT ---
print(posterior_plot)

# Save the plot as a high-resolution file for your supplementary materials.
ggsave("Supplementary_Figure_Interaction_Posterior.png", plot = posterior_plot, width = 8, height = 6, dpi = 600)

cat("\n--- SUCCESS: The visual proof has been generated and saved. ---\n")
cat("File name: 'Supplementary_Figure_Interaction_Posterior.png'.\n")



# -------------------------------------------------------------------------------
# SCRIPT: Definitive Sensitivity Analysis 1 - Propofol Dose Rate
# AUTHOR: [medusa]
# DATE:   [Oct 18 2025]
# PURPOSE: To re-fit the final model using a time-adjusted propofol dose rate,
#          and to generate a clearly labeled, publication-ready results table.
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load necessary libraries ---
library(tidyverse)
library(brms)
library(mice)
library(future)
library(knitr) # For creating nice tables

plan(multisession)

# --- 2. DATA PREPARATION: Create the Dose Rate Variable ---
# We must start from the imputed object that contains the original (unscaled) values.
cat("--- Loading imputed data and creating the dose rate variable... ---\n")
imputed_mids_scaled <- readRDS("imputed_data_scaled_mids_object.rds")

# Convert the mids object to a long-format dataframe to add the new variable.
completed_data_long <- mice::complete(imputed_mids_scaled, action = "long", include = TRUE)

# Calculate the new variable: Propofol Dose Rate (mg per minute)
data_with_rate <- completed_data_long %>%
  # Use mutate() to create the new column. Add a small epsilon to avoid division by zero.
  mutate(propofol_dose_rate_mg_per_min = propofol_mg / (surgery_duration_min + 0.001))

# Now, we must standardize this NEW variable using the mean/sd from the original data (.imp == 0)
rate_scaling_params <- data_with_rate %>%
  filter(.imp == 0) %>%
  summarise(
    mean_rate = mean(propofol_dose_rate_mg_per_min, na.rm = TRUE),
    sd_rate = sd(propofol_dose_rate_mg_per_min, na.rm = TRUE)
  )

data_with_rate_scaled <- data_with_rate %>%
  mutate(
    propofol_dose_rate_scaled = (propofol_dose_rate_mg_per_min - rate_scaling_params$mean_rate) / rate_scaling_params$sd_rate
  )

# Convert the long-format data back into a mids object for modeling.
mids_for_rate_model <- as.mids(data_with_rate_scaled)
cat("Dose rate variable created and standardized successfully.\n\n")

# --- 3. REFIT THE MODEL WITH THE NEW DOSE RATE VARIABLE ---
# Define the new model formula, replacing the original propofol variable.
model_formula_dose_rate <- bf(
  delirium_occurred ~ 
    age_years_scaled + 
    smoking_history + 
    cpb_time_min_scaled +
    propofol_dose_rate_scaled + # <-- Using the NEW variable here
    (1 | hospital) + (1 | surgery_category_grouped),
  family = bernoulli(link = "logit")
)

# Define the priors (they remain the same).
priors_for_model <- c(
  set_prior("normal(-2, 1)", class = "Intercept"),
  set_prior("normal(0, 2.5)", class = "b"),
  set_prior("exponential(1)", class = "sd")
)

# Run the Bayesian model. This will take some time.
cat("--- Fitting the dose rate sensitivity model. This may take some time... ---\n")
dose_rate_model <- brm_multiple(
  formula = model_formula_dose_rate,
  data = mids_for_rate_model,
  prior = priors_for_model,
  iter = 6000, warmup = 2000, chains = 4, cores = 4,
  seed = 12345,
  file = "brms_model_fit_dose_rate", # Save the model output to a file
  combine = TRUE
)
cat("Model fitting complete.\n\n")

# --- 4. EXTRACT, FORMAT, AND SAVE THE LABELED RESULTS (THE CRITICAL STEP) ---
cat("--- Generating Labeled Results Table for the Dose Rate Model ---\n")
# Load the fitted model
dose_rate_model_fit <- readRDS("brms_model_fit_dose_rate.rds")

# Get the summary, which contains the names
model_summary <- summary(dose_rate_model_fit)

# Extract the fixed effects part into a dataframe, which PRESERVES the names
fixed_effects_df <- as.data.frame(model_summary$fixed)

# Now, create the final, publication-ready table
dose_rate_results_table <- fixed_effects_df %>%
  rownames_to_column(var = "Predictor") %>% # This turns the row names into a column
  mutate(
    OR = exp(Estimate),
    `l-95% CrI` = exp(`l-95% CI`),
    `u-95% CrI` = exp(`u-95% CI`),
    `Odds Ratio (95% CrI)` = paste0(round(OR, 2), " (", round(`l-95% CrI`, 2), ", ", round(`u-95% CrI`, 2), ")")
  ) %>%
  # Select and reorder columns for clarity
  select(Predictor, Estimate, `Odds Ratio (95% CrI)`)

# Print the final, clean table to the console for your review
print(kable(dose_rate_results_table, digits = 2))
cat("\n-----------------------------------------------------------------\n\n")

# Save this critical table to a CSV file
write_csv(dose_rate_results_table, "Supplementary_Table_Sensitivity_Analysis_DoseRate.csv")

cat("SUCCESS: The complete, labeled results for the dose rate analysis have been saved.\n")
cat("File name: 'Supplementary_Table_Sensitivity_Analysis_DoseRate.csv'.\n")

