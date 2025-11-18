# -------------------------------------------------------------------------------
# SCRIPT: app.R (Shiny Web Application) - FINAL DEFINITIVE VERSION (Baseline Model Only)
# PROJECT: Postoperative Delirium Risk Calculator
# AUTHOR: [Your Name]
# DATE:   [Current Date]
# PURPOSE: A clean, robust, and intuitive clinical decision support tool to estimate
#          the baseline risk of postoperative delirium in adult cardiac surgery patients,
#          based on the final, converged Bayesian hierarchical model.
# -------------------------------------------------------------------------------

# --- 1. SETUP: Load all necessary libraries ---
library(shiny)
library(brms)
library(tidyverse)
library(mice)

# --- 2. PRE-COMPUTATION BLOCK: Load model and parameters once at startup ---
# This optimization ensures the app is fast and efficient.

cat("Loading model and pre-computing scaling parameters...\n")

# Load the final, converged baseline model (our champion model)
final_base_model <- readRDS("brms_model_fit_final_robust.rds")

# Load original data to get the scaling parameters (mean and sd).
# This is CRITICAL to ensure new data is transformed in exactly the same way.
imputed_mids_scaled_obj <- readRDS("imputed_data_scaled_mids_object.rds")
original_data <- mice::complete(imputed_mids_scaled_obj, 0) # 0 is the original data

continuous_vars_in_model <- c("age_years", "cpb_time_min", "propofol_mg")
scaling_params_base <- original_data %>%
  select(all_of(continuous_vars_in_model)) %>%
  summarise(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE))

cat("Application is ready.\n")

# --- 3. USER INTERFACE (UI) DEFINITION ---
# This controls the layout and appearance of the web page.
ui <- fluidPage(
  
  # Application Title
  titlePanel("Postoperative Delirium Risk Calculator for Adult Cardiac Surgery"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      h4("Patient & Procedure Information"),
      p(tags$i("Enter pre- and intra-operative data to calculate the baseline risk of delirium.")),
      
      numericInput("age_input", 
                   label = "Age (years):", 
                   value = 60, min = 18, max = 100),
      
      radioButtons("smoking_input", 
                   label = "Smoking History:",
                   choices = c("No", "Yes"), selected = "No"),
      
      numericInput("cpb_input",
                   label = "Cardiopulmonary Bypass (CPB) Time (minutes):",
                   value = 120, min = 0),
      
      numericInput("propofol_input",
                   label = "Total Propofol Dose (mg):",
                   value = 100, min = 0),
      
      # The button to trigger the calculation
      actionButton("calculate_btn", "Calculate Risk", class = "btn-primary", style="width:100%; font-size: 120%;")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      h3("Predicted Risk Profile"),
      
      # The main output area
      uiOutput("prediction_results"),
      
      hr(), # A horizontal line for separation
      
      # --- MANDATORY DISCLAIMER ---
      tags$div(
        class = "alert alert-warning", # Bootstrap class for styling
        tags$h5("Disclaimer"),
        tags$p(
          "This tool is intended for educational and informational purposes only and is not a substitute for professional clinical judgment. The predictions are derived from a statistical model based on a specific patient cohort and carry inherent uncertainty. Treatment decisions should not be based solely on the output of this calculator. The authors and their institutions assume no liability for any actions taken based on the use of this tool."
        )
      )
    )
  )
)

# --- 4. SERVER LOGIC DEFINITION ---
# This is the "brain" of the app, where all calculations happen.
server <- function(input, output) {
  
  # This reactive expression is triggered when the button is pressed
  observeEvent(input$calculate_btn, {
    
    # A) Ensure all inputs are provided before calculating
    req(input$age_input, input$cpb_input, input$propofol_input)
    
    # B) Create a new dataframe with the user's inputs
    new_patient_data <- tibble(
      age_years = input$age_input,
      smoking_history = factor(input$smoking_input, levels = c("No", "Yes")),
      cpb_time_min = input$cpb_input,
      propofol_mg = input$propofol_input,
      # Add placeholder values for the random effects; brms will handle this
      hospital = 1, # Placeholder
      surgery_category_grouped = "Isolated CABG" # Placeholder
    )
    
    # C) Apply the EXACT same standardization as the training data
    new_patient_data_scaled <- new_patient_data %>%
      mutate(
        age_years_scaled = (age_years - scaling_params_base$age_years_mean) / scaling_params_base$age_years_sd,
        cpb_time_min_scaled = (cpb_time_min - scaling_params_base$cpb_time_min_mean) / scaling_params_base$cpb_time_min_sd,
        propofol_mg_scaled = (propofol_mg - scaling_params_base$propofol_mg_mean) / scaling_params_base$propofol_mg_sd
      )
    
    # D) Generate the prediction using the loaded brms model
    predicted_probs <- posterior_epred(
      final_base_model,
      newdata = new_patient_data_scaled,
      allow_new_levels = TRUE,
      re.form = NA # Use only fixed effects for a generalizable prediction
    )
    
    # Summarize the posterior distribution to a single probability (median)
    risk_probability <- median(predicted_probs)
    
    # E) Stratify risk and define clinical suggestions
    if (risk_probability < 0.10) { # Low Risk threshold < 10%
      risk_level <- "Low Risk"
      risk_color <- "green"
      clinical_suggestions <- "Standard postoperative monitoring is recommended. Continue routine care."
    } else if (risk_probability >= 0.10 && risk_probability < 0.30) { # Moderate Risk 10-30%
      risk_level <- "Moderate Risk"
      risk_color <- "orange"
      clinical_suggestions <- "Consider enhanced surveillance. Proactively manage pain, sleep, and hydration. Encourage early mobilization and family engagement. Review medication for deliriogenic potential."
    } else { # High Risk threshold >= 30%
      risk_level <- "High Risk"
      risk_color <- "red"
      clinical_suggestions <- "High alert. Implement a multicomponent delirium prevention bundle immediately. Consider geriatric or psychiatric consultation. Minimize psychoactive medications and prioritize non-pharmacological interventions. Ensure frequent re-orientation."
    }
    
    # F) Render the final output to the UI
    output$prediction_results <- renderUI({
      HTML(paste0(
        "<p style='font-size:18px;'>The predicted probability of postoperative delirium is:</p>",
        "<p style='font-size:36px; font-weight:bold; color:", risk_color, ";'>", round(risk_probability * 100, 1), "%</p>",
        "<hr>",
        "<p style='font-size:18px;'>Risk Stratification:</p>",
        "<p style='font-size:24px; font-weight:bold; color:", risk_color, ";'>", risk_level, "</p>",
        "<hr>",
        "<p style='font-size:18px;'>Suggested Clinical Considerations:</p>",
        "<p style='font-size:16px;'>", clinical_suggestions, "</p>"
      ))
    })
  })
}

# --- 5. RUN THE APPLICATION ---
shinyApp(ui = ui, server = server)