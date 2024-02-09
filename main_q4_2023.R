#~############################################################################~#
# Preparation ----
#~############################################################################~#

# Load dependencies
source("dependencies.R")

# Load the colour palette
source("colour_palette.R")

## General parameters
my_dpi <- 1200 # change this parameter for resolution

# Change these as you change quarters
quaterly_data_path <- "data/ukmeasuresofnationalwellbeingnov2023.xlsx"
yearly_data_path <- "data/aprtojune23nonseasonallyadjusted.xlsx"
output_path <- "q4_2023/"

#~############################################################################~#
# Run the different analyses ----
#~############################################################################~#

# Check if the output_path exists, otherwise create directory
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

## Get a list of variables
# Get the list of sheets
sheet_names <- readxl::excel_sheets(quaterly_data_path)
# Write the sheet names to a text file
writeLines(sheet_names, paste0(output_path, "sheet_names.txt"))

## Analysis of ONS4

# quarter
my_sheets_ONS4 <- c(
  "1.1_Life_satisfaction", 
  "1.2_Worthwhile", 
  "1.3_Happiness", 
  "1.4_Feeling_anxious"
)
source("analysis_ONS4_quaterly.R")

# yearly
my_sheets_ONS4 <- c(
  "2 Life satisfaction (UK)", 
  "4 Worthwhile (UK)", 
  "6 Happiness (UK)", 
  "8 Anxiety (UK)"
)
source("analysis_ONS4_yearly.R")

## Quarterly analysis of new measures

# Old version of the analysis
# source("analysis_new_measures_old.R")

# More detailed analysis
# You need to clean and select all the data
# Load all the sheets in one go
my_sheets_NW <-  c(
  # "1.1_Life_satisfaction",
  # "1.2_Worthwhile",
  # "1.3_Happiness",
  # "1.4_Feeling_anxious",
  "1.5_Hope_for_future",
  "1.6_Fair_treatment",
  # "2.1_Unhappy_relationships",
  "2.2_Social_relationships",
  # "2.3_People_to_rely_on",
  "2.4_Loneliness",
  "2.5_Local_community_integration",
  # "2.6_Trust_in_others",
  # "3.1_Healthy_life_expectancy",
  "3.2_Satisfac'n_with_health",
  # "3.3_Physical_health_cond'ns",
  # "3.4_Depression_or_anxiety",
  "3.5_Satisfac'n_with_healthcare",
  "4.1_Satisfac'n_with_time_use",
  "4.2_Satisfac'n_with_job",
  # "4.3_Time_spent_on_unpaid_work",
  # "4.4_Volunteering",
  # "4.5_Engagement_arts_and_culture",
  # "4.6_Sports_participation",
  # "4.7_Visited_nature",
  "5.1_Satisfac'n_with_accomm",
  "5.2_Satisfac'n_with_local_area",
  # "5.3_Belonging_to_neighbourhood",
  "5.4_Digital_exclusion",
  # "5.5_Crime",
  # "5.6_Feeling_safe",
  # "6.1_Median_household_income",
  # "6.2_Median_household_wealth",
  # "6.3_Relative_low-income_h'hold",
  # "6.4_Household_income_inequality",
  # "6.5_Gender_pay_gap",
  # "6.6_Difficulty_managing_fin'ly",
  # "7.1_NEET",
  # "7.2_No_qualifications",
  # "7.3_A-level_or_equiv_quals",
  # "7.4_Human_capital",
  "7.5_Satisfact'n_with_edu_system",
  # "8.1_Unemployment_rate",
  # "8.2_Inflation_rate",
  # "8.3_Public_sector_net_debt",
  # "9.1_Voter_turnout",
  # "9.2_Trust_in_UK_government",
  # "9.3_Voice",
  "9.4_Satisfac'n_with_police",
  "9.5_Satisfact'n_courts_legal"#,
  # "10.1_Greenhouse_gas_emissions",
  # "10.2_Renewable_energy",
  # "10.3_Household_recycling",
  # "10.4_Protected_areas",
  # "10.5_Priority_species",
  # "10.6_Air_pollution",
  # "10.7_Surface_water_status",
  # "10.8_Pro_env_lifestyle"
)

dat_NW <- do.call(bind_rows, lapply(my_sheets_NW, function(sheet_name) {
  print(sheet_name)
  load_quarter_sheet(quaterly_data_path, sheet = sheet_name)
}))

# Add different titles for the different variables
unique(dat_NW$variable)
dat_NW <- dat_NW %>% 
  mutate(
    variable = ifelse(variable == "Social relationships", "Satisfaction with social relationships", variable), 
    descriptor = case_when(
      variable == "Hope for future" ~ "Hopeful about their future",
      variable == "Fair treatment" ~ "Very unfairly or somewhat unfairly treated by society",
      variable == "Satisfaction with social relationships" ~ "Fairly or very satisfied with social relationships",
      variable == "Loneliness" ~ "Feel lonely often or always",
      variable == "Local community integration" ~ "Strongly agree or agree that different backgrounds get on well together in local area",
      variable == "Satisfaction with health" ~ "Fairly or very satisfied with health",
      variable == "Satisfaction with healthcare" ~ "Tend to be satisfed with the healthcare system",
      variable == "Satisfaction with time use" ~ "Fairly or very satisfied with how they spend their time in a typical week",
      variable == "Satisfaction with job" ~ "Fairly or very satisfied with main job",
      variable == "Satisfaction with accommodation" ~ "Fairly or very satisfied with accommodation",
      variable == "Satisfaction with local area" ~ "Fairly or very satisfied with local area",
      variable == "Digital exclusion" ~ "Have not use the internet in the last 3 months (or have never used the internet)",
      variable == "Satisfaction with education system" ~ "Tend to be satisfied with education system",
      variable == "Satisfaction with police" ~ "Tend to be satisfied with the police",
      variable == "Satisfaction courts legal" ~ "Tend to be satisfied with courts and legal system",
      TRUE ~ as.character(variable) # Default case to keep original variable if no match
    )
  )

source("analysis_new_measures.R")

