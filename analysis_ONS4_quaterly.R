#~############################################################################~#
# Load data ----
#~############################################################################~#

# Load all the sheets in one go
my_sheets_ONS4 <- c(
  "1.1_Life_satisfaction", 
  "1.2_Worthwhile", 
  "1.3_Happiness", 
  "1.4_Feeling_anxious"
)

dat_ONS4 <- do.call(rbind, lapply(my_sheets_ONS4, function(sheet_name) {
  load_quarter_sheet(quaterly_data_path, sheet = sheet_name)
}))

# Add different titles for the different variables
dat_ONS4 <- dat_ONS4 %>% 
  mutate(descriptor = case_when(
  variable == "Life satisfaction" ~ "Low life satisfaction",
  variable == "Worthwhile" ~ "Low worthwhileness",
  variable == "Happiness" ~ "Low happiness",
  variable == "Feeling anxious" ~ "High anxiety",
  TRUE ~ as.character(variable) # Default case to keep original variable if no match
))

#~############################################################################~#
# Analysis ----
#~############################################################################~#

#~=======================================================~=
## Insights over time ----
#~=======================================================~=

# Prepare data
dat_ONS4_time <- dat_ONS4 %>% filter(str_detect(condition, "\\(Q"))
most_recent_time <- tail(unique(dat_ONS4_time$condition), 1)

# Obtain recent insights
ONS4_time_insights <- dat_ONS4_time %>% filter(
  condition == most_recent_time |
    str_detect(condition, "\\[L\\]") |
    str_detect(condition, "\\[S\\]")
)

# Write the insights
write_csv(
  ONS4_time_insights,
  paste0(output_path, "ONS4_time_insights.csv")
)


#~=======================================================~=
## Age ----
#~=======================================================~=

# Get the age data
dat_ONS4_age <- dat_ONS4 %>% filter(str_detect(condition, "Aged"))

# Make the figures
p_age_1 <- make_age_trend_plot(dat_ONS4_age, "Life satisfaction", "Low life satisfaction", "#3ab7b9")
p_age_2 <- make_age_trend_plot(dat_ONS4_age, "Worthwhile", "Low worthwhileness", "#f79321")
p_age_3 <- make_age_trend_plot(dat_ONS4_age, "Happiness", "Low happiness", "#8CC43B")
p_age_4 <- make_age_trend_plot(dat_ONS4_age, "Feeling anxious", "High anxiety", "#DB78A8")
p_age <- (p_age_1 + p_age_2) / (p_age_3 + p_age_4); p_age

# Save the figures
ggsave(
  filename = paste0(output_path, "ONS4_age.png"),
  plot = p_age,
  width = 10, height = 8,
  dpi = my_dpi
)

#~=======================================================~=
## Gender ----
#~=======================================================~=

# Get the gender data
dat_ONS4_gender <- dat_ONS4 %>% filter(condition %in% c("Male", "Female"))

# Make the figures
p_gender <- make_gender_plot(dat_ONS4_gender, "UK National Wellbeing Indicator")
p_gender

# Save the figures
ggsave(
  filename = paste0(output_path, "ONS4_gender.png"),
  plot = p_gender,
  width = 10, height = 8,
  dpi = my_dpi
)

