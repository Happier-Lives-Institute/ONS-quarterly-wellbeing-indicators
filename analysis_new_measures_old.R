#~############################################################################~#
# Load data ----
#~############################################################################~#

# Load all the sheets in one go
my_sheets_NW <- c(
  "1.5_Hope_for_future",
  "1.6_Fair_treatment",
  "2.2_Social_relationships",
  "4.1_Satisfac'n_with_time_use",
  "5.2_Satisfac'n_with_local_area"
)

dat_NW <- do.call(bind_rows, lapply(my_sheets_NW, function(sheet_name) {
  load_quarter_sheet(quaterly_data_path, sheet = sheet_name)
}))

# Add different titles for the different variables
unique(dat_NW$variable)
dat_NW <- dat_NW %>% 
  mutate(descriptor = case_when(
  variable == "Hope for future" ~ "High hope for the future",
  variable == "Fair treatment" ~ "Unfairly treated by society",
  variable == "Social relationships" ~ "Satisfaction with social relationships",
  TRUE ~ as.character(variable) # Default case to keep original variable if no match
))

#~############################################################################~#
# Analysis ----
#~############################################################################~#

#~=======================================================~=
## Insights over time ----
#~=======================================================~=

# Prepare data
dat_NW_time <- dat_NW %>% filter(
  str_detect(condition, "\\(Q") |
    str_detect(condition, "202") 
)
most_recent_time <- tail(unique(dat_NW_time$condition), 1)

# Obtain recent insights
NW_time_insights <- dat_NW_time %>% filter(
  condition == most_recent_time |
    str_detect(condition, "\\[L\\]") |
    str_detect(condition, "\\[S\\]")
)

# Write the insights
write_csv(
  NW_time_insights,
  paste0(output_path, "NW_time_insights_old.csv")
)

#~=======================================================~=
## Age ----
#~=======================================================~=

# Get the age data
dat_NW_age <- dat_NW %>% filter(str_detect(condition, "Aged"))

# Make the figures
data.frame(
  unique(dat_NW$variable),
  unique(dat_NW$descriptor)
)
p_age_1 <- make_age_trend_plot(dat_NW_age, "Hope for future", "High hope for the future", "#3ab7b9")
p_age_2 <- make_age_trend_plot(dat_NW_age, "Fair treatment", "Unfairly treated by society", "#f79321")
p_age_3 <- make_age_trend_plot(dat_NW_age, "Social relationships", "Satisfaction with social relationships", "#8CC43B")
p_age_4 <- make_age_trend_plot(dat_NW_age, "Satisfaction with time use", "Satisfaction with time use", "#888888")
p_age_5 <- make_age_trend_plot(dat_NW_age, "Satisfaction with local area", "Satisfaction with local area", "#DB78A8")
p_age <- (p_age_1 + p_age_2) / (p_age_3 + p_age_4) / (p_age_5 + plot_spacer()); p_age

# Save the figures
ggsave(
  filename = paste0(output_path, "NW_age.png"),
  plot = p_age,
  width = 10, height = 12,
  dpi = my_dpi
)

#~=======================================================~=
## Gender ----
#~=======================================================~=

# Get the gender data
dat_NW_gender <- dat_NW %>% filter(condition %in% c("Male", "Female"))

# Make the figures
p_gender <- make_gender_plot(dat_NW_gender, "UK National Wellbeing Indicator")
p_gender

# Save the figures
ggsave(
  filename = paste0(output_path, "NW_gender.png"),
  plot = p_gender,
  width = 10, height = 8,
  dpi = my_dpi
)
