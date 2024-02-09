#~############################################################################~#
# Load data ----
#~############################################################################~#

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

# Split the data into two sets
dat_NW_set_1 <- dat_NW %>% filter(!str_detect(variable, "Satisfaction"))
dat_NW_set_SW <- dat_NW %>% filter(str_detect(variable, "Satisfaction"))

#~############################################################################~#
# Analysis ----
#~############################################################################~#

#~=======================================================~=
## Insights over time ----
#~=======================================================~=

# Prepare data
dat_NW_time <- dat_NW %>% filter(
  str_detect(condition, "\\(Q") |
    str_detect(condition, "202") |
    str_detect(condition, "201") 
)

# Obtain recent insights
# most_recent_time <- tail(unique(dat_NW_time$condition), 1)
# NW_time_insights <- dat_NW_time %>% filter(
#   condition == most_recent_time |
#     str_detect(condition, "\\[L\\]") |
#     str_detect(condition, "\\[S\\]")
# )

# These are so new that the only variable we need to reduce for readability 
# is the loneliness one.
dat_NW_time_loneliness <- dat_NW_time %>% filter(variable == "Loneliness")
NW_time_insights <- rbind(
  dat_NW_time %>% filter(variable != "Loneliness"),
  dat_NW_time_loneliness %>% filter(
    condition == tail(unique(dat_NW_time_loneliness$condition), 1) |
      str_detect(condition, "\\[L\\]") |
      str_detect(condition, "\\[S\\]")
  )
)

# Write the insights
write_csv(
  NW_time_insights,
  paste0(output_path, "NW_time_insights.csv")
)

#~=======================================================~=
## Age ----
#~=======================================================~=

# Custom labeller function
# Need to fit the strips of the panels
wrap_label <- function(labels) {
  sapply(labels, function(label) {
    wrapped_label <- str_wrap(label, width = 40) # Adjust width as needed
    wrapped_label
  })
}

# First general set
p_age_1 <- dat_NW_set_1 %>% filter(str_detect(condition, "Aged")) %>% 
  ggplot(aes(x=condition, y=estimate, group=descriptor, color = descriptor)) +
  geom_line(linewidth=1.75) +
  scale_y_continuous(
    # limits = c(
    #   0, 
    #   100), 
    # breaks = seq(0, 100, 25), 
    labels = function(x) paste0(x, "%"),
    # expand = c(0, 0)
  ) +
  facet_wrap(~descriptor, scales = "free", labeller = as_labeller(wrap_label)) +
  cowplot::theme_cowplot() +
  labs(x = "", y = "", color = "Wellbeing indicator") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate x labels 90°
    # axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5), # Rotate x labels
    panel.grid.major = element_line(colour = "lightgrey"), # Add back major grid lines
    panel.grid.minor = element_line(colour = "lightgrey"), # Add back minor grid lines
    # Reinsert white background for the plot
    panel.background = element_rect(fill = "white", colour = "grey"),
    plot.background = element_rect(fill = "white", colour = NA),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "none",  # This line removes the legend
  ) +
  scale_colour_manual(values = extended_palette); p_age_1

# Save the figures
ggsave(
  filename = paste0(output_path, "NW_age_set_1.png"),
  plot = p_age_1,
  width = 10, height = 8,
  dpi = my_dpi
)

# Satisfaction set
p_age_SW <- dat_NW_set_SW %>% filter(str_detect(condition, "Aged")) %>% 
  ggplot(aes(x=condition, y=estimate, group=descriptor, color = descriptor)) +
  geom_line(linewidth=1.75) +
  scale_y_continuous(
    limits = c(
      min((dat_NW_set_SW %>% filter(str_detect(condition, "Aged")))$estimate) - 10, 
      100), 
    breaks = seq(0, 100, 25), 
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
    ) +
  cowplot::theme_cowplot() +
  labs(x = "", y = "", color = "Satisfaction with ...") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate x labels 90°
    # axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5), # Rotate x labels
    panel.grid.major = element_line(colour = "lightgrey"), # Add back major grid lines
    panel.grid.minor = element_line(colour = "lightgrey"), # Add back minor grid lines
    # Reinsert white background for the plot
    panel.background = element_rect(fill = "white", colour = "grey"),
    plot.background = element_rect(fill = "white", colour = NA),
    # legend.position = "bottom",
    # legend.direction = "vertical", # Align legend items horizontally
  ) +
  scale_colour_manual(values = extended_palette); p_age_SW

# Save the figures
ggsave(
  filename = paste0(output_path, "NW_age_set_SW.png"),
  plot = p_age_SW,
  width = 15, height = 8,
  dpi = my_dpi
)

#~=======================================================~=
## Gender ----
#~=======================================================~=

# Function to wrap text to a specified width to prevent too long y-axis
wrap_text <- function(text, width = 25) {
  sapply(text, function(x) str_wrap(x, width = width))
}

# Make the figures
p_gender_1 <- make_gender_plot(
  dat_NW_set_1 %>% filter(condition %in% c("Male", "Female")) %>% 
    mutate(descriptor = wrap_text(descriptor)), 
  "UK National Wellbeing Indicator"
); p_gender_1

ggsave(
  filename = paste0(output_path, "NW_gender_1.png"),
  plot = p_gender_1,
  width = 10, height = 8,
  dpi = my_dpi
)

p_gender_SW <- make_gender_plot(
  dat_NW_set_SW %>% filter(condition %in% c("Male", "Female")) %>% 
    mutate(descriptor = wrap_text(descriptor)), 
  "Satisfaction with ..."
); p_gender_SW

ggsave(
  filename = paste0(output_path, "NW_gender_SW.png"),
  plot = p_gender_SW,
  width = 10, height = 8,
  dpi = my_dpi
)
