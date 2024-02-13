#~############################################################################~#
# Load data ----
#~############################################################################~#

# Load all the sheets in one go
dat_ONS4 <- do.call(rbind, lapply(my_sheets_ONS4, function(sheet_name) {
  load_yearly_sheet(yearly_data_path, sheet = sheet_name)
}))

# Clean some variables
dat_ONS4 <- dat_ONS4 %>%
  mutate(
    # Extract the year
    year = str_extract(condition, "\\d{4}"),
    # Extract the quarter
    quarter = str_extract(condition, "\\(Q[1-4]\\)")
  ) %>%
  # Clean up the Quarter column to leave only the Q1, Q2, Q3, Q4 part
  mutate(quarter = str_remove_all(quarter, "[\\(\\)]")) %>% 
  mutate(year_quarter = paste(year, quarter),
         # Convert to factor and specify levels to ensure correct order in plot
         year_quarter = factor(year_quarter, levels = unique(year_quarter)))

#~############################################################################~#
# Analysis ----
#~############################################################################~#

#~=======================================================~=
## Over time ----
#~=======================================================~=

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### All time ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_time_q <- dat_ONS4 %>%
  ggplot(aes(x = year_quarter, y = score, group = variable, color = variable)) +
  geom_line(linewidth = 1.5) + # Line plot
  # geom_point() + # Add points to each data point for clarity
  cowplot::theme_cowplot() +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(x = "Time (year and quarter)", y = "Average score",
       color = "Wellbeing indicator") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10),
    panel.grid.major = element_line(colour = "lightgrey"), # Add back major grid lines
    # panel.grid.minor = element_line(colour = "lightgrey"), # Add back minor grid lines
    # Reinsert white background for the plot
    panel.background = element_rect(fill = "white", colour = "grey"),
    plot.background = element_rect(fill = "white", colour = NA),
    # legend.position = "bottom",
    # legend.direction = "vertical", # Align legend items horizontally
    # legend.justification = "center",
  ) +
  scale_color_manual(
    values = c(
      "Worthwhile" = "#f79321", 
      "Life satisfaction" = "#3ab7b9",
      "Happiness" = "#8CC43B",
      "Anxiety" = "#DB78A8"
    )); p_time_q

ggsave(
  filename = paste0(output_path, "ONS4_time_scores.png"),
  plot = p_time_q,
  width = 9, height = 5,
  dpi = my_dpi
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2018-2023 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_time_q_2018_2023 <- dat_ONS4 %>%
  filter(year >= 2018) %>%
  ggplot(aes(x = year_quarter, y = score, group = variable, color = variable)) +
  geom_line(linewidth = 1.5) + # Line plot
  # geom_point() + # Add points to each data point for clarity
  cowplot::theme_cowplot() +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(x = "Time (year and quarter)", y = "Average score",
       color = "Wellbeing indicator") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10),
    panel.grid.major = element_line(colour = "lightgrey"), # Add back major grid lines
    # panel.grid.minor = element_line(colour = "lightgrey"), # Add back minor grid lines
    # Reinsert white background for the plot
    panel.background = element_rect(fill = "white", colour = "grey"),
    plot.background = element_rect(fill = "white", colour = NA),
    # legend.position = "bottom",
    # legend.direction = "vertical", # Align legend items horizontally
    # legend.justification = "center",
  ) +
  scale_color_manual(
    values = c(
      "Worthwhile" = "#f79321", 
      "Life satisfaction" = "#3ab7b9",
      "Happiness" = "#8CC43B",
      "Anxiety" = "#DB78A8"
    )); p_time_q_2018_2023

ggsave(
  filename = paste0(output_path, "ONS4_time_scores_2018_2023.png"),
  plot = p_time_q_2018_2023,
  width = 8, height = 5,
  dpi = my_dpi
)

#~=======================================================~=
## Over time stacked ----
#~=======================================================~=

# Step 1: Transform Data
dat_long <- dat_ONS4 %>%
  pivot_longer(
    cols = c("Very High", "High", "Medium", "Low"), 
    names_to = "category", 
    values_to = "value"
  ) %>%
  mutate(year_quarter = factor(year_quarter, levels = unique(year_quarter))) # Ensure correct order

# Reorder the 'category' factor levels
dat_long$category <- factor(dat_long$category, levels = c("Very High", "High", "Medium", "Low"))

# Now, adjust the values so that they sum up to exactly 100 for each group
dat_long <- dat_long %>%
  group_by(year_quarter, variable) %>%
  mutate(
    sum = sum(value, na.rm = TRUE),   # Calculate the sum for each group
    adjusted_value = value / sum * 100  # Scale the values to sum up to 100
  ) %>%
  ungroup() %>%
  select(-sum, -value) %>%
  rename(value = adjusted_value)

# Prepare the labels and fill
my_labels <- c(
  "Very High" = "Very High (9-10)",
  "High" = "High (7-8)",
  "Medium" = "Medium (5-6)",
  "Low" = "Low (0-4)"
)

my_fill <- c(
  "Very High" = "#1f77b4", # Dark blue
  "High" = "#aec7e8",      # Light blue
  "Medium" = "#ffbb78",    # Orange
  "Low" = "#d62728"        # Red
)

# Step 2: Create the Plots

p_time_q_stacked_LS <- stacked_plot(dat_long, "Life satisfaction", my_labels, my_fill)
p_time_q_stacked_W <- stacked_plot(dat_long, "Worthwhile", my_labels, my_fill)
p_time_q_stacked_H <- stacked_plot(dat_long, "Happiness", my_labels, my_fill)
p_time_q_stacked_A <- stacked_plot(
  dat_long, "Anxiety", 
  c(
    "Very High" = "High (6-10)",
    "High" = "Medium (4-5)",
    "Medium" = "Low (2-3)",
    "Low" = "Very low (0-1)"
  ), 
  c(
    "Very High" = "#d62728", # Dark blue
    "High" = "#ffbb78",      # Light blue
    "Medium" = "#aec7e8",    # Orange
    "Low" = "#1f77b4"        # Red
  )
)

p_time_q_stacked <- p_time_q_stacked_LS + p_time_q_stacked_W + p_time_q_stacked_H + p_time_q_stacked_A

ggsave(
  filename = paste0(output_path, "ONS4_time_stack.png"),
  plot = p_time_q_stacked,
  width = 15, height = 10,
  dpi = my_dpi
)
