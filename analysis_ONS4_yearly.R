#~############################################################################~#
# Load data ----
#~############################################################################~#

# Load all the sheets in one go
my_sheets_ONS4 <- c(
  "2 Life satisfaction (UK)", 
  "4 Worthwhile (UK)", 
  "6 Happiness (UK)", 
  "8 Anxiety (UK)"
)

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

p_time_q <- dat_ONS4 %>%
ggplot(aes(x = year_quarter, y = score, group = variable, color = variable)) +
  geom_line(linewidth = 1.75) + # Line plot
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
    plot.background = element_rect(fill = "white", colour = NA)
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
  width = 10, height = 8,
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

# Step 2: Create the Plot
p_time_q_stacked <- dat_long %>% 
  ggplot(aes(x = year_quarter, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "stack") + # Stacked bar plot
  scale_fill_manual(
    values = c(
    "Very High" = "#1f77b4", # Dark blue
    "High" = "#aec7e8",      # Light blue
    "Medium" = "#ffbb78",    # Orange
    "Low" = "#d62728"        # Red
    ),
    labels = c(
      "Very High" = "Very High (9-10)",
      "High" = "High (7-8)",
      "Medium" = "Medium (5-6)",
      "Low" = "Low (0-4)"
    )
    ) +
  facet_wrap(~ variable, scales = "free") +
  cowplot::theme_cowplot() +
  labs(x = "", y = "",
       fill = "") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10), 
    # axis.title.x = element_text(margin= margin(t = 20, r = 0, b = 0, l = 0)),
    panel.background = element_rect(fill = "white", colour = "grey"),
    plot.background = element_rect(fill = "white", colour = NA),
    # Move legend to bottom
    legend.position = "bottom",
    # Ensure only one legend for the entire plot
    legend.justification = "center",
    legend.box = "horizontal",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0), 
                     labels = function(x) paste0(x, "%")); p_time_q_stacked

ggsave(
  filename = paste0(output_path, "ONS4_time_stack.png"),
  plot = p_time_q_stacked,
  width = 15, height = 10,
  dpi = my_dpi
)
