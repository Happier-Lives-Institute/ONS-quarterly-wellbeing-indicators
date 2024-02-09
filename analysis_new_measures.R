#~############################################################################~#
# Prepare the data ----
#~############################################################################~#

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
