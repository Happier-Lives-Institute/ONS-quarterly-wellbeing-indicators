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
## General levels of satisfaction ----
#~=======================================================~=

# First general set
p_general_sat <- dat_NW_set_SW %>% filter(str_detect(condition, "202")) %>% 
  # filter to take only the tail of each variable
  group_by(variable) %>%
  slice_tail(n = 1) %>%
  # make a graph with a point and se whiskers for each descriptor
  ggplot(aes(x=reorder(descriptor, estimate), y=estimate, group=descriptor)) +
  geom_point(aes(color = descriptor), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_y_continuous(
    limits = c(
      0, 
      100), 
    breaks = seq(0, 100, 25), 
    labels = function(x) paste0(x, "%"),
    expand = c(0, 2.5)
  ) +
  cowplot::theme_cowplot() +
  labs(x = "", y = "", color = "Satisfaction with ...") +
  theme(
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate x labels 90°
    # axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5), # Rotate x labels
    panel.grid.major = element_line(colour = "lightgrey"), # Add back major grid lines
    panel.grid.minor = element_line(colour = "lightgrey"), # Add back minor grid lines
    # Reinsert white background for the plot
    panel.background = element_rect(fill = "white", colour = "grey"),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "bottom",
    legend.direction = "vertical", # Align legend items horizontally
  ) +
  coord_flip() +
  # remove the colour legend
  guides(colour = "none") +
  scale_colour_manual(values = extended_palette); p_general_sat
  
# Save the figures
ggsave(
  filename = paste0(output_path, "NW_set_SW.png"),
  plot = p_general_sat,
  width = 10, height = 8,
  dpi = my_dpi
)

#~=======================================================~=
## Age ----
#~=======================================================~=

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### First set ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Satisfaction ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### General figure ----
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
    legend.position = "bottom",
    legend.direction = "vertical", # Align legend items horizontally
  ) +
  scale_colour_manual(values = extended_palette); p_age_SW

# Save the figures
ggsave(
  filename = paste0(output_path, "NW_age_set_SW.png"),
  plot = p_age_SW,
  width = 10, height = 8,
  dpi = my_dpi
)

#### Broken apart figure ----

# Make groups to categorise the satisfaction variables
dat_NW_set_SW <- dat_NW_set_SW %>% mutate(
  SW_group = case_when(
    descriptor %in% c(
        "Tend to be satisfied with the healthcare system",
        "Tend to be satisfied with education system",
        "Tend to be satisfied with the police",
        "Tend to be satisfied with courts and legal system"
    ) ~ "Healthcare, education, police, courts",
    descriptor %in% c(
      "Fairly or very satisfied with health",
      "Fairly or very satisfied with how they spend their time in a typical week",
      "Fairly or very satisfied with their education and skills",
      "Fairly or very satisfied with social relationships"
    ) ~ "Health, time, education, social relationships",
    descriptor %in% c(
      "Fairly or very satisfied with main job",
      "Fairly or very satisfied with local area",
      "Fairly or very satisfied with accommodation"
    ) ~ "Main job, local area, accommodation",
  )
)

# One with only the four that are about 'tend to be satisfied with'
p_age_SW_grouped <- dat_NW_set_SW %>% filter(str_detect(condition, "Aged")) %>% 
  mutate(descriptor = wrap_text(descriptor, 50)) %>% 
  ggplot(aes(x=condition, y=estimate, group=descriptor, color = descriptor)) +
  geom_line(linewidth=1.75) +
  scale_y_continuous(
    limits = c(
      0, 
      100), 
    breaks = seq(0, 100, 25), 
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  facet_wrap(~SW_group, scales = "free", labeller = as_labeller(wrap_label), ncol=2) +
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
    strip.background = element_blank(),
    strip.text = element_blank(),
    # legend.position = "bottom",
    legend.direction = "vertical", # Align legend items horizontally
    legend.justification = "center",
    # adjust the legend's position so that it is in the third cell of the facet_wrap
    # might need to fiddle with this and the wrap function
    legend.position = c(0.75, 0.1)
    
  ) +
  scale_colour_manual(values = extended_palette); p_age_SW_grouped

# save the figure
ggsave(
  filename = paste0(output_path, "NW_age_set_SW_grouped.png"),
  plot = p_age_SW_grouped,
  width = 10, height = 8,
  dpi = my_dpi
)

# Graph features
group_linewidth <- 1.5
group_alpha <- 0.8
group_h_w <- c(6, 6)

# One with the first group
p_age_SW_grouped_1 <- dat_NW_set_SW %>% filter(str_detect(condition, "Aged")) %>% 
  filter(SW_group == "Healthcare, education, police, courts") %>% 
  mutate(descriptor = wrap_text(descriptor, 50)) %>% 
  ggplot(aes(x=condition, y=estimate, group=descriptor, color = descriptor)) +
  geom_line(linewidth=group_linewidth, alpha = group_alpha) +
  scale_y_continuous(
    limits = c(
      0, 
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
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom",
    legend.direction = "vertical", # Align legend items horizontally
    legend.justification = "center",
  ) +
  scale_colour_manual(values = extended_palette); p_age_SW_grouped_1

# save the figure
ggsave(
  filename = paste0(output_path, "NW_age_set_SW_grouped_1.png"),
  plot = p_age_SW_grouped_1,
  width = group_h_w[1], height = group_h_w[2],
  dpi = my_dpi
)

# One with the second group
p_age_SW_grouped_2 <- dat_NW_set_SW %>% filter(str_detect(condition, "Aged")) %>% 
  filter(SW_group == "Health, time, education, social relationships") %>% 
  mutate(descriptor = wrap_text(descriptor, 50)) %>% 
  ggplot(aes(x=condition, y=estimate, group=descriptor, color = descriptor)) +
  geom_line(linewidth=group_linewidth, alpha = group_alpha) +
  scale_y_continuous(
    limits = c(
      0, 
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
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom",
    legend.direction = "vertical", # Align legend items horizontally
    legend.justification = "center",
  ) +
  scale_colour_manual(values = extended_palette[5:length(extended_palette)]); p_age_SW_grouped_2

# save the figure
ggsave(
  filename = paste0(output_path, "NW_age_set_SW_grouped_2.png"),
  plot = p_age_SW_grouped_2,
  width = group_h_w[1], height = group_h_w[2],
  dpi = my_dpi
)

# One with the third group
p_age_SW_grouped_3 <- dat_NW_set_SW %>% filter(str_detect(condition, "Aged")) %>% 
  filter(SW_group == "Main job, local area, accommodation") %>% 
  mutate(descriptor = wrap_text(descriptor, 50)) %>% 
  ggplot(aes(x=condition, y=estimate, group=descriptor, color = descriptor)) +
  geom_line(linewidth=group_linewidth, alpha = group_alpha) +
  scale_y_continuous(
    limits = c(
      0, 
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
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom",
    legend.direction = "vertical", # Align legend items horizontally
    legend.justification = "center",
  ) +
  scale_colour_manual(values = extended_palette[9:length(extended_palette)]); p_age_SW_grouped_3

# save the figure
ggsave(
  filename = paste0(output_path, "NW_age_set_SW_grouped_3.png"),
  plot = p_age_SW_grouped_3,
  width = group_h_w[1], height = group_h_w[2],
  dpi = my_dpi
)

#~=======================================================~=
## Gender ----
#~=======================================================~=

# Make the figures
p_gender_1 <- make_gender_plot(
  dat_NW_set_1 %>% filter(condition %in% c("Male", "Female")) %>% 
    mutate(descriptor = wrap_text(descriptor)), 
  "UK National Wellbeing Indicator"
); p_gender_1

ggsave(
  filename = paste0(output_path, "NW_gender_1.png"),
  plot = p_gender_1,
  width = 8, height = 6,
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
  width = 8, height = 7,
  dpi = my_dpi
)
