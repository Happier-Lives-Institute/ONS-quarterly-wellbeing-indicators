#~############################################################################~#
# Prepare the data ----
#~############################################################################~#

# Remove variables we are not interested in
dat_NW_RG <- dat_NW %>% filter(
  variable %ni% c(
    "Loneliness",
    "Hope for future",
    "Fair treatment",
    "Digital exclusion"
  )
)

#~############################################################################~#
# Age ----
#~############################################################################~#

# Make groups to categorise the satisfaction variables
dat_NW_RG <- dat_NW_RG %>% mutate(
  SW_group = case_when(
    descriptor %in% c(
        "Tend to be satisfied with the healthcare system",
        "Tend to be satisfied with education system",
        "Tend to be satisfied with the police",
        "Tend to be satisfied with courts and legal system"
    ) ~ "Systems",
    descriptor %in% c(
      "Fairly or very satisfied with social relationships",
      "Strongly agree or agree that different backgrounds get on well together in local area",
      "Fairly or very satisfied with local area"
    ) ~ "Social",
    descriptor %in% c(
      "Fairly or very satisfied with accommodation",
      "Fairly or very satisfied with their education and skills",
      "Fairly or very satisfied with main job",
      "Fairly or very satisfied with health",
      "Fairly or very satisfied with how they spend their time in a typical week"
    ) ~ "Personal"
  )
)

#~=======================================================~=
## Systems ----
#~=======================================================~=

# Graph features
group_linewidth <- 1.5
group_alpha <- 0.8
group_h_w <- c(6, 6)

# One with the first group
p_age_RG1 <- dat_NW_RG %>% filter(str_detect(condition, "Aged")) %>% 
  filter(SW_group == "Systems") %>% 
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
  labs(x = "", y = "", color = "") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate x labels 90°
    # axis.text.x = element_text(angle = 50, vjust = 0.5, hjust=0.45), # Rotate x labels
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
    legend.margin = margin(t = -25, unit = "pt")
  ) +
  scale_colour_manual(values = extended_palette); p_age_RG1

# save the figure
ggsave(
  filename = paste0(output_path, "age_RG1.png"),
  plot = p_age_RG1,
  width = group_h_w[1], height = group_h_w[2],
  dpi = my_dpi
)

# One with the second group
p_age_RG2 <- dat_NW_RG %>% filter(str_detect(condition, "Aged")) %>% 
  filter(SW_group == "Social") %>% 
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
  labs(x = "", y = "", color = "") +
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
    legend.margin = margin(t = -25, unit = "pt")
  ) +
  scale_colour_manual(values = extended_palette[5:length(extended_palette)]); p_age_RG2

# save the figure
ggsave(
  filename = paste0(output_path, "age_RG2.png"),
  plot = p_age_RG2,
  width = group_h_w[1], height = group_h_w[2],
  dpi = my_dpi
)

# One with the third group
p_age_RG3 <- dat_NW_RG %>% filter(str_detect(condition, "Aged")) %>% 
  filter(SW_group == "Personal") %>% 
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
  labs(x = "", y = "", color = "") +
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
    legend.margin = margin(t = -25, unit = "pt")
  ) +
  scale_colour_manual(values = extended_palette[8:length(extended_palette)]); p_age_RG3

# save the figure
ggsave(
  filename = paste0(output_path, "age_RG3.png"),
  plot = p_age_RG3,
  width = group_h_w[1], height = group_h_w[2],
  dpi = my_dpi
)

#~############################################################################~#
# Gender ----
#~############################################################################~#

# Make the figures
p_RG_gender <- make_gender_plot(
  dat_NW_RG %>% filter(condition %in% c("Male", "Female")) %>% 
    mutate(descriptor = wrap_text(descriptor, width = 30)), 
  ""
) +theme(legend.margin = margin(t = -25, unit = "pt")); p_RG_gender

ggsave(
  filename = paste0(output_path, "NW_RG_gender.png"),
  plot = p_RG_gender,
  width = 8, height = 8,
  dpi = my_dpi
)
