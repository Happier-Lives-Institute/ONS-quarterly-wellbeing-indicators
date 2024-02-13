#~############################################################################~#
# General preparation ----
#~############################################################################~#

# Clean
rm(list=ls())

#~=======================================================~=
## Installing ----
#~=======================================================~=
# Making sure to install everything that is needed (but without loading it)
# because something we only use one function so we want to avoid overwriting
# and taking up too much space.

# Here's a function that will install any missing library.
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (length(find.package(pkg, quiet = TRUE)) == 0) {
      install.packages(pkg)
    }
  }
}

install_if_missing(c("tidyverse", "readxl", "cowplot", "patchwork", "scales", "colorspace"))

#~=======================================================~=
## Loading ----
#~=======================================================~=

library(tidyverse)
library(patchwork) # for multiple graphs in one
library(colorspace) # for colour palettes

#~############################################################################~#
# Custom functions ----
#~############################################################################~#

# Custom labeller function
# Need to fit the strips of the panels
wrap_label <- function(labels) {
  sapply(labels, function(label) {
    wrapped_label <- str_wrap(label, width = 40) # Adjust width as needed
    wrapped_label
  })
}

# Function to wrap text to a specified width to prevent too long y-axis
wrap_text <- function(text, width = 25) {
  sapply(text, function(x) str_wrap(x, width = width))
}

# Function to check if a column can be fully converted to numeric
can_be_numeric <- function(x) {
  # Attempt to convert to numeric; NAs are allowed
  converted <- suppressWarnings(as.numeric(x))
  # Check if all non-NA original values were successfully converted
  all(is.na(x) | !is.na(converted))
}

# Function to load a sheet from the quarterly xlsx file
load_quarter_sheet <- function(xlsx_path, sheet_name) {
  
  # Prepare the variable name to classify this data.frame
  variable <- str_trim(str_replace_all(str_remove_all(str_remove_all(
    sheet_name, "[0-9]"), "\\."), "_", " "))
  variable <- str_replace_all(variable, "fac'n", "faction")
  variable <- str_replace_all(variable, "fact'n", "faction")
  variable <- str_replace_all(variable, "edu ", "education ")
  variable <- str_replace_all(variable, "accomm", "accommodation")
  
  # Read the sheet
  all_sheet_data <- readxl::read_xlsx(
    path = xlsx_path, sheet = sheet_name, col_names = F
  )
  
  # Detect the tables according to a specific pattern of formatting
  table_string <- paste0("Table ", str_remove(substr(sheet_name, 1, 4), "_"))
  
  # Find rows where any cell contains 'Table XX.XXa'
  # Take the last mention
  table_a_row <- tail(which(
    str_detect(all_sheet_data[[1]], 
               paste0(table_string, "a"))
  ), 1)
  start_row_a <- table_a_row + 1
  
  # Find rows where any cell contains 'Table XX.XXb'
  # Take the 2nd mention
  table_b_row <- tail(which(
    str_detect(all_sheet_data[[1]], 
               paste0(table_string, "b"))
  ), 1)
  start_row_b <- table_b_row + 1
  
  # Might need to set a contingency for when the tables do not have these formats
  
  # Read the tables. Adjust the range as needed based on the table structure
  table_a <- readxl::read_xlsx(
    xlsx_path, sheet = sheet_name, 
    range = readxl::cell_rows(c(start_row_a, start_row_b-2))
  ) %>%
    filter(!if_all(everything(), is.na)) # remove trailing rows with all NAs
  
  table_b <- readxl::read_xlsx(
    xlsx_path, sheet = sheet_name, 
    range = readxl::cell_rows(c(start_row_b, start_row_b + 1000))
  ) %>%
    filter(!if_all(everything(), is.na)) # remove trailing rows with all NAs
  
  # Make the column names more readable
  colnames(table_a)[1] <- "condition"
  colnames(table_b)[1] <- "condition"
  colnames(table_a)[2] <- "estimate"
  colnames(table_b)[2] <- "estimate"
  
  if(any(str_detect(colnames(table_a), "Sample size"))) {
    table_a <- table_a[, c(1:4, ncol(table_a)), drop = F]
  } else {
    table_a <- table_a[, c(1:4), drop = F]
  }
  if(any(str_detect(colnames(table_a), "Sample size"))) {
    table_b <- table_b[, c(1:4, ncol(table_b)), drop = F]
  } else {
    table_b <- table_b[, c(1:4), drop = F]
  }
  
  for (i in 1:length(colnames(table_a))) {
    colnames(table_a)[i] <- case_when(
      str_detect(colnames(table_a)[i], "LCL") ~ "lower",
      str_detect(colnames(table_a)[i], "UCL") ~ "upper",
      str_detect(colnames(table_a)[i], "Sample size") ~ "sample_size",
      .default = colnames(table_a)[i]
    )
  }
  
  for (i in 1:length(colnames(table_b))) {
    colnames(table_b)[i] <- case_when(
      str_detect(colnames(table_b)[i], "LCL") ~ "lower",
      str_detect(colnames(table_b)[i], "UCL") ~ "upper",
      str_detect(colnames(table_b)[i], "Sample size") ~ "sample_size",
      .default = colnames(table_b)[i]
    )
  }
  
  # need to clean [c] and [x] to become NA
  table_a <- table_a %>%
    mutate(across(everything(), ~str_replace_all(., "\\[c\\]", NA_character_))) %>%
    mutate(across(everything(), ~str_replace_all(., "\\[x\\]", NA_character_)))
  table_b <- table_b %>%
    mutate(across(everything(), ~str_replace_all(., "\\[c\\]", NA_character_))) %>%
    mutate(across(everything(), ~str_replace_all(., "\\[x\\]", NA_character_)))
  
  # Detect if a column has only numeric characters and convert it to numeric
  # Apply transformation conditionally across all character columns
  table_a <- table_a %>%
    mutate(across(where(~is.character(.x) && can_be_numeric(.x)), 
                  ~suppressWarnings(as.numeric(.x))))
  table_b <- table_b %>%
    mutate(across(where(~is.character(.x) && can_be_numeric(.x)), 
                  ~suppressWarnings(as.numeric(.x))))
  
  # Combine the tables
  my_table <- bind_rows(
    table_a %>% mutate(table = "a"),
    table_b %>% mutate(table = "b")
  ) %>% mutate(variable = variable)
  
  return(my_table)
  
}

# Function to load a sheet from the yearly xlsx file
load_yearly_sheet <- function(xlsx_path, sheet_name) {
  
  # Prepare the variable name to classify this data.frame
  variable <- str_trim(str_remove_all(str_replace_all(str_remove_all(str_remove_all(
    sheet_name, "[0-9]"), "\\."), "_"," "), "\\(UK\\)"))
  
  # Read the sheet
  all_sheet_data <- readxl::read_xlsx(
    path = xlsx_path, sheet = sheet_name, col_names = F
  )
  
  # Find rows where any cell contains 'Table '
  # Take the last mention
  table_row <- tail(which(
    str_detect(all_sheet_data[[1]], 
               "Table ")
  ), 1)
  start_row <- table_row + 1
  
  # Read the tables. Adjust the range as needed based on the table structure
  my_table <- readxl::read_xlsx(
    xlsx_path, sheet = sheet_name, 
    range = readxl::cell_rows(c(start_row, start_row + 1000))
  ) %>%
    # remove trailing rows with all NAs
    filter(!if_all(everything(), is.na)) %>% 
    # remove data accuracy columns 
    select(-contains("data accuracy")) %>% 
    mutate(variable = variable)
  
  colnames(my_table) <- c("condition", "score", "Low", "Medium", "High", "Very High", "variable")
  
  return(my_table)
}

#~############################################################################~#
# Custom plots ----
#~############################################################################~#

# Age trend plot
make_age_trend_plot <- function(dat, my_variable, my_title, my_colour) {
  dat %>% filter(variable == my_variable) %>%  
    ggplot(aes(x=condition, y=estimate, group=variable)) +
    geom_line(linewidth=1.75, colour = my_colour) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    cowplot::theme_cowplot() +
    labs(x = "", y = "", title = my_title) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # Rotate x labels 90°
      # axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=0.5), # Rotate x labels
      panel.grid.major = element_line(colour = "lightgrey"), # Add back major grid lines
      panel.grid.minor = element_line(colour = "lightgrey"), # Add back minor grid lines
      # Reinsert white background for the plot
      panel.background = element_rect(fill = "white", colour = "grey"),
      plot.background = element_rect(fill = "white", colour = NA)
    ) 
}

# Gender plot
make_gender_plot <- function(dat, my_y) {
  dat %>% 
    ggplot(aes(x = estimate, y = descriptor, group = condition, color = condition)) +
    geom_line(aes(group = descriptor), colour = "darkgrey") + # Draw lines between Male and Female for each variable
    geom_point(size = 3, alpha = 0.7) + # Draw circles for Male and Female; adjust size as needed
    scale_x_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")) +
    # scale_shape_manual(values = c("Male" = 21, "Female" = 21)) + # Ensure both Male and Female are represented by circles
    labs(x = "", y = my_y, color = "Sex") + # Label x and y axes
    cowplot::theme_cowplot() + # Use a minimal theme for a clean look
    theme(
      legend.position = "bottom", # Move legend to the bottom
      legend.direction = "horizontal", # Align legend items horizontally
      legend.box = "horizontal",
      legend.justification = "center",
      panel.grid.major = element_line(colour = "lightgrey"), # Add back major grid lines
      panel.grid.minor = element_line(colour = "lightgrey"), # Add back minor grid lines
      # Reinsert white background for the plot
      panel.background = element_rect(fill = "white", colour = "grey"),
      plot.background = element_rect(fill = "white", colour = NA)
    ) +
    scale_color_manual(values = c("Male" = "#f79321", "Female" = "#3ab7b9")) # Customize colors for Male and Female
}

# Make stacked plot for the time trend
stacked_plot <- function(my_data, my_variable, my_labels, my_fill) {
  
  my_data %>% filter(variable == my_variable) %>%
    ggplot(aes(x = year_quarter, y = value, fill = category)) +
    geom_bar(stat = "identity", position = "stack") + # Stacked bar plot
    scale_fill_manual(
      values = my_fill,
      labels = my_labels
    ) +
    cowplot::theme_cowplot() +
    labs(x = "", y = "", title = my_variable,
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
                       labels = function(x) paste0(x, "%"))
  
}
