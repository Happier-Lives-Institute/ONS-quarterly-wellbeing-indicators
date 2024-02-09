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

## Get a list of variables
# Get the list of sheets
sheet_names <- readxl::excel_sheets(quaterly_data_path)
# Write the sheet names to a text file
writeLines(sheet_names, paste0(output_path, "sheet_names.txt"))

## Analysis of ONS4
source("analysis_ONS4_quaterly.R")
source("analysis_ONS4_yearly.R")

## Quarterly analysis of new measures
# Old version of the analysis
# source("analysis_new_measures_old.R")
# More detailed analysis
source("analysis_new_measures.R")

