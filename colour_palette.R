# Starting with the original 5 colours
original_colours <- c("#3ab7b9", "#f79321", "#8CC43B", "#888888", "#DB78A8")

# # Generate lighter and darker shades for each original colour
# lighter_colours <- sapply(original_colours, function(col) lighten(col, amount = 0.2))
# darker_colours <- sapply(original_colours, function(col) darken(col, amount = 0.2))
# 
# # Combine original, lighter, and darker colours
# expanded_palette <- c(original_colours, lighter_colours, darker_colours)
# 
# # If needed, add complementary colours to reach 15
# additional_colours <- qualitative_hcl(15 - length(expanded_palette))
# expanded_palette <- c(expanded_palette, additional_colours)

# Additional complementary colours
additional_colours <- c(
  "#DAA520", # Golden rod
  "#4B0082", # Indigo
  "#FF1493", # Deep pink, replaced one pink with a deeper version for better contrast
  "#4682B4", # Steel blue
  "#006400", # Dark green, replacing pale green for better visibility
  "#DC143C", # Crimson
  "#FFBF00"  # Amber
)

# Combine the original and additional colours to create the extended palette
extended_palette <- c(original_colours, additional_colours)
