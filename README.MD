# Purpose

This is the code to reproduce the analysis and graphs for What Works Wellbeing's quarterly blogpost about wellbeing indicators. This code was produced by the Happier Lives Institute.

# Data sources

The data for all the indicators is available [here](https://www.ons.gov.uk/releases/ukmeasuresofnationalwellbeing) and the dashboard presentation is [here](https://www.ons.gov.uk/peoplepopulationandcommunity/wellbeing/articles/ukmeasuresofnationalwellbeing/dashboard).

The quarterly data for all the indicators is available [here](https://www.ons.gov.uk/peoplepopulationandcommunity/wellbeing/datasets/ukmeasuresofnationalwellbeing).

In there, there is an "alternative sources" sheet that tells you where to find more data for the different indicators.

For example, the ONS four are only represented with the % of people responding 'low' (0-4 on the 0-10 scale; or 'high in the case of anxiety). If you want the mean score, or the % of 'high', you need the extra data. This can be found [here](https://www.ons.gov.uk/peoplepopulationandcommunity/wellbeing/datasets/quarterlypersonalwellbeingestimatesnonseasonallyadjusted).

Using this data based on the [Open Government Licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

# How to use

First, obtain the code as an R project by clicking `ons_quarterly.Rproj`.

Run the `main.R` file, which will run all the analyses and produce the graphs. You need to provide it with two variables:
- `quaterly_data_path` which will tell it which data to select.
- `ouput_path` which will tell it where to output the results.

The code functions based on analyses in the different `analysis_XXX.R` files, and is powered by the the `dependencies.R` which contains custom graph functions.

These **custom graph functions** ensure that only one place needs to be edited to modify all the graphs. This doesn't prevent one from further customising one specific graph.