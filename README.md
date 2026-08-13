# Monash Sport Dashboard — modular prototype

This folder is a modular rewrite of the single-file Shiny prototype.

## Run

Open the project folder and run:

```r
shiny::runApp()
```

Install the UI packages first if needed:

```r
install.packages(c("shiny", "bslib", "bsicons"))
```

`readxl` is only required once Excel import is connected:

```r
install.packages("readxl")
```

## Structure

```text
app.R
R/
  config.R
  ui_helpers.R

  data_import.R
  data_validation.R
  data_cleaning.R
  calculations.R
  plots.R

  mod_overview.R
  mod_site_comparison.R
  mod_site_detail.R
  mod_venues_operations.R
  mod_forecasting.R
  mod_data_upload.R

www/
  styles.css
```

## Responsibilities

- `app.R`: assemble navigation and start modules only.
- `config.R`: shared choices, years, site names and theme.
- `ui_helpers.R`: reusable small UI components.
- `data_import.R`: Excel reading only.
- `data_validation.R`: check file/sheet/data validity.
- `data_cleaning.R`: convert raw Excel content into standard structures.
- `calculations.R`: reusable business calculations.
- `plots.R`: reusable ggplot/plotly functions.
- `mod_*.R`: one dashboard page/feature per module.
- `styles.css`: visual styling.

## Data
Raw Excel reports are stored locally in `data/raw/` and are not tracked by Git.