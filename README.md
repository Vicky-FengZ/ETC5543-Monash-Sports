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

### Import Monthly Site Reports

The importer automatically discovers files whose names start with
`Monthly Site Report`, extracts the year from each file name, validates the
Finance sections and combines all years into one long table.

```r
source("R/data_import.R")
source("R/data_validation.R")
source("R/data_cleaning.R")

reports <- load_monthly_site_reports()

finance_data <- reports$finance
discount_data <- reports$discounts
total_revenue_checks <- reports$total_revenue
reports$files
reports$validation
```

The Finance table contains one row per month, site, business unit, metric and
value type. Use rows where `is_summary` is `TRUE` for site-level totals; use
`site == "Monash Sport"` together with `is_summary` for the organisation total.

The Discounts table contains monthly Student, Staff and Other opportunity
values at the same site and business-unit levels. The Total Revenue table is an
annual audit table. It retains the value reported by Excel and independently
recalculates `actual_income + discount_opportunity`. Use
`calculated_total_revenue` for dashboard charts and inspect
`validation_status`, `validation_difference` and `reports$validation` before
using a newly added report year.

```r
table(
  reports$total_revenue$report_year,
  reports$total_revenue$validation_status
)

subset(
  reports$total_revenue,
  validation_status != "pass"
)
```

Future reports such as `Monthly Site Report 2026 -December.xlsx` can be placed
in the same folder and will be discovered automatically, provided they retain
the same labelled Finance structure (`INCOME`, `EXPENSES`, months and
`Budget`/`Actual`/`Variance`) together with the labelled `DISCOUNTS` and
`Total Revenue (actual + opp)` blocks. The year and source row numbers are not
hard-coded.

For uploaded files, keep the original file names because Shiny's temporary
`datapath` does not contain the report year:

```r
reports <- load_monthly_site_reports(
  paths = input$monthly_reports$datapath,
  source_names = input$monthly_reports$name
)
```
