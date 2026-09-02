# Monash Sport Performance Dashboard

This repository contains a modular R Shiny dashboard and its Excel data
pipeline. The pipeline discovers recurring reports, validates their structure
and calculations, and converts them into long tables that can later feed the
dashboard and forecasting modules.

## Quick start

Restore the project packages and start the app from the project root:

```r
renv::restore()
shiny::runApp()
```

The dashboard shell and upload interface are available. The import functions
are complete, but the dashboard modules are still using prototype inputs; the
upload module currently returns file metadata rather than imported data.

## Project structure

```text
app.R

R/
  config.R
  ui_helpers.R
  calculations.R
  plots.R

  data/
    load.R
    common.R
    monthly_site/
      import.R
      validation.R
      cleaning.R
    venues/
      import.R
      validation.R
      cleaning.R
    business_unit_templates/
      import.R
      validation.R
      cleaning.R

  mod_overview.R
  mod_site_comparison.R
  mod_site_detail.R
  mod_venues_operations.R
  mod_forecasting.R
  mod_data_upload.R

data/
  raw/                 # source Excel files; not tracked by Git
  processed/           # recommended location for production caches

www/
  styles.css
```

The main responsibilities are:

- `app.R`: assemble the navigation and connect Shiny modules.
- `R/data/load.R`: source the complete data layer in dependency order.
- `R/data/common.R`: helpers shared by all report types.
- `R/data/*/import.R`: discover files and read the required worksheets.
- `R/data/*/validation.R`: identify structural problems and reconciliation
  differences.
- `R/data/*/cleaning.R`: convert workbook layouts into analysis-ready tables.
- `R/mod_*.R`: one dashboard page or feature per module.

## Data architecture

All report types follow the same workflow:

```text
data/raw Excel files
        ↓
file discovery and worksheet import
        ↓
structure and calculation validation
        ↓
standardised long tables
        ↓
dashboard calculations, charts and forecasts
```

Extracted tables retain source metadata such as `source_file`, `source_sheet`,
`source_row` and `source_column`. This makes a dashboard value traceable back
to its original Excel cell.

### Data-source summary

| Source | Raw-data folder | Loader | Intended dashboard use |
|---|---|---|---|
| Monthly Site Reports | `data/raw/Monthly Site Report/` | `load_monthly_site_reports()` | Overview and Site & Facility Comparison |
| Venues Tracking Reports | `data/raw/Business Unit Tracking Reports/Venues/` | `load_venues_tracking_reports()` | Venues Operations and facility-level analysis |
| Business Unit Site/Program Reports | `data/raw/Business Unit Templates that feed master reports/` | `load_business_unit_template_reports()` | Site Detail, visits, memberships and enrolments |

## Loading data during development

Source the data layer once before using any loader:

```r
source("R/data/load.R")
```

The three loaders are independent. You only need to run the loader for the
report type you are currently developing or testing.

### 1. Monthly Site Reports

```r
monthly_reports <- load_monthly_site_reports()

monthly_reports$finance
monthly_reports$discounts
monthly_reports$total_revenue
monthly_reports$files
monthly_reports$validation
```

Key outputs:

| Object | Description |
|---|---|
| `finance` | Monthly Income and Expenses by site, business unit, Budget, Actual and Variance |
| `discounts` | Monthly Student, Staff and Other discount opportunities |
| `total_revenue` | Annual `Actual Income + Discount Opportunity` reconciliation against the workbook value |
| `files` | Discovered files and report years |
| `validation` | Import, structure and calculation issues |

Use `is_summary == TRUE` for site-level totals. The Monash Sport summary row is
the organisation-level total.

### 2. Venues Tracking Reports

```r
venues_reports <- load_venues_tracking_reports()

venues_reports$finance
venues_reports$attendance
venues_reports$facility_hire
venues_reports$membership
venues_reports$checks
venues_reports$attendance_checks
venues_reports$facility_hire_checks
venues_reports$membership_checks
venues_reports$files
venues_reports$validation
```

Key outputs:

| Object | Description |
|---|---|
| `finance` | MTD Finance values by fund, metric and value type |
| `attendance` | Facility, activity and customer/category attendance with Month and YTD periods |
| `facility_hire` | Facility-hire revenue and transaction measures with Month and YTD periods |
| `membership` | Membership measures such as New, Renewed, Active and Retention |
| `*_checks` | Independent monthly-to-YTD and finance reconciliation checks |

The source workbook's `MTD Total` and `YTD Total` rows are retained separately
from fund-level rows to prevent accidental double counting.

### 3. Business Unit Site and Program Reports

```r
site_reports <- load_business_unit_template_reports()

site_reports$finance
site_reports$visits
site_reports$memberships
site_reports$enrollments
site_reports$discounts
site_reports$total_revenue
site_reports$finance_checks
site_reports$visit_checks
site_reports$membership_checks
site_reports$enrollment_checks
site_reports$files
site_reports$validation
```

Key outputs:

| Object | Description |
|---|---|
| `finance` | Monthly and reported annual Finance values, including Profit/Loss |
| `visits` | Direct and Indirect visits by activity, audience, month and YTD |
| `memberships` | Monthly membership counts and shares where a Memberships sheet exists |
| `enrollments` | Monthly enrolment measures; multiple labelled Month blocks are supported |
| `discounts` | Discount opportunities retained from the Finance sheet |
| `total_revenue` | Independently recalculated Total Revenue and validation status |

`visit_type` distinguishes Direct and Indirect visits. `audience` contains
Student, Staff, Community and Total, while `period_type` distinguishes Month
and YTD values.

Visits can be formula-derived counts that display as whole people while storing
fractions. Visit checks therefore use a default tolerance of one visit; the
financial tolerance remains one cent:

```r
site_reports <- load_business_unit_template_reports(
  validation_tolerance = 0.01,
  visit_validation_tolerance = 1
)
```

## Validation

Validation does not silently overwrite or discard a source discrepancy.
Extracted data remain available, while the issue is recorded in a check table
or the loader's `validation` table.

Common statuses are:

| Status | Meaning |
|---|---|
| `pass` | Reported value agrees with the independently calculated value within tolerance |
| `fail` | Both values exist but do not reconcile |
| `missing_reported_value` | Components exist but the workbook has no reported total |
| `missing_components` | A reported total exists but some required detail is absent |
| `missing_monthly_values` | An annual/YTD value cannot be fully checked against monthly data |

Warnings do not stop extraction. Structural errors stop a strict import because
continuing could mislabel data. A compact review pattern is:

```r
print(site_reports)
site_reports$validation

subset(site_reports$visit_checks, status != "pass")
subset(site_reports$membership_checks, status != "pass")

stopifnot(!any(site_reports$validation$level == "error"))
```

## Adding future report years

Years and worksheet row positions are not hard-coded. Add a new file to the
appropriate raw-data folder and run its loader again.

| Report type | Example future filename | Required recognisable structure |
|---|---|---|
| Monthly Site Report | `Monthly Site Report 2026 - December.xlsx` | Finance sections labelled Income, Expenses, months, Budget, Actual and Variance |
| Venues Tracking Report | `2026 Venues Tracking Report.xlsx` | Labelled Finance, Attendance, Facility Hire and Membership tracking sheets |
| Business Unit report | `Aquatics Site Report 2026.xlsx` | Finance, Direct Visits and Indirect Visits; Memberships and Enrollments are optional |

If a future workbook changes its structure, inspect the returned `validation`
table before using the new year in the dashboard.

## Uploaded files

Shiny stores uploaded files under temporary paths that do not retain the
original report name. Always pass both `datapath` and `name` to a loader:

```r
monthly_reports <- load_monthly_site_reports(
  paths = input$monthly_reports$datapath,
  source_names = input$monthly_reports$name
)

venues_reports <- load_venues_tracking_reports(
  paths = input$venues_reports$datapath,
  source_names = input$venues_reports$name
)

site_reports <- load_business_unit_template_reports(
  paths = input$site_reports$datapath,
  source_names = input$site_reports$name
)
```

Uploaded workbooks should be imported only after an explicit user action, such
as clicking a Validate or Refresh button. Chart filters should operate on the
already imported R object and must not trigger another Excel import.

## Performance and website deployment

This project is a Shiny application rather than a self-contained static HTML
file. The browser displays HTML, but data import, validation, uploads and
predictions run in an R process on the server. A static HTML export could embed
precalculated results, but it would not support the current server-side upload
and forecasting workflow by itself.