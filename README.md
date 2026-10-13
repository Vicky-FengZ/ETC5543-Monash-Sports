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

The dashboard includes working Excel import pipelines, session-only file
uploads, cleaned-data inspection and Excel export. Successfully parsed uploads
are shared with the dashboard modules during the current Shiny session. The
Overview page uses Monthly Site Report data; the remaining analytical pages can
be connected progressively as their charts are completed.

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
venues_reports$court_hire
venues_reports$membership
venues_reports$online_sales
venues_reports$merchandise
venues_reports$casual_hours
venues_reports$casual_wages
venues_reports$checks
venues_reports$attendance_checks
venues_reports$facility_hire_checks
venues_reports$court_hire_checks
venues_reports$membership_checks
venues_reports$online_sales_checks
venues_reports$merchandise_checks
venues_reports$casual_hours_checks
venues_reports$casual_wages_checks
venues_reports$files
venues_reports$validation
```

Key outputs:

| Object | Description |
|---|---|
| `finance` | MTD Finance values by fund, metric and value type |
| `attendance` | Facility, activity and customer/category attendance with Month and YTD periods |
| `facility_hire` | Facility-hire revenue and transaction measures with Month and YTD periods |
| `court_hire` | Annual Court Hire comparisons by activity, including previous year, report year, growth achieved, monthly values and reported totals |
| `membership` | Membership measures such as New, Renewed, Active and Retention |
| `online_sales` | Online revenue by site, category and Month/YTD period, with reporting flags for incomplete months |
| `merchandise` | Equipment hire, equipment sales and Monash Sport merchandise revenue/transaction measures |
| `casual_hours` | Monthly summary only: budgeted hours, paid hours and variance, plus the reported total |
| `casual_wages` | Budget, actual and variance for the salary-year block matching each workbook's report year |
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
| Venues Tracking Report | `2026 Venues Tracking Report.xlsx` | Labelled Finance, Attendance, Facility Hire, Court Hire year comparison, Membership, Online Sales, Merchandise, Casual Hours and Casual Wages sheets; a year prefix such as `2026 Casual Hours` and a comparison name such as `Court Hire 2025 vs 2026` are accepted |
| Business Unit report | `Aquatics Site Report 2026.xlsx` | Finance, Direct Visits and Indirect Visits; Memberships and Enrollments are optional |

If a future workbook changes its structure, inspect the returned `validation`
table before using the new year in the dashboard.

## Data upload and review workflow

The Data page provides two clearly separated tasks:

- **Upload & Manage**: upload, parse and remove Excel workbooks.
- **View Cleaned Data**: inspect and download the standardised tables produced
  by successful imports.

### Uploading files

Upload each workbook through the matching report family:

| Upload area | Expected workbook |
|---|---|
| Monthly Site Reports | Annual Monthly Site Report workbooks used by Overview and site comparison |
| Detailed Site Reports | Business Unit Template site or program reports |
| Venues Tracking Reports | Annual Venues Tracking Report workbooks |

One or more `.xlsx` or `.xls` files can be selected at a time. The module copies
each file into a temporary session directory, preserves its original filename
for traceability, and then runs the appropriate import, cleaning and validation
pipeline. The original workbook on the user's computer is never modified.

Files are processed one at a time. The page shows overall parsing progress and
an individual status row for every uploaded file:

| Upload status | Meaning and action |
|---|---|
| `queued` or `parsing` | The workbook is waiting or is currently being read, cleaned and validated |
| `ready` | Parsing completed without validation errors; the cleaned data are available to the dashboard |
| `failed` | The file type, workbook structure, parser or validation produced a blocking error; its data are excluded from the dashboard |

The status row displays the relevant warning count or error message. For files
with warnings, select **View details** beneath the progress line to inspect the
original messages and a short explanation of their impact. Notes are grouped
by worksheet; notes without a worksheet appear under **File-level notes**.
Details are collapsed by default and do not open a popup. The successful status
remains `ready`, including when warnings are present.

A warning does not block a file or alter its extracted values. A validation
error or parser exception does block it, because continuing could silently
mislabel data. If failed validation also supplies warnings, those notes can be
inspected without including that file's data in the dashboard.

If a successfully parsed file has the same report family and filename as an
existing upload, it replaces the older session copy only after the new file has
parsed and validated successfully. If the replacement fails, the previous
ready file and its dashboard data remain available.

Use the remove button beside a status row to delete that temporary upload and
its parsed result. This does not delete or change the original Excel file.

### Viewing cleaned data

Open **View Cleaned Data** and select the report family. The preview supports:

- multiple reporting years;
- worksheet-style navigation based on `source_sheet`;
- Measure and Value type filters for Monthly Site Reports;
- free-text search across displayed columns;
- column sorting and adjustable row pagination; and
- downloading the filtered result as an Excel workbook, with one worksheet per
  represented `source_sheet`.

The preview is built from the already parsed R objects. Changing a filter,
sorting a column or moving between pages does not read the Excel files again.

The current interface is a cleaned-data viewer rather than a cell-for-cell raw
Excel viewer. To investigate a value in its original context, use
`source_file`, `source_sheet`, `source_row` and `source_column` to locate the
corresponding cell in the source workbook.

### New projects, audiences and indicators

Uploads can grow within the supported report layouts:

- Direct/Indirect Visits locate each month's columns from the next month or
  YTD heading. New activity rows and audience columns are retained, and the
  audience total check includes the new categories.
- Memberships, Enrollments and Venues Membership Tracking retain new labelled
  indicator columns. Merchandise Tracking also retains unfamiliar indicators.
- Monthly and Detailed Site Finance accept new discount categories. Reported
  Total Revenue is located at the beginning of each business-unit group and
  reconciled against actual income plus the extracted discount categories.
- The field audit checks recognised Monthly/Detailed finance data areas,
  Detailed visits/member areas, and Venues membership/merchandise areas. Values
  without a recognised column, and non-numeric contents in numeric fields,
  are retained in `additional_data` with their original text and source positions.
  They appear in the same source-sheet preview/export and are excluded from
  existing chart totals. The corresponding warning explains where to look.
  Existing discount subtotal columns are retained there for reference without
  triggering an unfamiliar-field warning or double-counting discounts.
- New worksheets without a supported layout are reported in **View details**.
  They require an extraction rule; they are not silently treated as finance or
  membership data. Dashboard/chart/helper/budget worksheets remain intentional
  exclusions. The field audit is not a cell-by-cell audit of the entire workbook;
  the chosen Casual Hours summary and reporting-year wage scope still apply.

In **View Cleaned Data**, select Detailed Site Reports or Venues Tracking
Reports, then **Metric settings** to define a membership/enrollment/merchandise
indicator's unit and treatment across months. For example, an average fee can
be Currency/Mean and active memberships Count/Last. Unfamiliar indicators start
as Unknown/None, so a fee is not automatically labelled as a headcount.

These settings add `unit` and `aggregation` metadata to the preview and downloaded
tables; they do not recalculate reported values or automatically add indicators
to existing charts. Settings apply within the current session, including later
uploads with the same report family, table and metric. They are isolated from
other users. Rate indicators cannot be configured to sum across months.

For definitions that should apply on future app launches, maintain
[`config/metric_definitions.csv`](config/metric_definitions.csv). Its columns are
`family,dataset,metric,unit,aggregation`; names are matched without case/spacing
differences. Supported units are Unknown, Count, Currency, Rate, Hours and Text;
aggregations are None, Sum, Last and Mean. Restart the app after changing the
CSV. The file is loaded once, and must be included when distributing the app.
The default definitions live outside the parsing code in this file.

### Session scope

Uploads are deliberately session-only in the current version. Refreshing or
closing the dashboard clears the temporary files and their parsed results.
Persistent per-user storage, cached RDS files and background import workers are
reserved for a later deployment stage.

For custom Shiny upload code, remember that Shiny's temporary `datapath` does
not retain the user's filename. Pass both values to a loader:

```r
monthly_reports <- load_monthly_site_reports(
  paths = input$monthly_reports$datapath,
  source_names = input$monthly_reports$name,
  strict = FALSE
)
```

Dashboard filters and charts should operate on the imported R object rather
than calling a loader again.

## Forecasting and business planning

The **Forecasting** page uses Monthly Site Reports for monthly Income and
Expenses. History and models are selected automatically; users do not need to
choose a statistical model. Net Result is not a forecast target.

Choose a 3/6/12-month horizon, price rise (%) and one casual-payroll mode:

- **Quick estimate** (default): Venues Tracking Reports provide the historical
  Casual Salaries reference; enter only the additional pay rise (%). Its
  predicted payroll is not a headcount budget or all staff costs.
- **Advanced pay & hours** (inside **More options**): add/remove staff categories and enter current hourly
  wage, total monthly paid hours, pay rise and hours change. Optional future
  hours support new categories. Review current cost, planned cost and the
  difference, and confirm the selected Expenses scope before using the result.
  This mode needs no Venues wage data and never stacks with the quick rise.

Only the payroll difference is added to baseline Expenses, not the whole future
payroll. Advanced inputs are incremental scenarios, not a replacement of payroll
already inside the finance forecast. Separate employer on-costs and annual-salary
staff are not automatically computed. Scope/history changes reset the advanced
confirmation.

The page calculates minimum income growth and price increase needed to cover
the selected period's costs, including an existing shortfall. **Price sensitivity**
is visible inside **Income needed to cover costs**. Advanced payroll, scope/history,
pricing, demand and timing are grouped in **More options**; edited settings apply
directly, without a separate enable switch. Closing that panel does not reset
the plan. **Monthly plan** and both CSV downloads are always visible below the
results. Validation runs internally and model names are recorded in the assumptions CSV.
Missing years are not joined or filled with zero, and model accuracy is assessed
only when continuous history supports rolling-origin validation.

See [Forecasting and practical planning](docs/forecasting.md) for the formulas,
data checks, limitations, model comparison and testing instructions.

## Performance and website deployment

This project is a Shiny application rather than a self-contained static HTML
file. The browser displays HTML, but data import, validation, uploads and
predictions run in an R process on the server. A static HTML export could embed
precalculated results, but it would not support the current server-side upload
and forecasting workflow by itself.
