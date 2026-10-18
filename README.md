# Monash Sport Performance Dashboard

This repository contains a modular R Shiny dashboard and an Excel data
pipeline. The pipeline finds recurring reports, checks their structure and
calculations, and converts them into long tables for use in the dashboard
and forecasting modules.

## Quick start

From the project root, restore the packages and start the app:

```r
renv::restore()
shiny::runApp()
```

You can import Excel reports, inspect the cleaned data and export the results
to Excel. Uploaded files last for the current Shiny session, and the dashboard
modules share data from successful imports. Overview uses Monthly Site Report
data. Other analytical pages can be connected as their charts are completed.

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

Each file has a specific role:

- `app.R`: assemble the navigation and connect Shiny modules.
- `R/data/load.R`: source the complete data layer in dependency order.
- `R/data/common.R`: helpers shared by all report types.
- `R/data/*/import.R`: discover files and read the required worksheets.
- `R/data/*/validation.R`: identify structural problems and reconciliation
  differences.
- `R/data/*/cleaning.R`: convert workbook layouts into analysis-ready tables.
- `R/mod_*.R`: one dashboard page or feature per module.

## Data architecture

Each report type follows this workflow:

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

Extracted tables keep source metadata such as `source_file`, `source_sheet`,
`source_row` and `source_column`, so you can trace a dashboard value back to
its original Excel cell.

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

The three loaders run independently. Run the one for the report type you are
developing or testing.

### 1. Monthly Site Reports

```r
monthly_reports <- load_monthly_site_reports()

monthly_reports$finance
monthly_reports$discounts
monthly_reports$total_revenue
monthly_reports$files
monthly_reports$validation
```

Outputs:

| Object | Description |
|---|---|
| `finance` | Monthly Income and Expenses by site, business unit, Budget, Actual and Variance |
| `discounts` | Monthly Student, Staff and Other discount opportunities |
| `total_revenue` | Annual `Actual Income + Discount Opportunity` reconciliation against the workbook value |
| `files` | Discovered files and report years |
| `validation` | Import, structure and calculation issues |

Use `is_summary == TRUE` for site totals. The Monash Sport summary row gives
the total for the organisation.

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

Outputs:

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

The source workbook's `MTD Total` and `YTD Total` rows stay separate from
fund-level rows to avoid double counting.

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

Outputs:

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

Visit formulas can store fractional values even when Excel displays whole
people. Visit checks allow a difference of one visit by default. Financial
checks use a tolerance of one cent:

```r
site_reports <- load_business_unit_template_reports(
  validation_tolerance = 0.01,
  visit_validation_tolerance = 1
)
```

## Validation

Validation keeps extracted data available and records source discrepancies in
a check table or the loader's `validation` table. It does not silently overwrite
or discard a discrepancy.

Common statuses are:

| Status | Meaning |
|---|---|
| `pass` | Reported value agrees with the independently calculated value within tolerance |
| `fail` | Both values exist but do not reconcile |
| `missing_reported_value` | Components exist but the workbook has no reported total |
| `missing_components` | A reported total exists but some required detail is absent |
| `missing_monthly_values` | An annual/YTD value cannot be fully checked against monthly data |

Warnings allow extraction to continue. Structural errors stop a strict import
to avoid mislabelling data. To review issues:

```r
print(site_reports)
site_reports$validation

subset(site_reports$visit_checks, status != "pass")
subset(site_reports$membership_checks, status != "pass")

stopifnot(!any(site_reports$validation$level == "error"))
```

## Adding future report years

Add a new file to the appropriate raw-data folder and run its loader again.
The loaders do not hard-code years or worksheet row positions.

| Report type | Example future filename | Required recognisable structure |
|---|---|---|
| Monthly Site Report | `Monthly Site Report 2026 - December.xlsx` | Finance sections labelled Income, Expenses, months, Budget, Actual and Variance |
| Venues Tracking Report | `2026 Venues Tracking Report.xlsx` | Labelled Finance, Attendance, Facility Hire, Court Hire year comparison, Membership, Online Sales, Merchandise, Casual Hours and Casual Wages sheets; a year prefix such as `2026 Casual Hours` and a comparison name such as `Court Hire 2025 vs 2026` are accepted |
| Business Unit report | `Aquatics Site Report 2026.xlsx` | Finance, Direct Visits and Indirect Visits; Memberships and Enrollments are optional |

If a workbook's structure changes, check the returned `validation` table
before using that year's data in the dashboard.

## Data upload and review workflow

The Data page has two tasks:

- **Upload & Manage** lets you upload, parse and remove Excel workbooks.
- **View Cleaned Data** lets you inspect and download the standardised tables
  from successful imports.

### Uploading files

Upload each workbook through the matching report family:

| Upload area | Expected workbook |
|---|---|
| Monthly Site Reports | Annual Monthly Site Report workbooks used by Overview and site comparison |
| Detailed Site Reports | Business Unit Template site or program reports |
| Venues Tracking Reports | Annual Venues Tracking Report workbooks |

Select one or more `.xlsx` or `.xls` files at a time. The module copies each
file into a temporary session directory, keeps its original filename for
traceability, and runs the matching import, cleaning and validation pipeline.
Your original workbook stays unchanged.

The module processes one file at a time. The page shows overall parsing
progress and a status row for each upload:

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

A file with the same report family and filename as an existing upload replaces
the session copy after it passes parsing and validation. If the replacement
fails, the previous ready file and its dashboard data stay available.

Use the remove button beside a status row to delete the temporary upload and
its parsed result. Your original Excel file stays unchanged.

### Viewing cleaned data

Open **View Cleaned Data** and select the report family. The preview supports:

- multiple reporting years;
- worksheet-style navigation based on `source_sheet`;
- Measure and Value type filters for Monthly Site Reports;
- free-text search across displayed columns;
- column sorting and adjustable row pagination; and
- downloading the filtered result as an Excel workbook, with one worksheet per
  represented `source_sheet`.

The preview uses the parsed R objects. You can filter, sort and move between
pages without reading the Excel files again.

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

Metric definitions are automatic. Existing defaults identify known counts,
currencies and rates. Unfamiliar indicators retain their name and reported
value with internal Unknown/None definitions. Users do not configure them when
uploading. Internal `unit` and `aggregation` columns are omitted from the ordinary
preview and cleaned Excel download; the imported R objects retain them.
New indicators do not automatically become chart/forecast targets.

Default definitions are delivered in
[`config/metric_definitions.csv`](config/metric_definitions.csv). Its columns are
`family,dataset,metric,unit,aggregation`; names are matched without case/spacing
differences. Supported units are Unknown, Count, Currency, Rate, Hours and Text;
aggregations are None, Sum, Last and Mean. Restart the app after changing the
CSV. The file is loaded once, and must be included when distributing the app.
The default definitions live outside the parsing code in this file.

### Standard Excel templates

Each upload card includes **Download Excel template**. Three blank workbooks
are shipped in `www/templates/`, outside `data/raw/` so they are not discovered
as real reports. They contain input areas, formula totals and a built-in guide
showing required headings, supported additions and filename/year rules.
The example year is 2026; change the guide's Reporting year and Save As with
the matching year before uploading completed data.

- Monthly Site: organisation Finance Summary plus one example site, with
  Income, Expenses, Discounts and Total Revenue. Update the organisation links
  to include every site when adding more site worksheets.
- Detailed Site: Finance, Direct Visits, Indirect Visits, Memberships and
  Enrollments. Rename activity rows and insert new activities before Total;
  repeated audience/indicator additions must be consistent across months.
- Venues Tracking: all nine supported worksheets, including the selected
  Casual Hours summary and the reporting-year Casual Salaries block.

Amber cells are editable inputs. Grey labels and dark headings identify the
required structure. Calculated cells should retain their formulas. Blank is
unreported; enter zero only for a confirmed zero. Blank templates are starting
files rather than reported data. Unknown fields can be viewed/downloaded, but
new business relationships and arbitrary layouts require extraction rules.

See [Excel template guide](docs/excel_templates.md) for instructions and
the specific extension boundaries for each worksheet.

### Session scope

Uploads last only for the current session. Refreshing or closing the dashboard
clears the temporary files and parsed results. Persistent per-user storage,
cached RDS files and background import workers are planned for a later
deployment stage.

In custom Shiny upload code, Shiny's temporary `datapath` does not retain the
original filename. Pass both values to the loader:

```r
monthly_reports <- load_monthly_site_reports(
  paths = input$monthly_reports$datapath,
  source_names = input$monthly_reports$name,
  strict = FALSE
)
```

Reuse the imported R object for dashboard filters and charts to avoid calling
a loader again.

## Forecasting and business planning

The **Forecasting** page uses monthly Income and Expenses from Monthly Site
Reports. It selects history and models automatically, so you do not need to
choose a statistical model. It does not forecast Net Result.

Choose a 3/6/12-month horizon, a price rise (%) and a casual-payroll mode:

- **Quick estimate** is the default. Enter the additional pay rise (%). Venues
  Tracking Reports supply the historical Casual Salaries reference. The
  predicted payroll is not a headcount budget or a measure of all staff costs.
- **Manual adjustment** uses one signed **Monthly payroll adjustment (AUD)**
  entered in **Your plan**. Positive amounts add costs, negative amounts reduce
  costs, and 0 leaves costs unchanged. You do not need Venues wage data.

Choose the payroll method in **More options**; only one method applies at a
time. A manual amount adjusts the Overall financial forecast by an additional
monthly amount. It is not a full replacement payroll budget, so do not deduct
savings from departures already reflected in the forecast. The amount applies
each month from the selected effective date, without compounding. The system
rejects blank amounts and reductions that would make any month's total
Expenses negative.

The page calculates the minimum income growth and price increase needed to
cover costs over the selected period, including any existing shortfall.
**Price sensitivity** appears in **Income needed to cover costs**. Use
**More options** to set the payroll method, history, pricing, demand and timing.
Settings apply directly and stay in effect when the panel is closed.
**Monthly plan** and both CSV downloads are always visible below the results.

Validation runs internally, and the assumptions CSV records the selected model
names. Missing years remain gaps and are not filled with zero. Model accuracy
is assessed only when continuous history supports rolling-origin validation.

See [Forecasting and practical planning](docs/forecasting.md) for the formulas,
data checks, limitations, model comparison and testing instructions.

## Performance and website deployment

The dashboard runs as a Shiny application. The browser displays HTML, while
an R process on the server handles data import, validation, uploads and
predictions. A static HTML export could include precalculated results. On its
own, it would not support the server-side upload and forecasting workflow.
