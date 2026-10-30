# Data dictionary

The data layer (`R/data/`) converts each uploaded workbook into **long
tables**: one row per value, with columns that say what the value is, when it
applies and exactly where it came from in the workbook. This page lists every
table the dashboard uses, by report family. Column types are the R types the
cleaning code creates (see the `empty_*_table()` functions in each
`R/data/*/cleaning.R` file, which are the source of truth).

Uploads are combined in `R/mod_data_upload.R`, and each report family is
returned as a list of these tables. The **Data → View Cleaned Data** page shows
the same tables and can export them to Excel.

## Columns shared by most tables

| Column | Type | Meaning |
|---|---|---|
| `report_year` | integer | Reporting year of the source workbook (from its file name). |
| `date` | date | First day of the month the value belongs to. Empty for year-to-date or total rows. |
| `month` | integer | Month number, 1–12. |
| `month_label` | character | Month abbreviation, e.g. `Jan`. |
| `period_type` | character | `Month` for a single month, `YTD` for a year-to-date value reported in the workbook, `Reported Total` for a workbook total. |
| `row_type` | character | The kind of row in the source layout, e.g. a detail line, a section total or an overall total (values differ by table, see below). |
| `metric` | character | What the value measures, e.g. `Income`, `Revenue`, `Paid Hours`. |
| `value` | double | The number itself. |
| `is_reported` | logical | `TRUE` when the month has actually been reported. Zero-filled months that have not been reported yet are `FALSE` and are left out of KPIs and trends. |
| `source_file`, `source_sheet`, `source_row`, `source_column` | character / integer | Exact workbook location of the value, so every number can be traced back to the Excel cell. |

**Preference rules used across the dashboard.** Where a workbook reports a
total (a YTD figure, a section total or a facility total), the dashboard uses
that reported figure rather than re-adding its components, and only sums the
months or details when no total is reported. This avoids double counting and
keeps dashboard figures equal to the workbook's own totals. The tests in
`tests/test_site_detail.R` and `tests/test_venues_operations.R` check these
rules.

## 1. Monthly Site Reports (`monthly_site_report_data`)

Used by Overview, Site Comparison, Executive Dashboard and Forecasting.

### `finance`

| Column | Type | Meaning |
|---|---|---|
| `site` | character | Site name, e.g. `H&F Clayton`, `Caulfield`, or `Monash Sport` for the organisation summary. |
| `site_code` | character | Finance code of the site, e.g. `Z07601`. |
| `business_unit` | character | Business unit label from the workbook. |
| `is_summary` | logical | `TRUE` for the organisation-wide summary rows. |
| `metric` | character | `Income` or `Expenses`. |
| `value_type` | character | `Actual` or `Budget`. |

Plus the shared columns `report_year`, `date`, `month`, `month_label`,
`value` and the four `source_*` columns.

### `discounts`

Same identifying columns as `finance`, with `discount_type` (e.g. student,
staff or other discounts) in place of `metric` / `value_type`.

### `total_revenue`

Annual reconciliation of revenue opportunity, one row per site and year.

| Column | Type | Meaning |
|---|---|---|
| `period_end` | date | End of the period the totals cover. |
| `actual_income` | double | Actual income. |
| `discount_opportunity` | double | Value of discounts given. |
| `calculated_total_revenue` | double | `actual_income + discount_opportunity`. |
| `reported_total_revenue` | double | Total revenue as reported in the workbook. |
| `validation_difference`, `validation_relative_difference` | double | Calculated minus reported, absolute and relative. |
| `validation_tolerance` | double | Allowed difference before a warning is raised. |
| `validation_status` | character | Result of the reconciliation check. |

## 2. Detailed Site Reports (Business Unit Templates, `business_unit_template_report_data`)

Used by Site Detail and the facility views in Site Comparison. `finance`,
`discounts` and `total_revenue` follow the Monthly Site Report layout above,
with these identifying columns instead of `site`:

| Column | Type | Meaning |
|---|---|---|
| `report_unit` | character | Business unit the template belongs to, e.g. `Health & Fitness`, `Aquatics`. |
| `campus` | character | `Clayton`, `Caulfield` or `Peninsula`. Together with `report_unit` this identifies a dashboard site. |
| `raw_business_unit` | character | Business unit label exactly as written in the workbook. |
| `fund_code` | character | Finance fund code. |

### `visits`

| Column | Type | Meaning |
|---|---|---|
| `visit_type` | character | `Direct` or `Indirect` visits. |
| `activity` | character | Activity name for activity rows, e.g. a class or facility. |
| `row_type` | character | `Total` for visit totals, `Activity` for per-activity rows. |
| `audience` | character | `Total`, `Student`, `Staff` or `Community`. |

### `memberships` and `enrollments`

Membership and program enrolment snapshots, one row per metric and month.

| Column | Type | Meaning |
|---|---|---|
| `dataset_type` | character | Which section of the template the row came from. |
| `metric` | character | Standardised metric, e.g. `Total Active`, `New Members`, `Expired/Cancelled`, `Active Student Members`, `Target`, `Student Share`. |
| `raw_metric` | character | Metric label exactly as written in the workbook. |
| `metric_type` | character | Whether the metric is a count, a share or a target. |

Enrolment templates zero-fill months that have not been reported yet; the
Site Detail page treats those zeros as missing.

### Check tables

`finance_checks`, `discount_checks`, `visit_checks`, `membership_checks` and
`enrollment_checks` record each reconciliation check (`check_type`,
`expected_value`, `reported_value`, `difference`, `tolerance`, `status`) with
the source location, and feed the warnings shown on the Data page.

## 3. Venues Tracking Reports (`venues_tracking_report_data`)

Used by Venues Operations and by Forecasting's quick payroll estimate.

### `finance`

| Column | Type | Meaning |
|---|---|---|
| `fund` | character | Fund name for fund rows. |
| `row_type` | character | `MTD Total` (all funds for the month), `YTD Total` (all funds year to date) or a single fund's row. |
| `metric` | character | `Income` or `Expenses`. |
| `value_type` | character | `Actual` or `Budget`. |

### `attendance`

| Column | Type | Meaning |
|---|---|---|
| `area` | character | Venue area, e.g. indoor or outdoor; `All` for totals. |
| `activity` | character | Activity, e.g. a sport; `Overall` for the overall total. |
| `category` | character | Breakdown within an activity, e.g. an audience; `Total` for its total. |
| `row_type` | character | `Detail`, `Section Total`, `Summary Total` or `Overall Total`. |

### `facility_hire` and `court_hire`

| Column | Type | Meaning |
|---|---|---|
| `facility` / `court_type` | character | Facility or court being hired. |
| `category`, `raw_label` | character | Hire category and its original workbook label. |
| `row_type` | character | `Detail` lines or the facility `Total`. |
| `unit_cost` | double | Hire price per unit, where reported. |
| `metric` | character | `Revenue` or `Transactions` (facility hire). |
| `data_year`, `comparison_role` | integer / character | Court hire only: the year a column describes and whether it is the current or comparison year. |

### `membership`

| Column | Type | Meaning |
|---|---|---|
| `membership_group`, `membership_type` | character | Membership family and individual product. |
| `row_type` | character | Detail rows or group totals. |
| `metric`, `raw_metric` | character | Standardised and original measure, e.g. active members, new members. |

### `online_sales` and `merchandise`

| Column | Type | Meaning |
|---|---|---|
| `site`, `site_scope`, `category` | character | Online sales: where the sale belongs and its product category. |
| `section`, `item`, `raw_item` | character | Merchandise: product section and item (standardised and original). |
| `unit_cost` | double | Merchandise item price, where reported. |
| `row_type`, `metric` | character | Detail lines or totals, and the measure (e.g. quantity or sales). |

### `casual_hours` and `casual_wages`

| Column | Type | Meaning |
|---|---|---|
| `record_group` | character | `Monthly Summary` for monthly budgeted, paid and variance hours; other groups hold pay-period detail. |
| `record_id`, `roster`, `fortnight`, `notes` | character | Pay-period detail rows. |
| `pay_period_start`, `pay_period_end`, `timesheet_deadline`, `pay_date` | date | Pay-period dates. |
| `series`, `salary_year` | character / integer | Casual wages: salary series (e.g. `Casual Salaries`) and its salary year. |
| `metric`, `raw_metric` | character | E.g. `Budgeted Hours`, `Paid Hours`, `Variance`, `Actual`. |

### Check tables

Each Venues table has a matching `*_checks` table (`check_type`,
`expected_value`, `reported_value`, `difference`, `tolerance`, `status`, plus
`group`, `item` and the source location) used for the Data page warnings.

## Shared tables in every report family

| Table | Meaning |
|---|---|
| `validation` | One row per import issue: `level` (`error`, `warning` or information), `source_file`, `sheet`, `message`. |
| `files` | One row per uploaded workbook with its parsing summary. |

Additional measures found in a workbook beyond the standard layout are kept
and labelled using `config/metric_definitions.csv` (units and how to aggregate
them); see "New projects, audiences and indicators" in the README.
