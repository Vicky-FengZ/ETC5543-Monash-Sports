# Tests

The tests are plain R scripts (no test framework): each file stops with an
error when a check fails and prints `PASS …` lines when it succeeds. Run
them from the project root.

## Running the tests

| Command | Data used | Time* |
|---|---|---|
| `source("tests/run_tests.R")` | Small fixtures and a few sample workbooks | about 1 minute |
| `source("tests/run_sample_tests.R")` | Also every synthetic workbook in `data/sample/` | about 4 minutes |
| `source("tests/run_full_tests.R")` | Also every real report in `data/raw/` (not in the repository) | several minutes |

\* On an 8-core laptop. Each run ends with `ALL TESTS PASSED` and lists the
slowest groups. GitHub Actions (`.github/workflows/tests.yml`) runs the
sample tests on every push.

The test files run in groups on several R processes at once. To run
everything in one R session, for example while debugging, first run
`Sys.setenv(MONASH_SPORT_TEST_WORKERS = "1")`.

## What each file checks

**Importing and uploading reports**

| File | Checks |
|---|---|
| `test_data_pipeline_regressions.R` | Cell parsing (percentages, negatives in brackets), worksheet selection and order, and validation messages |
| `test_venues_tracking_extensions.R` | Venues Tracking Report import for a future year: year-named worksheets, new sections and the reconciliation checks |
| `test_extensible_imports.R` | New columns and categories in later reports pass through cleaning, validation, preview and export |
| `test_excel_templates.R` | The blank Excel templates have the structure the importers expect |
| `test_basic_upload.R` | Data page: one "Add data" entry, matching files to report types, choosing a type for unrecognised files, replacing and removing files, saving and loading parsed data |
| `test_upload_warnings.R` | Validation warnings are counted, grouped and shown without losing any |
| `test_upload_realworld.R` | Situations met in practice, with the real importers: two files for one year, a wrong year in the file name, blank templates, damaged files, saved data that overlaps the session, a new report year |
| `test_real_upload_parsers.R` | Every workbook in the report set parses (sample and full runs only) |

**Dashboard pages**

| File | Checks |
|---|---|
| `test_overview_charts.R` | Overview finance figures and charts |
| `test_executive_dashboard.R` | Executive Dashboard visuals and how they behave with partial data |
| `test_site_comparison.R` | Site Comparison calculations: site metrics, revenue opportunity, discount mix |
| `test_site_detail.R` | Site Detail filters, KPIs and trends |
| `test_venues_operations.R` | Venues Operations attendance, facility hire, workforce and finance values |
| `test_chart_and_ui_helpers.R` | Shared chart and interface helpers (gaps in lines, legends, palettes, month labels) |
| `test_plot_smoke.R` | Every Site Detail and Venues chart draws at a wide and a narrow size |

**Forecasting**

| File | Checks |
|---|---|
| `test_forecasting.R` | Models, backtests, prediction intervals and scenarios; gaps and short histories |
| `test_forecasting_simple.R` | The default workflow and casual-payroll adjustments |
| `test_forecasting_manual.R` | Manual monthly changes, their timing and the CSV downloads |
| `test_forecasting_options.R` | Safeguards for zero months and payroll choices |
| `test_forecasting_layout.R` | Page structure and the settings shown in each mode |

**The whole app and its numbers**

| File | Checks |
|---|---|
| `test_app_smoke.R` | Starts the app from `app.R` and renders every output on every page with no data, one report of each kind, a combined set and (sample and full runs) the whole set, also loaded from saved data |
| `test_cross_page_consistency.R` | The same figure is the same on every page (Overview, Executive Dashboard, Forecasting, Site Comparison) |
| `test_key_numbers.R` | Site totals add up to the organisation summary; the annual figures match the accepted snapshot and any figures confirmed in the Excel reports (sample and full runs only) |

## Supporting files

| File | Purpose |
|---|---|
| `fixtures_site_venues.R` | Small hand-checkable data for the Site Detail and Venues tests |
| `helper_parse_cache.R` | Parses each sample workbook once per run and reuses the result, to keep the run short |
| `snapshots/` | Accepted key figures for the sample workbooks, used by `test_key_numbers.R` |

When a code change is meant to alter the key figures, see *Testing* in the
main README for how to accept a new snapshot.
