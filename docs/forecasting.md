# Forecasting and practical planning

The Forecasting page uses uploaded **Monthly Site Reports** for monthly Actual
Income and Expenses. It forecasts those two measures only. It does not forecast
Net Result and does not treat historic Budget as a future budget.

## What the user can do

1. Upload Monthly Site Reports in **Data**, then open **Forecasting**.
2. Choose Overall or a site, the earliest training year, the last actual month,
   and a 3, 6 or 12 month horizon. Forecasting starts after the last complete
   actual month, not after the computer's current date.
3. Choose automatic model comparison or a specific baseline model.
4. Set the annual payroll increase, a per-employee/FTE annual increase, or a
   percentage payroll increase. Enter employer cost, including on-costs if
   relevant. Enter a payroll share when changing payroll by percentage or
   changing non-payroll costs; that share is a user assumption, not an extracted
   payroll measurement.
5. Enter the price increase, the share of income that can be repriced, the demand
   change, and the effective month. The initial 100% repriceable share is an
   editable assumption. Demand is not estimated from historical prices.
6. Compare monthly actuals, the statistical baseline and the scenario. Inspect
   the minimum income growth and price change needed to cover the **whole
   selected period's** expenses. Inspect individual months in the monthly table.
   **Use this minimum price** applies the rounded-up minimum to the scenario.
7. Download the monthly plan and assumptions as separate CSV files.

## Data checks and model selection

- Only monthly Actual summary rows are used. Overall prefers the Monash Sport
  summary; site summaries are used as a fallback only if it is absent, with a
  coverage note. Detail rows and YTD totals are not added to monthly totals.
- Overlapping actual site/month/metric records stop modelling until the
  duplicate upload is removed. A missing site's contribution is not summed as
  zero. Missing months remain missing and split the history.
- Both Income and Expenses must exist for a month. The training window is the
  latest complete continuous run. Incomplete final months are excluded. Final
  months with both measures equal to zero are excluded until the user explicitly
  confirms them as actuals; interior zero months are preserved.
- Fewer than 12 continuous months: forecasting is unavailable. One continuous
  year: Seasonal naive repeats the same month-of-year values as a provisional
  baseline, without a backtest claim or seasonal interval estimate.
- With sufficient history, Seasonal naive and Naive compete on identical
  rolling-origin folds. With at least 30 continuous months, additive
  Holt-Winters joins the comparison and all candidates use at least 24 training
  months. The held-out horizon is at most 12 months, shortened to retain at
  least 6 forecast origins; at most the most recent 12 origins are scored.
- RMSE selects the automatic model separately for Income and Expenses. MAE,
  fold count and held-out horizon are displayed. Candidates that fail on a
  fold are excluded rather than scored on an easier subset. A manually chosen
  model is allowed without claiming that it won validation.
- Prediction intervals use benchmark residual variance or the Holt-Winters
  prediction method. Monthly 80% and 95% baseline intervals are shown when
  estimable. Their assumptions include stable historical dynamics and
  uncorrelated model errors; no aggregate-period confidence interval is claimed.
- Financial point forecasts and interval lower bounds are constrained to zero.
  Negative historical gross totals require review of accounting adjustments.
  Uploaded validation warnings remain visible in the planning-basis note.

Current files contain 2021, 2022, 2023 and 2025. With December 2025 as cutoff,
the missing 2024 year means training uses only January–December 2025. Upload
the missing year or further continuous years to make longer-history comparisons
possible. The older continuous period can be explored by choosing December 2023
as cutoff; it may include pandemic effects and needs business review.

## Scenario calculations

All percentages below are fractions. Let `I_t` and `E_t` be baseline Income and
Expenses, `a_t` indicate months on/after the effective date, `s` be payroll share,
`q` repriceable income share, `v` demand change, `p` price increase, `w` wage
increase and `c` non-payroll cost change.

```text
Scenario Income_t = I_t x (1 + a_t v) x (1 + a_t q p)

Percentage wage addition_t = a_t E_t s w
Fixed wage addition_t      = a_t annual payroll increase / 12
Per-employee addition_t    = a_t annual increase per employee x affected FTE / 12

Scenario Expenses_t = E_t + wage addition_t + a_t E_t (1 - s) c
```

These are additional changes relative to the statistical baseline, applied once,
not a monthly compounding schedule. Volume changes affect Income; staffing or
other volume-related costs must be entered separately. Percentage scenarios use
a constant assumed payroll share. Fixed wage amounts use an even monthly
allocation; they do not model individual payroll dates or pay bands.

```text
Minimum total income growth = max(0, total Scenario Expenses / total Baseline Income - 1)

Income before price = sum(I_t x (1 + a_t v))
Repriceable exposure = q x sum(I_t x (1 + a_t v), over active months)

Minimum price increase = max(0, total Scenario Expenses - Income before price)
                         / Repriceable exposure
```

If there is no repriceable exposure and a shortfall remains, the price threshold
is reported as not achievable. If costs are already covered, no positive
increase is required. The threshold includes an existing baseline shortfall;
it does not assume that the original baseline was balanced. Individual months
can still have insufficient income even when the whole period breaks even.
Displayed minimum rates round upward to 0.1 percentage points so a displayed
price requirement is not lower than the calculated threshold.
Scenario interval columns in the downloaded plan transform the baseline 80%
intervals conditional on fixed assumptions; they do not model input uncertainty.

## Implementation and verification

- `R/forecasting.R`: cleaned-data selection, continuity checks, benchmark
  prediction, rolling-origin tests and scenario equations.
- `R/forecast_plots.R`: historical/baseline/scenario charts and price sensitivity.
- `R/mod_forecasting.R`: inputs, reactive wiring, results, model checks and CSVs.
- `tests/test_forecasting.R`: aggregation, gaps, duplicates, interval availability,
  known payroll examples, delayed timing, demand, break-even boundaries and
  reactive upload/removal states.

From the project root in RStudio:

```r
source("tests/run_tests.R")
shiny::runApp()
```

No new R package is required. The models use `stats`, which ships with R, and the
existing Shiny packages. Data are parsed by the upload module once per upload;
changing business assumptions does not re-read Excel or re-fit baseline models.

## Reference

Hyndman, R.J., and Athanasopoulos, G. (2021). *Forecasting: Principles and
Practice*, 3rd edition. OTexts. Accessed 8 October 2026.

- [Benchmark methods](https://otexts.com/fpp3/simple-methods.html)
- [Rolling-origin cross-validation](https://otexts.com/fpp3/tscv.html)
- [Prediction intervals](https://otexts.com/fpp3/prediction-intervals.html)
- [Scenario forecasting](https://otexts.com/fpp3/scenarios.html)
- [Missing values and outliers](https://otexts.com/fpp3/missing-outliers.html)
