# Forecasting and practical planning

The Forecasting page uses uploaded **Monthly Site Reports** for monthly Actual
Income and Expenses, and **Venues Tracking Reports** for the recorded Casual
Salaries component. It does not forecast Net Result or treat historical Budget
as a future budget. Original Excel files and extracted data remain unchanged.

## What the user can do

1. Upload **Monthly Site Reports** in Data and open Forecasting. Add Venues
   Tracking Reports if you want a history-based casual pay estimate.
2. Choose the forecast horizon (3/6/12 months) and price rise (%). The default
   **Quick estimate** mode needs only the casual pay rise (%); its reference is
   the predicted Venues-reported payroll for the selected future period.
3. For a roster you know better than the historical reports, open **More options**
   and select **Advanced pay & hours**. Add or remove casual staff categories, then enter each
   category's current hourly wage, total monthly paid hours, pay rise and
   hours change. Optional future monthly hours override the percentage; use
   them when a new category has zero current hours.
4. Review current monthly payroll, planned monthly payroll and the difference.
   Confirm that these current costs belong to the selected Expenses scope and
   that the proposed changes are not already included in its forecast.
   Advanced mode uses only these inputs, never the quick pay rise or the
   Venues wage model.
5. Review Income, Expenses and **Income needed to cover costs**. **Price
   sensitivity** is visible beside the break-even explanation, not inside a
   collapsed details tab. **Use this minimum price** applies the rounded-up
   requirement; it does not infer a demand response to the price.
6. **More options** also contains scope, history, demand, repriceable income
   share and effective month. Edits apply directly; no separate enable switch
   is required. Closing the panel keeps all settings, including the selected
   payroll mode. The panel header shows the active payroll mode, with a shortcut
   from Your plan when Advanced payroll is active. Changing scope or the finance
   history resets the advanced cost-inclusion confirmation.
7. Read **Monthly plan** directly below the results; it is not collapsed.
   Its two CSV downloads save monthly values and assumptions. The assumptions
   CSV records the active mode and, in advanced mode, category inputs and the
   confirmation. Technical checks and methodology are documented here, not
   separate page tabs. **Reset plan**
   restores Quick estimate, a 12-month horizon, zero price/pay changes,
   automatic settings and one blank staff category.

Automatic defaults are Overall, all available history (the latest usable
continuous run), the latest complete actual month, automatic model comparison,
unchanged demand, 100% repriceable income and first-forecast-month effectiveness.
The pricing assumption is stated beside the control; it is not inferred from
the workbooks. No employee count, FTE, payroll-share estimate or statistical
model choice is needed.

Without usable Venues wages, a zero quick pay rise still permits price-only
planning. A positive rise exposes the missing-data reason rather than silently
using zero payroll. Advanced mode works without Venues wages, including for
individual sites, but still requires a valid Monthly Site Reports finance
history. Empty or invalid categories are not treated as zero costs.

## Recorded casual wage baseline and scope

- Use only `casual_wages` rows with `period_type == "Month"`, `metric ==
  "Actual"`, matching date/report/salary years and a reported finite value.
  Monthly Budget, Variance, old-year comparison blocks and Reported Total are
  not added to monthly Actuals. Overlapping wage months stop wage modelling.
- Respect the finance cutoff to avoid using later wage actuals in backtests.
  Missing months remain missing. Use the latest continuous run of at least 12
  months; exclude unreported final months and unconfirmed final zeros. Do not
  divide a partial annual sum by 12 or use it as a complete year's salary.
- Select the wage model using the same benchmark/backtest framework, then
  align its predicted dates to the finance plan. Wage observations may end
  earlier; intervening predictions are clearly identified as estimates, never
  written into the Actual table. Refuse plans more than 24 wage forecast steps
  after the last wage actual, or inconsistent payroll greater than total
  Expenses when applying any pay or paid-hours change, including a reduction.
- The cleaned wages have no site allocation. Treat them as the **Venues-reported
  payroll component**, not all staff costs. A nonzero casual adjustment
  is available only for Overall and assumes that this reported component is
  included in the organisation's Expenses. It is never extrapolated to other
  staff or apportioned to sites. Use Advanced pay & hours with site-specific categories for site planning.
  Confirm reporting scope and payroll on-cost treatment with the report owner
  before relying on this component for operational decisions.
- **Predicted casual payroll** (before your changes) is the sum of model
  predictions for the selected future months, not a chosen historical annual
  total, an individual's wage or a fixed-headcount budget. **Last actual**
  identifies the last observed month, not the future period.
- Historical payroll mixes hours, headcounts and pay rates, and its forecast
  may continue those trends. Quick mode applies **only** an additional pay rise
  to the predicted payroll. It does not assume a future headcount, derive a
  wage rate from Casual Hours or accept an extra hours-change input.
- Advanced mode uses a **user-entered current monthly roster**, not Venues
  data. Hours are total paid hours for the whole category per month; do not
  multiply by headcount again or overlap categories. One monthly roster is
  repeated across the plan, so seasonal monthly staffing is not modelled.
- Both modes apply only a payroll **difference** to the finance baseline.
  Advanced inputs do not subtract and replace the wages already embedded in
  that baseline. They are incremental planning assumptions, not a reconstruction
  of a full expense budget. Confirm that the planned changes are additional
  to the financial forecast and that scopes and cost definitions match.
- The first version covers casual hourly-paid staff. Fixed annual salaries
  and separate employer on-costs (such as superannuation or payroll tax) are
  not automatically computed. Do not mix those costs into only one side of
  the current/future comparison.
- Advanced checks require unique non-empty categories, finite non-negative
  wages and hours, a positive rate whenever paid hours exist, pay rises of
  0–100%, hours changes of -100–200%, and at most 20 categories. Explicit future
  hours must be non-negative. A percentage cannot create hours from zero.
  Current monthly casual payroll cannot exceed total monthly forecast Expenses.
  These are safeguards, not proof that the input/reference scopes match.

The supplied 2022–2024 Casual Salaries blocks have full monthly Actuals; the
2025 block ends at September (October–December are blank). Its Total is not
treated as full-year payroll. With a December 2025 finance cutoff, three wage
months are modelled for alignment before the January 2026 planning period.

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
  fold count and held-out horizon are retained internally. Candidates that fail on a
  fold are excluded rather than scored on an easier subset. Selection is always
  automatic in the page; model names and selected methods are recorded in the
  assumptions CSV. The technical checks tab is no longer displayed.
  The main chart headers do not ask users to interpret model names.
- Prediction intervals use benchmark residual variance or the Holt-Winters
  prediction method. Monthly 80% and 95% baseline intervals are shown when
  estimable. Their assumptions include stable historical dynamics and
  uncorrelated model errors; no aggregate-period confidence interval is claimed.
- Financial point forecasts and interval lower bounds are constrained to zero.
  Negative historical gross totals require review of accounting adjustments.
  Uploaded validation warnings can be reviewed in Data. Long-form notes and
  limitations are not displayed in Forecasting; missing data and invalid input
  errors still block affected calculations, and limited history has a short notice.

Current files contain 2021, 2022, 2023 and 2025. With December 2025 as cutoff,
the missing 2024 year means training uses only January–December 2025. Upload
the missing year or further continuous years to make longer-history comparisons
possible. The older continuous period can be explored by choosing December 2023
as cutoff; it may include pandemic effects and needs business review.

## Scenario calculations

All percentages below are fractions. Let `I_t` and `E_t` be baseline Income and
Expenses, `W_t` predicted Venues payroll, `a_t` indicate active months, `q`
repriceable income share, `v` demand change, `p` price rise and `w` quick pay rise.
For manual category `j`, let `u_j` be current hourly wage, `h_j` current total
monthly paid hours, `w_j` pay rise and `r_j` hours change.

```text
Scenario Income_t = I_t x (1 + a_t v) x (1 + a_t q p)

Quick payroll change_t = a_t W_t x w

Advanced current monthly payroll = sum(u_j x h_j)
Advanced future monthly payroll  = sum(u_j x (1 + w_j) x h_j x (1 + r_j))
# An entered future-hours target replaces h_j x (1 + r_j).
Advanced payroll change_t = a_t x (future monthly payroll - current monthly payroll)

Scenario Expenses_t = E_t + payroll change_t from ONE selected mode
```

Changes are additional to the statistical baseline, applied once rather than
compounding monthly. The whole current/future payroll budget is never added
again. For example, a category at $20/hour and 20 hours/month costs $400 now.
A 5% pay rise plus 20% more hours gives $504/month: add only $104/month to
Expenses. Over six active months this is $624. Quick inputs are ignored in
advanced mode, and advanced rows are ignored in quick mode, even if previously
entered values remain in the browser.

Volume changes affect Income only; they do not automatically change staff hours.
A manual monthly roster is assumed constant across the planning period; the
result is conditional on that assumption, not an estimate of future hiring.
Negative total monthly Expenses are rejected; interval lower bounds are clamped
to zero after a reduction.

The calculation helper retains its prior annual, per-employee, payroll-share,
monthly-extra-cost and aggregate paid-hours modes for existing R callers and
regression tests. These are not dashboard controls and cannot be activated by
sending unused legacy inputs to the Shiny module.

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
intervals conditional on fixed assumptions; they do not model input uncertainty
or add the uncertainty of the separately predicted casual wage component.

## Implementation and verification

- `R/forecasting.R`: cleaned-data selection, continuity checks, benchmark
  prediction, rolling-origin tests and scenario equations.
- `R/forecast_plots.R`: historical/baseline/scenario charts and price sensitivity.
- `R/mod_forecasting.R`: streamlined inputs, reactive wiring, results and CSVs.
- `tests/test_forecasting.R`: aggregation, gaps, duplicates, interval availability,
  known payroll examples, delayed timing, demand, break-even boundaries and
  reactive upload/removal states.
- `tests/test_forecasting_simple.R`: automatic selection, ignored removed/inactive
  settings, partial wage years, date alignment, site-scope protection, pay/hours
  interactions and reductions, retained R-call compatibility and no payroll double counting.
- `tests/test_forecasting_tiered.R`: category validation, optional future-hours
  targets, exact payroll differences and break-even thresholds, mode exclusivity,
  dynamic add/remove, missing wages, confirmation reset and cleared-input reset.
- `tests/test_forecasting_layout.R`: settings grouped under More options,
  directly visible monthly results, removed technical tabs, direct settings
  application, unchanged models and both CSV downloads in either payroll mode.

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
Practice*, 3rd edition. OTexts. Accessed 12 October 2026.

- [Benchmark methods](https://otexts.com/fpp3/simple-methods.html)
- [Rolling-origin cross-validation](https://otexts.com/fpp3/tscv.html)
- [Prediction intervals](https://otexts.com/fpp3/prediction-intervals.html)
- [Scenario forecasting](https://otexts.com/fpp3/scenarios.html)
- [Missing values and outliers](https://otexts.com/fpp3/missing-outliers.html)
