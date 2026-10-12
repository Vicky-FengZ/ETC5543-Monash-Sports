# Forecasting and practical planning

The Forecasting page turns uploaded monthly financial data into a practical
planning scenario. It forecasts **Income** and **Expenses** only; **Net Result**
is not a separate forecast target, and historical Budget values are not treated
as future budgets.

The page uses:

- **Monthly Site Reports** for monthly Actual Income and Expenses; and
- **Venues Tracking Reports** for the recorded Actual Casual Salaries component
  used by the optional quick payroll estimate.

Forecasting reads the cleaned data already held by the dashboard. Changing an
assumption does not re-read Excel, alter an uploaded workbook or overwrite a
cleaned table.

## Current page workflow

### 1. Upload the required data

Upload Monthly Site Reports in **Data** before opening Forecasting. At least 12
continuous months containing both Actual Income and Actual Expenses are needed.

Venues Tracking Reports are required only when the user applies a positive
casual-pay increase in **Quick estimate**. Price-only planning with a 0% casual
pay increase can still run without a usable Venues payroll history.

### 2. Set the main plan

The always-visible **Your plan** card contains three controls:

- **Forecast horizon:** 3, 6 or 12 months;
- **Casual pay increase:** the extra rate applied in Quick estimate; and
- **Price increase:** the price change applied to the repriceable share of
  forecast Income.

The default is a 12-month plan with no pay or price increase.

### 3. Use More options when needed

**More options** contains all detailed controls in one place. Closing the panel
does not reset or disable its values. Its header shows whether **Quick payroll**
or **Advanced payroll** is active.

The panel contains:

- **Payroll:** switch between Quick estimate and Advanced pay & hours;
- **Scope and history:** choose Overall or a site, the first history year, the
  last actual month and whether final zero months are confirmed actuals; and
- **Pricing and timing:** set the repriceable Income share, an explicit demand
  or volume change, and the first forecast month in which changes take effect.

These inputs apply directly; there is no separate “enable custom settings”
switch. Changing scope or financial history clears the Advanced payroll
cost-inclusion confirmation because the previous confirmation may no longer
match the selected Expenses data.

### 4. Read the results

The page shows:

- the selected planning scope, training period and payroll reference;
- **Scenario Income**, **Scenario Expenses** and **Minimum Price Increase**;
- monthly Income and Expenses charts with Actual, baseline and scenario series;
- **Income needed to cover costs**, including price sensitivity; and
- a directly visible **Monthly plan** table.

The forecast charts use the latest 36 historical months for display where
available. Missing historical months break the Actual line rather than being
visually joined. Shaded 80% and 95% ranges appear only when the financial model
has enough residual history; they describe model uncertainty, not uncertainty
in the user's payroll, price or demand assumptions.

The price-sensitivity chart keeps the selected demand and repriceable-Income
assumptions fixed. It compares total Income at different price increases with
total scenario Expenses. It is a planning relationship, not an estimated price
elasticity or a prediction of customer behaviour.

### 5. Download or reset the plan

Two downloads are available beside Monthly plan:

- `monthly_finance_scenario.csv` contains the monthly baseline, scenario,
  payroll and available interval columns used by the plan; and
- `forecast_scenario_assumptions.csv` records scope, training dates, selected
  models, pricing/timing inputs and the active payroll assumptions. In Advanced
  mode it also records every staff category and the cost-inclusion confirmation.

**Reset plan** restores Quick estimate, Overall, a 12-month horizon, 0% pay and
price changes, unchanged demand, 100% repriceable Income, first-month
effectiveness, the earliest available history year and latest available cutoff.
Final zero months return to unconfirmed, the payroll confirmation is cleared,
and Advanced payroll returns to one new category row with wage and hours blank.

## Financial history and automatic model selection

Only monthly **Actual** summary rows are used. For Overall, the Monash Sport
organisation summary is preferred so that the same activity is not counted
again through site totals. Site summaries are used only as a fallback when the
organisation summary is absent. Detail rows and YTD totals are not added to the
monthly series.

The preparation rules are:

- Income and Expenses must both exist for a month;
- duplicate site/month/measure Actual records stop forecasting;
- missing months remain missing and split the history;
- the latest complete continuous run is used for model training;
- incomplete final months are excluded;
- final months where both Income and Expenses are zero are excluded unless the
  user confirms them as actuals; and
- negative monthly Income or Expenses stop forecasting for accounting review.

At least 12 continuous months are required. With limited history, Seasonal
naive is used as a provisional reference. When enough rolling origins exist,
Seasonal naive and Naive are compared on the same held-out months. From 30
continuous months, additive Holt-Winters joins the same comparison using at
least 24 training months. The test horizon is at most 12 months and the most
recent 12 valid origins are scored.

The model with the lowest rolling-origin RMSE is selected separately for Income
and Expenses. The user does not choose a statistical model. Model comparison
continues to run internally even though the old Forecast checks tab is no longer
shown. The selected model names are saved in the assumptions CSV.

## Quick estimate payroll

Quick estimate is designed for a low-input planning pass. It forecasts the
recorded Casual Salaries component from Venues Tracking Reports and applies the
user's pay increase to those future monthly values.

Only rows meeting all of the following conditions are accepted as wage actuals:

- `period_type == "Month"`;
- `metric == "Actual"`;
- date, report year and salary year agree; and
- the value is finite and explicitly reported.

Budget, Variance, prior-year comparison blocks and Reported Total are not used
as monthly wage actuals. Blank final months are not converted to zero. By
default, final zero months are also treated as unconfirmed until the user opts
in. Missing months split the wage history, and overlapping wage months stop the
wage model.

The latest continuous run must contain at least 12 months. The same automatic
benchmark process is used for the wage series, and predictions are aligned to
the finance plan dates. Any gap between the last wage actual and the first plan
month is forecast for alignment; it is never relabelled as Actual. The wage
history is rejected when reaching the plan would require more than 24 forecast
steps.

The displayed **Predicted casual payroll** is therefore the sum of future
monthly wage predictions for the selected plan. It is not a historical annual
total, an hourly rate, a headcount budget or all employee costs. Historical
payroll can already contain changes in staffing levels, hours and rates.

Quick estimate applies only an additional pay-rate increase. It does not add a
separate hours adjustment, derive an hourly rate from Casual Hours or infer a
future headcount. Because the cleaned Casual Salaries values do not contain a
site allocation, a non-zero Quick payroll adjustment is available only for
**Overall**. Use Advanced pay & hours for a site-specific scenario.

## Advanced pay & hours

Advanced payroll uses the user's staff-category assumptions instead of the
Venues wage forecast. For each non-overlapping casual staff category, enter:

- a unique category name;
- current hourly wage;
- current total paid hours per month across the whole category;
- future pay increase;
- future hours change; and, optionally,
- an explicit future monthly-hours target.

An explicit future-hours target overrides the percentage hours change. It must
be used to create hours for a new category whose current hours are zero. The
same monthly roster is repeated through the plan, so this mode does not model
seasonal staffing or predict recruitment.

Before the scenario is calculated, the user must confirm that the current wages
belong to the selected Expenses scope and that the planned changes are not
already reflected in the financial forecast. This mode adds only the difference
between planned and current payroll. It does not add the whole payroll again or
replace the wages already embedded in baseline Expenses.

Advanced validation requires:

- 1–20 uniquely named categories;
- finite, non-negative wages and hours;
- a positive hourly wage whenever current or future paid hours are positive;
- pay increases from 0% to 100%;
- hours changes from -100% to 200%;
- non-negative explicit future hours; and
- current monthly payroll no greater than monthly baseline Expenses.

Fixed annual salaries and separate employer on-costs, such as superannuation or
payroll tax, are not calculated automatically.

## Scenario calculations

All percentages below are expressed as fractions. Let:

- `I_t` and `E_t` be baseline Income and Expenses;
- `W_t` be predicted Venues casual payroll;
- `a_t` be 1 from the selected effective month and 0 beforehand;
- `q` be the repriceable Income share;
- `v` be the user-entered demand or volume change;
- `p` be the price increase; and
- `w` be the Quick casual-pay increase.

For Advanced category `j`, let `u_j` be the current hourly wage, `h_j` the
current total monthly hours, `w_j` the pay increase and `r_j` the hours change.

```text
Scenario Income_t = I_t x (1 + a_t v) x (1 + a_t q p)

Quick payroll change_t = a_t W_t x w

Advanced current monthly payroll = sum(u_j x h_j)
Advanced future monthly payroll  = sum(u_j x (1 + w_j) x h_j x (1 + r_j))
# An explicit future-hours target replaces h_j x (1 + r_j).
Advanced payroll change_t = a_t x (future monthly payroll - current monthly payroll)

Scenario Expenses_t = E_t + payroll change_t from the one active payroll mode
```

Pay, hours, price and demand changes start in the selected effective month and
are applied once to each affected month; they do not compound month by month.
Quick and Advanced payroll are mutually exclusive. Values left in the inactive
mode do not affect the scenario.

For example, a category at $20/hour and 20 hours/month costs $400 now. A 5% pay
increase and 20% more hours produce a planned monthly cost of $504. The scenario
adds the $104 difference, not the full $504. If six forecast months are active,
the total payroll change is $624.

The whole-plan requirements are:

```text
Minimum total Income growth = max(0, total Scenario Expenses / total Baseline Income - 1)

Income before price = sum(I_t x (1 + a_t v))
Repriceable exposure = q x sum(I_t x (1 + a_t v), for active months)

Minimum price increase = max(0, total Scenario Expenses - Income before price)
                         / Repriceable exposure
```

If a shortfall remains but there is no repriceable exposure, the minimum price
is reported as **Not achievable**. If costs are already covered, the minimum is
0%. Displayed minimum rates round upward to 0.1 percentage points. **Use this
minimum price** is offered only when the calculated rate is finite and no more
than the page's 500% price-input limit.

These are whole-plan thresholds. A plan that covers costs in total can still
have individual months where Income is below Expenses. The calculation also
includes any shortfall already present in the baseline; it does not assume the
baseline was balanced.

## Errors, warnings and limitations

The page keeps user-facing messages short, but the underlying checks still run.
A blocking data or input problem appears above the results and prevents charts,
tables and downloads from presenting an invalid scenario. A short notice is
shown when the financial history has fewer than 24 continuous months.

Common blocking conditions include:

- fewer than 12 continuous finance or wage months;
- duplicate or negative Actual values;
- an old, missing or misaligned wage reference when a positive Quick pay change
  is requested;
- applying the organisation-level Quick payroll adjustment to a site;
- incomplete or invalid Advanced category inputs;
- missing Advanced cost-inclusion confirmation; and
- payroll assumptions inconsistent with total forecast Expenses.

Workbook validation warnings remain available in **Data** for review. The
Forecasting page no longer repeats the long Data notes and limitations list,
Forecast checks table or How it works tab. Removing those display sections does
not remove validation, model comparison or calculation safeguards.

Forecast intervals are conditional on historical model behaviour. They do not
include uncertainty in the separately forecast payroll component or in user
assumptions about prices, demand, pay rates, hours or staffing. The plan is a
scenario for decision support, not a guaranteed budget or causal forecast.

## Implementation and verification

- `R/forecasting.R`: cleaned-data selection, continuity rules, automatic model
  comparison, wage preparation and scenario equations.
- `R/forecast_plots.R`: Income, Expenses and price-sensitivity charts.
- `R/mod_forecasting.R`: current page layout, inputs, reactive calculations,
  displayed results and CSV downloads.
- `tests/test_forecasting.R`: finance aggregation, gaps, duplicates, models,
  intervals and known scenario calculations.
- `tests/test_forecasting_simple.R`: quick payroll, missing/partial wage data,
  date alignment and compatibility safeguards.
- `tests/test_forecasting_tiered.R`: Advanced category validation, mode
  exclusivity, dynamic categories, confirmation and reset behaviour.
- `tests/test_forecasting_layout.R`: More options grouping, directly visible
  Monthly plan, removed legacy sections and both CSV downloads.

From the project root in RStudio:

```r
source("tests/run_tests.R")
shiny::runApp()
```

No additional forecasting package is required. The models use `stats`, which
ships with R, plus the project's existing Shiny packages.

## Reference

Hyndman, R.J., and Athanasopoulos, G. (2021). *Forecasting: Principles and
Practice*, 3rd edition. OTexts. Accessed 12 October 2026.

- [Benchmark methods](https://otexts.com/fpp3/simple-methods.html)
- [Rolling-origin cross-validation](https://otexts.com/fpp3/tscv.html)
- [Prediction intervals](https://otexts.com/fpp3/prediction-intervals.html)
- [Scenario forecasting](https://otexts.com/fpp3/scenarios.html)
- [Missing values and outliers](https://otexts.com/fpp3/missing-outliers.html)
