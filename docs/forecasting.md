# Forecasting and planning

Use Forecasting to estimate monthly **Income** and **Expenses** for Monash
Sport as a whole (**Overall**), then see how payroll changes, prices and demand
affect the plan. All amounts are in Australian dollars (AUD).

The baseline forecast uses historical Actual values. Historical Budget values
are not treated as future budgets, and the page does not forecast Net Result.

Monthly Actual Income and Expenses come from **Monthly Site Reports**. The
optional quick payroll estimate uses recorded Actual Casual Salaries from
**Venues Tracking Reports**.

Forecasting uses the cleaned data already loaded in the dashboard. Changing a
setting does not re-read Excel or change uploaded workbooks and cleaned tables.

## Using the Forecasting page

### 1. Upload the required data

Upload Monthly Site Reports in **Data** before opening Forecasting. You need at
least 12 continuous months with both Actual Income and Actual Expenses.

You need Venues Tracking Reports to apply a positive casual-pay increase in
**Quick estimate**. With a 0% casual pay increase, you can plan price changes
without a usable Venues payroll history. Manual adjustment needs only Monthly
Site Reports.

### 2. Choose a payroll method and set Your plan

Choose **Quick estimate** or **Manual adjustment** under **More options >
Payroll**. Enter the amount or percentage for the selected method in
**Your plan**.

| Control | Meaning | Accepted values |
| --- | --- | --- |
| Forecast horizon | Number of future months in the plan | 3, 6 or 12 months |
| Casual pay increase (%) | Additional pay-rate increase applied to predicted Venues casual payroll in Quick estimate | 0% to 100% |
| Monthly payroll adjustment (AUD) | Additional monthly change in Overall Expenses in Manual adjustment | Any finite positive or negative amount, or 0, subject to the monthly expense limit |
| Price increase (%) | Price increase applied to the repriceable share of forecast Income | 0% to 500% |

Quick estimate applies the pay percentage, while Manual adjustment applies the
monthly amount. The calculation uses only the selected method.

In Manual adjustment, enter a positive amount to add costs, a negative amount
to reduce costs, or 0 to leave costs unchanged. Include only changes that the
financial forecast does not already reflect.

The page starts with Quick estimate, a 12-month horizon, 0% pay increase,
0% price increase and a manual amount of 0.

### 3. Set optional assumptions

Use **More options** to choose the payroll method, history, pricing and timing.
The header shows which payroll method is active. Your settings still apply
when the panel is closed.

Under **History**, set **Use history from** to the earliest reporting year you
want to include and **Last actual month** to the latest month to consider as
historical Actual.

Within these limits, the model uses the latest continuous run of complete
months. The plan starts the following month. If final months are incomplete
or contain unconfirmed zeros, the last usable actual can be earlier than your
selected Last actual month.

When you change files or the historical period, check that your manual amount
still represents an additional change to the financial forecast.

Under **Pricing and timing**:

- **Income that can be repriced (%)** sets the share of forecast Income
  affected by the price increase, from 0% to 100%. A 10% price increase on a
  50% repriceable share increases Income by 5% in affected months when demand
  is unchanged. The default share is 100%.
- **Demand / volume change (%)** sets your assumed change in demand, from
  -99% to 200%. It affects Income without automatically changing Expenses.
  The system does not estimate how customers respond to prices. The default
  is 0%.
- **Changes take effect from** sets the month when payroll, price and demand
  adjustments start. All changes start together, in the first forecast month
  by default.

#### Review final zero months when prompted

If the reports end with zero values, a question lists the affected months.
Choose **Not yet reported** (the default) or **Confirmed zero actuals**.

Income & Expenses and Casual payroll have separate questions. The finance
question covers months where both Income and Expenses are zero. The Casual
payroll question appears only in Quick estimate and uses the finance plan's
actual cutoff.

Your confirmation applies to the current data and historical period. You may
need to confirm again after changing files, history or the payroll method.
Blank or missing months are never listed as zeros. Zero values followed by
later non-zero actuals do not need this confirmation.

### 4. Read the results

The results include:

- **Forecast starting point**, which shows Overall and the finance training period;
- **Predicted casual payroll** in Quick estimate, or **Manual payroll adjustment**
  in Manual adjustment;
- **Scenario Income**, **Scenario Expenses** and **Minimum Price Increase**;
- monthly Income and Expenses charts with Actual, baseline and scenario series;
- **Income needed to cover costs**, including **Price sensitivity**; and
- a **Monthly plan** table with baseline Income, scenario Income, baseline
  Expenses, scenario Expenses, Payroll Change and Income Needed.

In each month, Payroll Change is the amount added to or deducted from Expenses,
and Income Needed equals scenario Expenses. With a usable Quick wage reference,
the table also shows predicted and scenario casual payroll. Manual adjustment
does not provide a full payroll total.

Charts show the latest 36 historical months where available, with breaks in
the Actual line for missing months. Shaded 80% and 95% ranges appear when the
financial model has enough residual history. These ranges describe uncertainty
in the financial forecast; they do not account for uncertainty in your payroll,
price or demand assumptions.

The price-sensitivity chart compares Income at different price increases with
total scenario Expenses, keeping demand and the repriceable Income share fixed.
It shows the effect of your assumptions. It does not estimate price elasticity
or predict customer behaviour.

### 5. Download or reset the plan

Use the two download buttons beside Monthly plan:

- `monthly_finance_scenario.csv` contains the monthly baseline, scenario,
  payroll and available interval columns used by the plan; and
- `forecast_scenario_assumptions.csv` records scope, training dates, selected
  models, pricing/timing inputs and the active payroll assumptions. Manual mode
  records the signed monthly adjustment and its meaning. Quick mode records
  the pay increase and available wage-model details. Both modes record the
  finance zero-month confirmation; wage confirmation applies only to Quick.

In Manual mode, the monthly CSV leaves full casual-payroll totals blank because
the adjustment does not identify how much payroll is already inside Expenses.

**Reset plan** selects Quick estimate and restores a 12-month horizon, 0% pay
and price changes, unchanged demand, 100% repriceable Income and the first
forecast month as the effective date. It also selects the earliest available
history year and latest available cutoff, clears final-zero confirmations,
and sets the manual monthly amount to 0. All calculations cover Overall.

## Financial history and automatic model selection

The model uses monthly **Actual** summary rows. For Overall, it takes the
Monash Sport organisation summary when available and falls back to site
summaries when that summary is absent. Using one source avoids counting the
same activity through both organisation and site totals. Detail rows and YTD
totals are excluded from the monthly series.

Before fitting a model, the system:

- uses a month only when it has both Income and Expenses;
- stops forecasting if site/month/measure Actual records overlap;
- keeps missing months as gaps that split the history;
- trains on the latest continuous run of complete months;
- excludes incomplete final months;
- excludes final months where both Income and Expenses are zero unless you
  confirm them as actuals; and
- stops for accounting review if monthly Income or Expenses are negative.

The model needs at least 12 continuous months. With limited history, it uses
Seasonal naive as a provisional reference. Once there are enough rolling
origins, it compares Seasonal naive and Naive on the same held-out months.
With at least 30 continuous months, the comparison also includes additive Holt-Winters,
using at least 24 training months. The test horizon is at most 12 months, and
the comparison scores the most recent 12 valid origins.

Income and Expenses each use the model with the lowest rolling-origin RMSE.
Selection is automatic, and the assumptions CSV records both model names.

## Quick estimate payroll

Quick estimate forecasts recorded Casual Salaries from Venues Tracking Reports,
then applies your pay increase to the predicted monthly values.

A row counts as a wage actual when:

- `period_type == "Month"`;
- `metric == "Actual"`;
- date, report year and salary year agree; and
- the value is finite and explicitly reported.

The wage model excludes Budget, Variance, prior-year comparison blocks and
Reported Total. Blank final months stay blank; final zero months need your
confirmation before they count as actuals. Missing months split the wage
history. Overlapping wage months stop the model.

The latest continuous wage history must contain at least 12 months. The wage
model uses the same automatic benchmark process and predicts values for the
finance plan's dates. If the last wage actual ends before the plan starts, it
forecasts the intervening months too. Those values remain forecasts. Wage
history is rejected if reaching the end of the plan would require more than
24 forecast steps.

**Predicted casual payroll** adds up the monthly wage forecasts for your plan.
It is not a historical annual total, an hourly rate, a headcount budget or a
measure of all employee costs. The historical payroll may already reflect
changes in staffing levels, hours and rates.

Quick estimate adds the extra pay-rate cost to baseline Expenses. It does not
adjust hours separately, derive an hourly rate from Casual Hours or estimate
future headcount. Casual Salaries has no site allocation, so the Overall plan
uses it as a Venues-reported payroll component. Its coverage of all casual
employees is unverified. The system cannot automatically split total Expenses
into casual wages and other costs.

## Manual payroll adjustment

Choose **Manual adjustment** in More options and enter **Monthly payroll
adjustment (AUD)** in Your plan. Enter the net additional monthly change in
Overall Expenses, rather than a full future payroll budget.

For example, if a departure saves $4,000 per month and new hires cost $6,000
per month, enter +$2,000. Count each change once. If the forecast already
reflects a departure, do not deduct its savings again. Include any changes in
employer on-costs in your amount; the system does not calculate them.

The amount applies every month from the date selected in **Changes take effect
from**. Earlier months stay unchanged. The amount does not compound or depend
on the Venues wage series.

Enter a finite number, including zero if you want no change. A blank field
is not treated as zero. The system rejects a reduction that would make total
Expenses negative in any affected month. Otherwise, the reduction lowers
scenario Expenses and the income needed to cover costs.

You need to know the additional change relative to the displayed forecast.
Manual adjustment does not reconstruct payroll accounts, track individual
employee changes or split baseline Expenses into wages and other costs.

## Scenario calculations

All percentages below are expressed as fractions. Let:

- `I_t` and `E_t` be baseline Income and Expenses;
- `W_t` be predicted Venues casual payroll;
- `a_t` be 1 from the selected effective month and 0 beforehand;
- `q` be the repriceable Income share;
- `v` be the user-entered demand or volume change;
- `p` be the price increase;
- `w` be the Quick casual-pay increase; and
- `d` be the signed monthly amount entered for Manual adjustment.

```text
Scenario Income_t = I_t x (1 + a_t v) x (1 + a_t q p)

Quick payroll change_t = a_t W_t x w

Manual payroll change_t = a_t x d

Scenario Expenses_t = E_t + payroll change_t from the one active payroll mode
```

Payroll, price and demand changes start in the selected effective month and
apply once to each affected month, without monthly compounding. Quick estimate
and Manual adjustment are mutually exclusive, so values in the inactive mode
do not affect the scenario.

In a January-to-December plan, a +$2,000 monthly adjustment starting in July
adds $12,000 to total Expenses. A -$2,000 adjustment reduces Expenses by $12,000,
provided no affected month's total becomes negative.

The requirements for covering costs over the full plan are:

```text
Minimum total Income growth = max(0, total Scenario Expenses / total Baseline Income - 1)
# This ratio applies when total Baseline Income is positive.

Income before price = sum(I_t x (1 + a_t v))
Repriceable exposure = q x sum(I_t x (1 + a_t v), for active months)

Minimum price increase = max(0, total Scenario Expenses - Income before price)
                         / Repriceable exposure
```

When total Baseline Income is zero, minimum Income growth is 0% if scenario
Expenses are also zero. With positive Expenses, it is **Not achievable**.

If there is a shortfall and no repriceable exposure, minimum price increase
is **Not achievable**. If Income already covers costs, the minimum is 0%.
Displayed minimum rates round up to 0.1 percentage points. The **Use this
minimum price** button appears when the calculated rate is finite and within
the page's 500% price-input limit.

These thresholds cover the full plan. Individual months can still have Income
below Expenses even when the total covers costs. The calculation includes any
shortfall already present in the baseline.

## Errors, warnings and limitations

Data and input errors appear near the results and prevent invalid scenarios
from being displayed or exported. A notice appears when the financial history
has fewer than 24 continuous months.

Calculation stops when:

- the finance history has fewer than 12 continuous months, or the wage history
  has fewer than 12 continuous months for a positive Quick pay increase;
- Actual records overlap or training data contain negative monthly totals;
- a positive Quick pay increase needs wage history that is missing, misaligned
  or too old for the plan;
- a Quick payroll change would apply to Overall based only on an unverified
  partial set of site summaries;
- a manual adjustment is blank, non-finite or too large to calculate;
- a manual reduction would make any month's total Expenses negative; or
- Quick payroll assumptions are inconsistent with total forecast Expenses.

Review workbook validation warnings in **Data** before using a plan to make
a decision. The financial and wage models check their own data series. These
checks cannot establish whether Venues wages cover all employees or reconcile
those wages to a dedicated payroll account within Overall Expenses.

Forecast intervals reflect historical model behaviour. They exclude uncertainty
in the separate payroll forecast and in your assumptions about prices, demand
and payroll changes. Use the plan to explore scenarios and support decisions;
it is not a guaranteed budget or a causal forecast.

## Implementation and verification

- `R/forecasting.R`: cleaned-data selection, continuity rules, automatic model
  comparison, wage preparation and scenario equations.
- `R/forecast_plots.R`: Income, Expenses and price-sensitivity charts.
- `R/mod_forecasting.R`: page layout, inputs, reactive calculations,
  displayed results and CSV downloads.
- `tests/test_forecasting.R`: finance aggregation, gaps, duplicates, models,
  intervals and known scenario calculations.
- `tests/test_forecasting_simple.R`: quick payroll, missing/partial wage data,
  date alignment and organisation-wide planning.
- `tests/test_forecasting_manual.R`: signed adjustments, timing, cost limits,
  mode exclusivity, input validation and reset behaviour.
- `tests/test_forecasting_layout.R`: More options grouping, Monthly plan,
  organisation-wide settings and Overall metadata in both CSV downloads.
- `tests/test_forecasting_options.R`: final-zero reporting status, independent
  finance and wage confirmations, and invalidation after context changes.

From the project root in RStudio:

```r
source("tests/run_tests.R")
shiny::runApp()
```

The models use `stats`, which comes with R. The page's controls, outputs and
downloads use the project's Shiny packages.
