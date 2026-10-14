# Excel templates

The three upload cards offer **Download Excel template**. The blank workbooks
are stored in `www/templates/`, outside the raw-data folder. Each contains a
guide and the worksheet structure used by the dashboard's importers.

The Monthly Site template has its guide on **DASHBOARD**. The Detailed Site
and Venues templates use **Help Sheet**. These guide sheets are excluded from
business-data imports. The templates contain no original business figures.

## Fill and upload

1. Save a working copy of the appropriate template.
2. Set **Reporting year** in cell **B5** of the guide. The example year is 2026.
   For Venues, this also updates the Casual Salaries title and Court Hire years.
3. Enter reported values in amber cells. Replace example activity and item
   labels with the actual names. Grey labels and dark headings identify
   required structure. Keep calculated cells as formulas.
4. Save As using one clear report year in the filename:
   - `Monthly Site Report 2027.xlsx`
   - `Venues Site Report 2027.xlsx` or `<site> Program Report 2027.xlsx`
   - `2027 Venues Tracking Report.xlsx`
5. Save after Excel recalculates, then upload to the matching report card.
6. Review **View details** and **View Cleaned Data**. Confirm that the expected
   worksheets, new labels, periods and totals are present.

Leave unreported months blank. Enter `0` only for a confirmed zero, and do not
replace every blank with zero. When changing years, update both guide cell B5
and the filename. Do not put two different years in the filename. Uploading
an unfilled template may produce missing-data notices. It is a starting file,
not a completed report.

## Monthly Site Reports

The template includes **Monash Sport - Finance Summary** and one example
**H&F Clayton (Z07601)** site. Finance Summary links to that one site initially.

| Area | Supported addition | Keep |
| --- | --- | --- |
| Site worksheets | Copy the example worksheet and rename it to the actual site and code | The same finance and discount layout |
| Business-unit label | Replace the example site's business-unit label | `YTD Summary` in the organisation Finance Summary |
| Discounts | Insert a labelled discount amount column in the DISCOUNTS group | Month rows, `Total` and existing group boundaries |
| Finance | Add data for a new reporting year | `INCOME`, `EXPENSES`, `Budget`, `Actual`, `Variance` |
| Revenue | Extend the calculation after adding discounts | `Total Revenue (actual + opp)` and its location in the first column of the group |

When adding a site, copy the example worksheet and update its name and site
code. Extend the organisation Finance Summary formulas to include every site
exactly once. Do not add site amounts to an organisation total that already
includes them.

New discount categories must contain amounts rather than discount percentages.
Add the category name on the `DISCOUNTS` header row, retaining Jan-Dec and
`Total`. Extend the discount total and `Total Revenue` formulas so revenue
reconciliation remains available. Keep the business-unit labels above INCOME
and EXPENSES.

## Detailed Site Reports

Use a separate workbook for each site, program and year. **Finance**,
**Direct Visits** and **Indirect Visits** are required. **Memberships** and
**Enrollments** are included for reporting those measures.

| Worksheet | Supported addition | Keep |
| --- | --- | --- |
| Finance | Discount amount categories | All four finance, discount and revenue sections |
| Direct / Indirect Visits | Rename activities or insert rows before Total | Jan-Dec blocks, YTD Total and each block's Total |
| Direct / Indirect Visits | Insert a new audience in every month and YTD block | Mutually exclusive attendance counts and matching total formulas |
| Memberships / Enrollments | Insert a named numeric indicator column | Month column and Jan-Dec rows |

To add a sport, replace `Activity 1` with the actual activity name, or insert
an activity row before `Total`. Extend each column's total formula.

To add an audience such as Alumni, insert the same named column in **every
month and YTD block**, then update each block's Total. Audience categories
must be mutually exclusive. Do not count the same visit in multiple categories
or mix prices, currency amounts or percentages into attendance-count columns.

New membership and enrollment indicators can be viewed and downloaded. Their
relationship to existing indicators is not inferred automatically. The
template's `New Members` and `Total Enrollments` use the existing
Students/Staff/Community definitions. If a new audience changes the meaning
of these totals, the existing reconciliation rules also require an update.
Independent indicators retain their reported values but do not automatically
become chart or forecast targets.

## Venues Tracking Reports

The template includes all nine supported worksheets. Keep their names.

| Worksheet | Layout to retain | Supported addition or boundary |
| --- | --- | --- |
| MTD - Venues Finance | Month, Funds, Income/Expenses/Profit-Loss headings, seven measures, MTD Total and YTD Total | Insert fund rows before MTD Total and update totals. Keep measure positions and order. |
| Facility Attendance | Jan-Dec and YTD, activity sections and totals | Insert categories before each Total. Update Overall Total when adding sections. |
| Facility Hire Tracking | Paired MTD revenue and transaction columns for each month and YTD | Add category rows within the correct facility section and update totals. |
| Court Hire | Court title, Month, previous year, report year, Growth Achieved and Total | Copy complete court blocks and update Total Court Hire. |
| Membership Tracking | Repeated monthly metric groups and the final Total group | Insert the same new metric in each period group. Active is a stock, not an additive flow. |
| Online Sales | Site heading beside Jan-Dec and YTD, category rows and Total | Add categories inside a site block and update its total. Arbitrary new metric columns are not supported. |
| Merchandise Tracking | Item/Cost columns, monthly and YTD metric groups, section Total | Add item rows or repeat a named new metric in every period group. Extend totals. |
| Casual Hours | Summary, Budgeted Hours, Hours Paid, Variance, Jan-Dec and Total | Only this summary is extracted. Staff and fortnight detail remain outside scope. |
| Casual Wages | Reporting-year Casual Salaries title, Month, Budget, Actual, Variation and Total | Set guide B5 and filename to the same reporting year. Only that year's block is extracted. |

The seven MTD Finance measures, in order, are Income Budget / Actual / Variance,
Expenses Budget / Actual / Variance, and Profit/Loss Net Variance. Do not insert
arbitrary metric columns into this fixed layout. Variance is Actual minus
Budget. Net Variance is Income Variance minus Expenses Variance.

Ordinary Facility Hire and Merchandise category rows need reported values or
a known Cost. Completely unfilled category rows may be interpreted as section
headings.

Membership Active is an end-of-month count. The template uses its December
value in the Total group rather than adding monthly counts. For partial-year
analysis, inspect the latest reported month.

To change the Venues reporting year, update Help Sheet B5 and the filename,
then save in Excel. Formulas update the Court Hire year headers and the
Casual Wages year title.

## Changes requiring a new rule

- Entirely new worksheet types or renamed required worksheets.
- Changing monthly blocks to daily transactions, or swapping row and column
  layouts.
- Adding arbitrary measures to fixed-position MTD Finance, Facility Hire,
  Casual Hours or Casual Wages columns.
- New business formulas, weighted averages or changed membership subtotal
  meanings. A name alone does not define the calculation.

Known indicators use the default definitions delivered with the application.
Unknown indicators retain their reported values within supported layouts.
They do not automatically participate in calculations that require a defined
meaning. These templates cover the supported structures. If an upload reports
a structural problem, use its details to check the source workbook.

## Delivery

Include the application, `config/metric_definitions.csv` and `www/templates/`
together. The configuration file supplies internal default indicator
definitions used during import and validation. Users operate the workbook
and upload controls. Routine uploading does not require editing R code or
the configuration file.
