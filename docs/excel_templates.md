# Excel templates / 标准 Excel 模板

The three upload cards offer **Download Excel template**. The blank workbooks
are stored in `www/templates/`, not in the raw-data folder. Each contains a
guide and the worksheet structure used by the dashboard's importers.

这三份模板供填写后上传使用，不包含原始报表的业务数值。网站三个上传卡片
各有一个模板下载入口。Monthly 模板的说明位于 `DASHBOARD`，另两类位于
`Help Sheet`；说明页不会作为业务数据导入。

## Fill and upload / 填写和上传

1. Save a working copy of the appropriate template.
2. Set **Reporting year** in cell **B5** of the guide. The example year is 2026.
   For Venues, this also updates the Casual Salaries title and Court Hire years.
3. Enter reported values in amber cells. Replace example activity/item labels
   with the actual names. Grey labels and dark headings identify required
   structure; keep calculated cells as formulas.
4. Save As using one clear report year in the filename:
   - `Monthly Site Report 2027.xlsx`
   - `Venues Site Report 2027.xlsx` or `<site> Program Report 2027.xlsx`
   - `2027 Venues Tracking Report.xlsx`
5. Save after Excel recalculates, then upload to the matching report card.
6. Review **View details** and **View Cleaned Data**. Confirm that the expected
   worksheets, new labels, periods and totals are present.

填写黄色单元格，未报告的月份保持空白；只有确认数值为零时才填写 `0`。
不要把空白统一补成零。修改年份时，同时修改说明页 B5 和文件名；不要在文件名
同时写两个不同年份。空白模板直接上传后可能有数据未填写的提示，不能视为真实报表。

## Monthly Site Reports

The template includes **Monash Sport - Finance Summary** and one example
**H&F Clayton (Z07601)** site. Finance Summary links to that one site initially.

| Area | Supported addition | Keep |
| --- | --- | --- |
| Site worksheets | Copy the example worksheet, rename to the actual site and code | The same finance/discount layout |
| Business-unit label | Replace the example site's business-unit label | `YTD Summary` in the organisation Finance Summary |
| Discounts | Insert a labelled discount amount column in the DISCOUNTS group | Month rows, `Total`, and existing group boundaries |
| Finance | Add data for a new reporting year | `INCOME`, `EXPENSES`, `Budget`, `Actual`, `Variance` |
| Revenue | Extend the calculation after adding discounts | `Total Revenue (actual + opp)` and its group-first-column location |

新增站点时，复制示例工作表，并修改名称和站点代码。组织 Finance Summary 的
公式最初只链接一个示例站点；增加站点后必须更新汇总公式，覆盖全部站点且每个
站点只计入一次。不要将站点金额和已经包含它的组织汇总金额再次相加。

新增折扣必须是金额，不是折扣百分比。在 `DISCOUNTS` 标题行新增名称，保留
Jan–Dec 和 `Total`。调整折扣总计及 `Total Revenue` 公式，且保留收入核对。
保留 INCOME/EXPENSES 上方的业务单元标签。

## Detailed Site Reports

Use a separate workbook for each site/program/year. **Finance**, **Direct Visits**
and **Indirect Visits** are required. **Memberships** and **Enrollments** are
included for reporting those measures.

| Worksheet | Supported addition | Keep |
| --- | --- | --- |
| Finance | Discount amount categories | All four finance/discount/revenue sections |
| Direct / Indirect Visits | Rename activities or insert rows before Total | Jan–Dec blocks, YTD Total, each block's Total |
| Direct / Indirect Visits | Insert a new audience in every month and YTD block | Mutually exclusive attendance counts and matching total formulas |
| Memberships / Enrollments | Insert a named numeric indicator column | Month column and Jan–Dec rows |

新增体育项目：将 `Activity 1` 改为实际项目，或在 `Total` 之前插入活动行，
并扩展每列 Total 的求和范围。

新增人群：例如 Alumni，需要在 **所有月份和 YTD 分组** 中都新增同名列，
并更新每组的 Total。人群应互斥，同一个到访不能同时计入多个分类；不要在
人次区域混入平均价格、金额或百分比。

新增会员或报名指标可以显示和下载。系统不会自动推断新指标与旧指标的关系。
模板的 `New Members` 和 `Total Enrollments` 按现有 Students/Staff/Community
定义计算；若新增人群改变这些总量的含义，旧的业务合计核对规则也需要调整。
独立指标可保留原值，但不会自动成为新的图表或预测目标。

## Venues Tracking Reports

The template includes all nine supported worksheets. Keep their names.

| Worksheet | Layout to retain | Supported addition / boundary |
| --- | --- | --- |
| MTD - Venues Finance | Month, Funds, Income/Expenses/Profit-Loss headings; seven measures; MTD Total and YTD Total | Insert fund rows before MTD Total and update totals. Keep measure positions/order. |
| Facility Attendance | Jan–Dec and YTD, activity sections and totals | Insert categories before each Total; update Overall Total when adding sections. |
| Facility Hire Tracking | Paired MTD revenue and transaction columns for each month and YTD | Add category rows within the correct facility section and update totals. |
| Court Hire | Court title; Month, previous year, report year, Growth Achieved; Total | Copy complete court blocks and update Total Court Hire. |
| Membership Tracking | Repeated monthly metric groups and the final Total group | Insert the same new metric in each period group. Active is a stock, not an additive flow. |
| Online Sales | Site heading beside Jan–Dec and YTD; category rows and Total | Add categories inside a site block and update its total. Arbitrary new metric columns are not supported. |
| Merchandise Tracking | Item/Cost columns; monthly and YTD metric groups; section Total | Add item rows or repeat a named new metric in every period group. Extend totals. |
| Casual Hours | Summary, Budgeted Hours, Hours Paid, Variance; Jan–Dec and Total | Only this summary is extracted. Staff/fortnight detail remains outside scope. |
| Casual Wages | Reporting-year Casual Salaries title; Month, Budget, Actual, Variation; Total | Set guide B5 and filename to the same reporting year. Only that year's block is extracted. |

MTD Finance 的七列依次为 Income Budget / Actual / Variance、Expenses Budget /
Actual / Variance、Profit/Loss Net Variance。不能在这个固定布局中随意插入新
指标列。Variance 为 Actual − Budget；Net Variance 为 Income Variance −
Expenses Variance。

Facility Hire 和 Merchandise 的普通类别行应填写实际数据或已知 Cost；全空的
类别行可能被识别为分组标题。Membership 中的 Active 表示期末存量，模板的
Total 对它取 December 值，不能把各月人数相加；部分年度分析应查看最新已报告月份。

修改 Venues 年份时，只需更新 Help Sheet B5 和文件名，再用 Excel 保存。
`Court Hire` 的年度标题和 `Casual Wages` 的年度标题会通过公式同步。

## Changes requiring a new rule / 需要新增提取规则的变化

- Entirely new worksheet types or renamed required worksheets.
- Changing monthly blocks to daily transactions, or swapping row/column layouts.
- Adding arbitrary measures to fixed-position MTD Finance, Facility Hire,
  Casual Hours or Casual Wages columns.
- New business formulas, weighted averages or changed membership subtotal
  meanings. A name alone does not define the calculation.

交付版本没有 Metric settings。已有指标使用随应用交付的后台规则，新增未知
指标在支持结构内保留原值，不会自动参加需要明确含义的汇总或预测。模板并不
覆盖所有可能的 Excel 格式；遇到结构错误时按上传详情说明检查原文件。

## Delivery / 交付

Include the application, `config/metric_definitions.csv` and `www/templates/`
together. Users operate the workbook and upload controls; routine uploading
does not require editing R code or the configuration file.
