# Figures

Screenshots and diagrams used in the report. Make screenshots with the
synthetic workbooks in `data/sample/` only: the real reports are
confidential and must not appear in the report or the repository.

| File | Figure in the report | How it was made |
|---|---|---|
| `architecture.mmd` | Source of `architecture.png` | Mermaid diagram, edited by hand |
| `architecture.png` | `fig-architecture`: how the parts of the dashboard fit together | Rendered from `architecture.mmd` with the Mermaid CLI, e.g. `mmdc -i architecture.mmd -o architecture.png -b white -s 2` |
| `before_after.png` | `fig-before-after`: one worksheet and the cleaned `finance` table | Drawn from the sample `Monthly Site Report 2025 -December.xlsx` (H&F Clayton sheet, January to March) and the rows the importer produces for those cells |
| `forecasting_results.png` | `fig-forecasting`: Forecasting page results | Screenshot of the Forecasting page with every sample workbook loaded: 12-month horizon, a Quick estimate casual pay increase that adds about $149,000 to expenses over the plan and the minimum price increase (37.7%, chosen with "Use this minimum price") |
| `price_sensitivity.png` | `fig-price-sensitivity`: income needed to cover costs | Screenshot of the same plan, lower on the Forecasting page |

To retake a screenshot after the dashboard changes, open the dashboard,
choose the `data/sample/` folder on the Data page and set the plan above.
