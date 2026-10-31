# Project report

| Path | What it is |
|---|---|
| `report.qmd` | The report: title, authors and text |
| `_quarto.yml` | How it is rendered (PDF and website settings) |
| `figures/` | Screenshots and diagrams used in the text |
| `references/` | `references.bib` (literature, edit by hand), `packages.bib` (software, rebuilt on each render), `apa.csl` (citation style) |
| `website/` | Look of the website version |
| `_extensions/report/` | Monash report template for the PDF ([quarto-monash/report](https://github.com/quarto-monash/report), CC0), with two small edits marked "Project edit" |
| `_output/` | Rendered `report.pdf` and `index.html` (website) |

Render both versions from the project root, in the RStudio Terminal:

```bash
quarto render report/report.qmd
```
