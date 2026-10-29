# Dashboard design

## Visual style

The dashboard follows the Monash brand palette (Monash brandbook, brand
elements > colours), styled for Monash Sport:

- Navigation: Blueberry (`#121256`) navbar with a Heritage blue (`#ABF5F9`)
  underline; the active page is underlined in Heritage blue. The official
  Monash reversed (white) logo (`www/brand/monash-logo-reversed-white.png`,
  copied from the slides template) sits on the left with a plain-text "Sport"
  label; use the supplied artwork only.
- Panels: near-square cards (4px corners) on a light page (`#F4F7FA`); KPI
  tiles have a Monash blue (`#006DAE`) left accent and Blueberry values.
- Charts: Monash blue is the primary series, orange (`#F86700`, a brand
  data-visualisation utility colour) the comparison series, Blueberry a third
  category, and Electric blue (`#285AFF`) the forecast scenario and its
  intervals. Actuals and axes use the brand greys.
- Typeface: Helvetica Neue, falling back to Helvetica and Arial.

Status colours (green, amber, red) are kept for good, warning and problem
states. Chart colours are defined once in `CHART_COLOURS` in `R/config.R`;
page colours are CSS variables in the `:root` block of `www/styles.css`.
