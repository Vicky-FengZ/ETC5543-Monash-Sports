# Chart utilities and small UI helpers: line gaps, legends, palettes, the
# navbar data chip, KPI rows and locale-independent month labels.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
})

# break_line_gaps ---------------------------------------------------------

months_2023 <- seq(as.Date("2023-10-01"), by = "month", length.out = 3)
months_2025 <- seq(as.Date("2025-01-01"), by = "month", length.out = 2)

no_gap <- break_line_gaps(months_2023, c(1, 2, 3))
stopifnot(
  identical(no_gap$x, months_2023),
  identical(no_gap$y, c(1, 2, 3))
)

# A missing year inserts exactly one NA point, so lines() breaks there.
with_gap <- break_line_gaps(c(months_2023, months_2025), c(1, 2, 3, 4, 5))
stopifnot(
  length(with_gap$y) == 6L,
  is.na(with_gap$y[[4L]]),
  identical(with_gap$y[-4L], c(1, 2, 3, 4, 5)),
  inherits(with_gap$x, "Date"),
  all(diff(as.numeric(with_gap$x)) > 0)
)

# Unsorted input gives the same result as sorted input.
shuffled <- break_line_gaps(
  c(months_2025, months_2023)[c(5, 1, 3, 2, 4)],
  c(4, 5, 1, 2, 3)[c(5, 1, 3, 2, 4)]
)
stopifnot(identical(shuffled, with_gap))

# Annual series use max_gap = 1.
annual <- break_line_gaps(c(2021, 2022, 2023, 2025), c(10, 20, 30, 50), max_gap = 1)
stopifnot(
  identical(annual$y, c(10, 20, 30, NA, 50)),
  identical(annual$x, c(2021, 2022, 2023, 2024, 2025))
)
cat("PASS break_line_gaps monthly, annual and unsorted input\n")

# chart_legend -----------------------------------------------------------

legend_file <- tempfile(fileext = ".png")
grDevices::png(legend_file, width = 600, height = 400)
plot(1:10, type = "n")
plain <- graphics::legend(
  "top", legend = c("Income", "Expenses"), horiz = TRUE, lty = 1, plot = FALSE
)
padded <- chart_legend(
  "top", legend = c("Income", "Expenses"), horiz = TRUE, lty = 1, plot = FALSE
)
positional <- chart_legend(
  "top", c("Income", "Expenses"), horiz = TRUE, lty = 1, plot = FALSE
)
vertical <- chart_legend(
  "topright", legend = c("Income", "Expenses"), lty = 1, plot = FALSE
)
vertical_plain <- graphics::legend(
  "topright", legend = c("Income", "Expenses"), lty = 1, plot = FALSE
)
grDevices::dev.off()
unlink(legend_file)

stopifnot(
  # Horizontal legends get extra room so entries cannot overlap.
  padded$rect$w > plain$rect$w,
  # Labels given as the second argument behave like graphics::legend().
  isTRUE(all.equal(positional$rect, padded$rect)),
  # Vertical legends are unchanged.
  isTRUE(all.equal(vertical$rect, vertical_plain$rect))
)
cat("PASS chart_legend headroom and positional labels\n")

# Palettes ---------------------------------------------------------------

stopifnot(
  length(chart_categorical_colours(12L)) == 12L,
  chart_categorical_colours(1L) == MONASH_COLOURS[["monash_blue"]],
  !anyDuplicated(chart_categorical_colours(length(CHART_CATEGORICAL))),
  identical(
    chart_metric_colours(c("Expenses", "Income", "Net Result")),
    unname(CHART_COLOURS[c("secondary", "primary", "tertiary")])
  ),
  !any(is.na(chart_metric_colours(c("Income", "Budget Variance"))))
)
cat("PASS brand chart palettes\n")

# Locale-independent month labels -----------------------------------------

stopifnot(
  identical(format(as.Date("2025-01-15"), "%b"), "Jan"),
  identical(format(as.Date("2025-12-01"), "%b %Y"), "Dec 2025")
)
cat("PASS English month abbreviations regardless of system locale\n")

# format_year_ranges -----------------------------------------------------

stopifnot(
  identical(format_year_ranges(c(2025, 2021, 2022, 2023, 2022)), "2021–2023, 2025"),
  identical(format_year_ranges(2024), "2024"),
  identical(format_year_ranges(integer()), ""),
  identical(format_year_ranges(c(NA, 2020)), "2020")
)
cat("PASS format_year_ranges\n")

# navbar_data_status -----------------------------------------------------

uploads <- data.frame(
  report_type = c("monthly_site", "monthly_site", "monthly_site", "venues_tracking", "site_detail"),
  original_name = c(
    "Monthly Site Report 2021 -December.xlsx",
    "Monthly Site Report 2022 -Dec.xlsx",
    "Monthly Site Report 2025 -December.xlsx",
    "2024 Venues Tracking Report.xlsx",
    "Aquatics Site Report 2023.xlsx"
  ),
  status = c("ready", "ready", "ready", "ready", "failed"),
  stringsAsFactors = FALSE
)
status_html <- as.character(navbar_data_status(uploads))
stopifnot(
  grepl("Data 2021–2022, 2025", status_html, fixed = TRUE),
  grepl("Venues Tracking Reports: 2024", status_html, fixed = TRUE),
  # Failed uploads do not count as loaded data.
  grepl("Detailed Site Reports: none", status_html, fixed = TRUE),
  !grepl("empty", status_html, fixed = TRUE)
)

venues_only <- as.character(navbar_data_status(uploads[4L, , drop = FALSE]))
stopifnot(grepl("Data 2024", venues_only, fixed = TRUE))

for (nothing in list(NULL, uploads[0, , drop = FALSE], uploads[5L, , drop = FALSE])) {
  empty_html <- as.character(navbar_data_status(nothing))
  stopifnot(
    grepl("No data loaded", empty_html, fixed = TRUE),
    grepl("navbar-data-status empty", empty_html, fixed = TRUE)
  )
}
cat("PASS navbar data status chip\n")

# make_value_box_row -----------------------------------------------------

kpi_boxes <- lapply(c("A", "B", "C"), function(title) {
  list(title = title, value = "1", subtitle = "x", icon = "people")
})
kpi_row <- make_value_box_row(c(kpi_boxes, list(NULL)))
kpi_html <- as.character(kpi_row)
count_matches <- function(pattern, text) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  sum(matches > 0L)
}
stopifnot(
  count_matches("bslib-value-box ", kpi_html) == 3L,
  # Each box is its own grid item; a single item holding all three was the
  # bug that stacked every KPI in the first column.
  count_matches('class="bslib-grid-item', kpi_html) == 3L,
  grepl("kpi-value-box-row", kpi_html, fixed = TRUE),
  grepl("minmax(min(170px, 100%), 1fr)", kpi_html, fixed = TRUE)
)
cat("PASS KPI rows give every value box its own grid item\n")

cat("PASS test_chart_and_ui_helpers.R\n")
