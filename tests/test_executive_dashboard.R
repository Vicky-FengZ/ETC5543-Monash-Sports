suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
})

# -------------------------------------------------------------------------
# Executive Dashboard regression tests: Overview + fixed Sites presentation
# -------------------------------------------------------------------------
# This test file is sourced by tests/run_tests.R after the application's
# data, calculation, plotting, registry, module and fixture helpers are loaded.
# The separate Site Detail, Site Comparison and Venues tests remain unchanged.

# Registry contract -------------------------------------------------------
expected_overview_ids <- c(
  "monthly_income_expenses", "annual_performance_trend",
  "budget_vs_actual", "revenue_capture", "key_insights"
)
expected_site_ids <- c(
  "site_finances", "site_visits", "site_memberships",
  "site_enrolments", "site_rankings"
)
expected_overview_kpi_titles <- c(
  "Total Income", "Total Expenses", "Net Result", "Budget Variance"
)
expected_site_kpi_titles <- c(
  "Total Visits",
  "Active Members",
  "Visits Growth",
  "Sites with Positive Net Result"
)

registry <- executive_registry_table()
stopifnot(
  is.data.frame(registry), nrow(registry) == 10L,
  !anyDuplicated(registry$id),
  all(c("id", "title", "category", "focus", "component_type",
        "source_group", "source_label", "required_data",
        "default_selected", "preferred_size", "supports_year_review",
        "supports_comparison") %in% names(registry)),
  identical(executive_visual_ids(focus = "Overview"), expected_overview_ids),
  identical(executive_visual_ids(focus = "Site"), expected_site_ids),
  length(executive_visual_ids(focus = "Metrics")) == 0L,
  identical(executive_default_visual_ids(category = "Finance", focus = "Overview"),
            expected_overview_ids),
  identical(executive_default_visual_ids(category = "Finance", focus = "Site"),
            expected_site_ids),
  identical(executive_visual_ids(component_type = "table"), "site_rankings"),
  identical(executive_visual_ids(component_type = "insight"), "key_insights"),
  length(executive_visual_ids(component_type = "graph")) == 8L,
  identical(executive_visual_ids(source_group = "monthly"),
            c("monthly_income_expenses", "annual_performance_trend",
              "budget_vs_actual", "revenue_capture", "site_finances",
              "site_rankings")),
  identical(executive_visual_ids(source_group = "detailed"),
            c("site_visits", "site_memberships", "site_enrolments")),
  all(registry$supports_year_review), all(registry$supports_comparison)
)

# Old Site/Facility visuals must no longer be registered in the Executive page.
retired_site_ids <- c(
  "site_performance", "site_ranking", "site_budget_performance",
  "site_revenue_opportunity", "site_discount_mix", "site_values",
  "facility_ranking", "facility_performance", "facility_discount_mix",
  "facility_discount_trend", "facility_revenue_opportunity",
  "facility_capture_rate"
)
stopifnot(!any(retired_site_ids %in% registry$id))

# Required datasets are distinct; one uploaded report family does not imply
# another is available. A missing worksheet is not a zero-valued worksheet.
empty_summary <- executive_source_availability_summary(NULL)
stopifnot(
  identical(as.character(empty_summary$source_group),
            c("monthly", "detailed", "supporting")),
  identical(as.integer(empty_summary$total_visuals), c(6L, 3L, 1L)),
  identical(as.integer(empty_summary$available_visuals), c(0L, 0L, 1L))
)

minimal_monthly <- list(monthly_reports = list(
  finance = data.frame(report_year = 2025L, site = "Aquatics Clayton", value = 1)
))
minimal_availability <- executive_registry_availability_table(minimal_monthly)
check_available <- function(id) {
  minimal_availability$available[match(id, minimal_availability$id)]
}
stopifnot(
  isTRUE(check_available("site_finances")),
  isTRUE(check_available("site_rankings")),
  !check_available("site_visits"),
  !check_available("site_memberships"),
  !check_available("site_enrolments"),
  !check_available("revenue_capture")
)

minimal_detailed <- list(site_reports = list(
  visits = data.frame(report_year = 2025L, report_unit = "Aquatics",
                      campus = "Clayton", value = 10),
  memberships = data.frame(report_year = 2025L, report_unit = "Aquatics",
                           campus = "Clayton", value = 5),
  enrollments = data.frame(report_year = 2025L, report_unit = "Aquatics",
                          campus = "Clayton", value = 4)
))
stopifnot(
  all(executive_registry_availability_table(minimal_detailed)$available[
    match(c("site_visits", "site_memberships", "site_enrolments"),
          executive_registry_table()$id)
  ]),
  !executive_visual_availability(minimal_detailed, "site_finances")$available,
  executive_visual_availability(
    minimal_detailed, "site_visits",
    context = list(view_mode = "Year Review", year = "2025",
                   comparison_year = NULL, site = "All Monash Sport")
  )$available,
  !executive_visual_availability(
    minimal_detailed, "site_visits",
    context = list(view_mode = "Comparison", year = "2025",
                   comparison_year = "2024", site = "All Monash Sport")
  )$available
)
cat("PASS Executive visual registry and reporting availability\n")

# -------------------------------------------------------------------------
# Executive Dashboard context helpers
# -------------------------------------------------------------------------

stopifnot(
  identical(
    executive_default_comparison_year(
      "2026",
      c("2024", "2025", "2026")
    ),
    "2025"
  ),

  identical(
    executive_visual_context_label(
      view_mode = "Year Review",
      year = "2026",
      site = "All Monash Sport",
      comparison_year = NULL,
      metric = "Net Result"
    ),
    "Year Review · 2026 · All Monash Sport · Net Result"
  ),

  identical(
    executive_visual_context_label(
      view_mode = "Comparison",
      year = "2026",
      site = "All Monash Sport",
      comparison_year = "2025",
      metric = "Net Result"
    ),
    "Comparison · 2026 vs 2025 · All Monash Sport · Net Result"
  )
)

stopifnot(

  identical(
    executive_context_trend_years(
      context = list(
        view_mode = "Year Review",
        year = "2026",
        comparison_year = NULL
      ),
      available_years = c(
        2023,
        2024,
        2025,
        2026,
        2027
      )
    ),
    c(
      2023L,
      2024L,
      2025L,
      2026L
    )
  ),

  identical(
    executive_context_trend_years(
      context = list(
        view_mode = "Comparison",
        year = "2026",
        comparison_year = "2024"
      ),
      available_years = c(
        2023,
        2024,
        2025,
        2026
      )
    ),
    c(
      2024L,
      2026L
    )
  )
)

# Dynamic Executive years come from the uploaded report families.

dynamic_year_data <- list(

  monthly_reports = list(

    finance = data.frame(
      report_year = c(
        2024L,
        2027L
      ),
      value = c(
        100,
        200
      ),
      stringsAsFactors = FALSE
    )
  ),

  site_reports = NULL,

  venues_reports = list(

    attendance = data.frame(
      report_year = c(
        2025L,
        2028L
      ),
      value = c(
        1000,
        1200
      ),
      stringsAsFactors = FALSE
    )
  )
)


stopifnot(

  identical(
    executive_available_years(
      dashboard_data =
        dynamic_year_data,
      focus =
        "Overview"
    ),
    c(
      2024L,
      2027L
    )
  ),

  identical(
    executive_available_years(
      dashboard_data =
        dynamic_year_data,
      focus =
        "Site"
    ),
    c(
      2024L,
      2027L
    )
  ),

  identical(
    executive_available_years(
      dashboard_data =
        dynamic_year_data,
      focus =
        "Metrics"
    ),
    c(
      2025L,
      2028L
    )
  ),

  length(
    executive_available_years(
      dashboard_data =
        dynamic_year_data,
      focus =
        "Unknown"
    )
  ) == 0L
)

stopifnot(

  identical(
    executive_comparison_year_choices(
      year = "2030",
      years = c(
        2028L,
        2029L,
        2030L,
        2031L
      )
    ),
    c(
      "2028",
      "2029",
      "2031"
    )
  ),

  identical(
    executive_comparison_year_choices(
      year = "2029",
      years = c(
        2029L,
        2030L
      )
    ),
    "2030"
  ),

  length(
    executive_comparison_year_choices(
      year = "2030",
      years = 2030L
    )
  ) == 0L,

  length(
    executive_comparison_year_choices(
      year = NULL,
      years = integer()
    )
  ) == 0L
)

# Executive Site choices come from the uploaded Monthly Site Reports.

dynamic_site_reports <- list(

  finance = data.frame(
    site = c(
      "Aquatics Clayton",
      "Esports",
      "Aquatics Clayton",
      ""
    ),
    stringsAsFactors = FALSE
  ),

  discounts = data.frame(
    site = c(
      "Caulfield",
      "Esports",
      NA_character_
    ),
    stringsAsFactors = FALSE
  ),

  total_revenue = data.frame(
    site = c(
      "Aquatics Clayton",
      "Wellness Hub"
    ),
    stringsAsFactors = FALSE
  )
)


stopifnot(

  identical(
    executive_site_choices(
      dynamic_site_reports
    ),
    c(
      "All Monash Sport",
      "Aquatics Clayton",
      "Caulfield",
      "Esports",
      "Wellness Hub"
    )
  ),

  identical(
    executive_site_choices(
      NULL
    ),
    "All Monash Sport"
  ),

  identical(
    executive_site_choices(
      list(
        finance =
          data.frame(
            value = 1,
            stringsAsFactors = FALSE
          )
      )
    ),
    "All Monash Sport"
  )
)

# Executive Site selection is reconciled when uploaded sites change.

stopifnot(

  # Keep a selected site while it is still available.
  identical(
    executive_reconcile_site_selection(
      site = "Esports",
      choices = c(
        "All Monash Sport",
        "Aquatics Clayton",
        "Esports"
      )
    ),
    "Esports"
  ),

  # A removed site falls back safely.
  identical(
    executive_reconcile_site_selection(
      site = "Esports",
      choices = c(
        "All Monash Sport",
        "Aquatics Clayton"
      )
    ),
    "All Monash Sport"
  ),

  # Missing browser state also falls back safely.
  identical(
    executive_reconcile_site_selection(
      site = NULL,
      choices = c(
        "All Monash Sport",
        "Aquatics Clayton"
      )
    ),
    "All Monash Sport"
  )
)

# Executive year selection is reconciled when available data changes.

  stopifnot(

    # Keep the current year when it is still available.
    identical(
      executive_reconcile_year_selection(
        year = "2029",
        years = c(
          2028L,
          2029L,
          2030L
        )
      ),
      "2029"
    ),

    # If the selected year disappears, use the latest available year.
    identical(
      executive_reconcile_year_selection(
        year = "2027",
        years = c(
          2028L,
          2029L,
          2030L
        )
      ),
      "2030"
    ),

    # A missing selection also defaults to the latest available year.
    identical(
      executive_reconcile_year_selection(
        year = NULL,
        years = c(
          2028L,
          2029L,
          2030L
        )
      ),
      "2030"
    ),

    # An empty browser value behaves like no selection.
    identical(
      executive_reconcile_year_selection(
        year = "",
        years = c(
          2028L,
          2029L,
          2030L
        )
      ),
      "2030"
    ),

    # No uploaded years means there is no valid selection.
    identical(
      executive_reconcile_year_selection(
        year = "2030",
        years = integer()
      ),
      ""
    )
  )

# Executive comparison selection is reconciled when data changes.

stopifnot(

  # Keep the comparison year when it is still valid.
  identical(
    executive_reconcile_comparison_year(
      year = "2030",
      comparison_year = "2028",
      years = c(
        2028L,
        2029L,
        2030L
      )
    ),
    "2028"
  ),

  # If the comparison year disappears, select the default valid comparison.
  identical(
    executive_reconcile_comparison_year(
      year = "2030",
      comparison_year = "2028",
      years = c(
        2029L,
        2030L
      )
    ),
    "2029"
  ),

  # Missing comparison selection also receives a valid default.
  identical(
    executive_reconcile_comparison_year(
      year = "2030",
      comparison_year = NULL,
      years = c(
        2028L,
        2029L,
        2030L
      )
    ),
    "2029"
  ),

  # The current year can never also be the comparison year.
  identical(
    executive_reconcile_comparison_year(
      year = "2030",
      comparison_year = "2030",
      years = c(
        2028L,
        2029L,
        2030L
      )
    ),
    "2029"
  ),

  # With only one available year, comparison is impossible.
  identical(
    executive_reconcile_comparison_year(
      year = "2030",
      comparison_year = "2029",
      years = 2030L
    ),
    ""
  )
)

# Executive View mode is reconciled against available years.

stopifnot(

  # Comparison remains valid when at least two years exist.
  identical(
    executive_reconcile_view_mode(
      view_mode = "Comparison",
      years = c(
        2029L,
        2030L
      )
    ),
    "Comparison"
  ),

  # Year Review remains valid when multiple years exist.
  identical(
    executive_reconcile_view_mode(
      view_mode = "Year Review",
      years = c(
        2029L,
        2030L
      )
    ),
    "Year Review"
  ),

  # Comparison is impossible when only one year remains.
  identical(
    executive_reconcile_view_mode(
      view_mode = "Comparison",
      years = 2030L
    ),
    "Year Review"
  ),

  # No data also falls back safely to Year Review.
  identical(
    executive_reconcile_view_mode(
      view_mode = "Comparison",
      years = integer()
    ),
    "Year Review"
  ),

  # Missing or invalid browser state also falls back safely.
  identical(
    executive_reconcile_view_mode(
      view_mode = NULL,
      years = c(
        2029L,
        2030L
      )
    ),
    "Year Review"
  ),

  identical(
    executive_reconcile_view_mode(
      view_mode = "Something Else",
      years = c(
        2029L,
        2030L
      )
    ),
    "Year Review"
  )
)

# The Executive module uses the correct dynamic year family
# for each dashboard focus.

dynamic_selector_data <- shiny::reactiveVal(
  list(

    monthly_reports = list(

      finance = data.frame(
        report_year = c(
          2029L,
          2030L
        ),
        site = c(
          "Aquatics Clayton",
          "Aquatics Clayton"
        ),
        value = c(
          100,
          200
        ),
        stringsAsFactors = FALSE
      )
    ),

    site_reports = NULL,

    venues_reports = list(

      attendance = data.frame(
        report_year = c(
          2028L,
          2031L
        ),
        value = c(
          1000,
          1200
        ),
        stringsAsFactors = FALSE
      )
    )
  )
)

shiny::testServer(
  mod_executive_dashboard_server,
  args = list(
    data = dynamic_selector_data
  ),
  {

    session$setInputs(
      dashboard_section = "finance"
    )

    session$flushReact()


    stopifnot(
      identical(
        available_executive_years(),
        c(
          2029L,
          2030L
        )
      )
    )

    year_selector_html <-
      output$year_selector$html


    stopifnot(
      grepl(
        "2029",
        year_selector_html,
        fixed = TRUE
      ),
      grepl(
        "2030",
        year_selector_html,
        fixed = TRUE
      )
    )

    session$setInputs(
      year = "2030"
    )

    session$flushReact()


    comparison_selector_html <-
      output$comparison_year_selector$html


    stopifnot(
      grepl(
        "2029",
        comparison_selector_html,
        fixed = TRUE
      ),

      !grepl(
        'value="2030"',
        comparison_selector_html,
        fixed = TRUE
      )
    )

    # Removing the selected year must reconcile the Year selector
    # to the latest year that is still available.

    dynamic_selector_data(
      list(

        monthly_reports = list(

          finance = data.frame(
            report_year = 2029L,
            value = 100,
            stringsAsFactors = FALSE
          )
        ),

        site_reports = NULL,

        venues_reports = list(

          attendance = data.frame(
            report_year = c(
              2028L,
              2031L
            ),
            value = c(
              1000,
              1200
            ),
            stringsAsFactors = FALSE
          )
        )
      )
    )

    session$flushReact()

    reconciled_year_selector_html <-
      output$year_selector$html

    reconciled_view_selector_html <-
      output$view_mode_selector$html

    stopifnot(
      grepl(
        "2029",
        reconciled_year_selector_html,
        fixed = TRUE
      ),

      !grepl(
        "2030",
        reconciled_year_selector_html,
        fixed = TRUE
      ),

      grepl(
        "Year Review",
        reconciled_view_selector_html,
        fixed = TRUE
      ),

      !grepl(
        "Comparison",
        reconciled_view_selector_html,
        fixed = TRUE
      )
    )


    # Even if stale browser state still says Comparison,
    # the server context must fall back to Year Review.

    session$setInputs(
      view_mode = "Comparison",
      comparison_year = "2028"
    )

    session$flushReact()

    single_year_context <-
      executive_context()

    stopifnot(
      identical(
        single_year_context$view_mode,
        "Year Review"
      ),

      is.null(
        single_year_context$comparison_year
      )
    )


    # Restore the original fixture for the remaining focus tests.

    dynamic_selector_data(
      list(

        monthly_reports = list(

          finance = data.frame(
            report_year = c(
              2029L,
              2030L
            ),
            site = c(
              "Aquatics Clayton",
              "Aquatics Clayton"
            ),
            value = c(
              100,
              200
            ),
            stringsAsFactors = FALSE
          )
        ),

        site_reports = NULL,

        venues_reports = list(

          attendance = data.frame(
            report_year = c(
              2028L,
              2031L
            ),
            value = c(
              1000,
              1200
            ),
            stringsAsFactors = FALSE
          )
        )
      )
    )

    session$flushReact()

    # ---------------------------------------------------------------------
    # Removed comparison years must reconcile to a valid remaining year.
    # ---------------------------------------------------------------------

    # First make 2028 a genuinely available comparison year.
    dynamic_selector_data(
      list(

        monthly_reports = list(

          finance = data.frame(
            report_year = c(
              2028L,
              2029L,
              2030L
            ),
            value = c(
              50,
              100,
              200
            ),
            stringsAsFactors = FALSE
          )
        ),

        site_reports = NULL,

        venues_reports = list(

          attendance = data.frame(
            report_year = c(
              2028L,
              2031L
            ),
            value = c(
              1000,
              1200
            ),
            stringsAsFactors = FALSE
          )
        )
      )
    )

    session$flushReact()


    session$setInputs(
      year = "2030",
      comparison_year = "2028"
    )

    session$flushReact()


    # Now remove 2028 while keeping the current year 2030.
    dynamic_selector_data(
      list(

        monthly_reports = list(

          finance = data.frame(
            report_year = c(
              2029L,
              2030L
            ),
            value = c(
              100,
              200
            ),
            stringsAsFactors = FALSE
          )
        ),

        site_reports = NULL,

        venues_reports = list(

          attendance = data.frame(
            report_year = c(
              2028L,
              2031L
            ),
            value = c(
              1000,
              1200
            ),
            stringsAsFactors = FALSE
          )
        )
      )
    )

    session$flushReact()


    reconciled_comparison_selector_html <-
      output$comparison_year_selector$html


    stopifnot(
      grepl(
        'value="2029" selected',
        reconciled_comparison_selector_html,
        fixed = TRUE
      ),

      !grepl(
        'value="2028"',
        reconciled_comparison_selector_html,
        fixed = TRUE
      )
    )

    session$setInputs(
      dashboard_section = "site"
    )

    session$flushReact()

    stopifnot(
      identical(
        available_executive_years(),
        c(
          2029L,
          2030L
        )
      )
    )


    session$setInputs(
      dashboard_section = "metrics"
    )

    session$flushReact()


    stopifnot(
      identical(
        available_executive_years(),
        c(
          2028L,
          2031L
        )
      )
    )

    metrics_year_selector_html <-
      output$year_selector$html


    stopifnot(
      grepl(
        "2028",
        metrics_year_selector_html,
        fixed = TRUE
      ),
      grepl(
        "2031",
        metrics_year_selector_html,
        fixed = TRUE
      )
    )
  }
)


cat(
  "PASS Executive Dashboard context helpers\n"
)

# -------------------------------------------------------------------------
# Fixed Sites presentation UI and graph contracts
# -------------------------------------------------------------------------
site_ui_html <- htmltools::renderTags(
  executive_site_dashboard_ui(shiny::NS("executive"), "Year Review · 2025")
)$html
rank_ui_html <- htmltools::renderTags(
  executive_site_rankings_card_ui(shiny::NS("executive"))
)$html
stopifnot(
  grepl("executive-site-grid", site_ui_html, fixed = TRUE),
  grepl("executive-site-rankings-table", rank_ui_html, fixed = TRUE),
  grepl("executive-site-rankings-card", rank_ui_html, fixed = TRUE),
  all(vapply(c("site_finances", "site_visits", "site_memberships",
               "site_enrolments"), function(id) {
    grepl(paste0("executive-executive_plot_", id),
          site_ui_html, fixed = TRUE)
  }, logical(1))),
  grepl("executive-site_rankings_table", rank_ui_html, fixed = TRUE),
  !grepl("choose_visuals", site_ui_html, fixed = TRUE)
)

# Build 2025 and 2026 finance for three reporting sites. The first site also
# has operational reports. The organisation-wide total is deliberately absent;
# the finance calculation must derive it from site summaries.
finance_fixture <- expand.grid(
  report_year = c(2025L, 2026L),
  site = c("Health & Fitness Clayton", "Peninsula", "Participation Programs"),
  metric = c("Income", "Expenses"),
  value_type = c("Actual", "Budget"),
  stringsAsFactors = FALSE
)
finance_fixture$is_summary <- TRUE
finance_fixture$date <- as.Date(sprintf("%d-12-01", finance_fixture$report_year))
finance_fixture$value <- ifelse(
  finance_fixture$metric == "Income", 1000, 650
) + ifelse(finance_fixture$value_type == "Budget", -100, 0) +
  ifelse(finance_fixture$report_year == 2026L, 150, 0) +
  ifelse(finance_fixture$site == "Peninsula", 100, 0)

site_fixture <- make_site_detail_fixture()
programme_enrolments <- data.frame(
  report_year = 2025L, report_unit = "Participation Programs",
  campus = "Clayton", date = as.Date(c("2025-01-01", "2025-03-01",
                                           "2025-01-01", "2025-03-01")),
  month = c(1L, 3L, 1L, 3L),
  metric = c("Total Enrolments", "Total Enrolments", "Target", "Target"),
  value = c(15, 20, 30, 40), stringsAsFactors = FALSE
)
site_fixture$enrollments <- rbind(site_fixture$enrollments,
                                  programme_enrolments)

executive_fixture <- shiny::reactiveVal(list(
  monthly_reports = list(finance = finance_fixture),
  site_reports = site_fixture,
  venues_reports = NULL
))

# Calculation and chart preconditions ------------------------------------
annual_site_finance <- calculate_site_annual_finance(
  list(finance = finance_fixture)
)
stopifnot(
  nrow(annual_site_finance) == 6L,
  all(annual_site_finance$net_actual ==
        annual_site_finance$income_actual - annual_site_finance$expenses_actual),
  !"Monash Sport" %in% annual_site_finance$site,
  length(unique(annual_site_finance$site)) == 3L
)

# Test chart helpers with deliberately incomplete data. Missing Direct or
# Indirect must stay missing; a user cannot infer a verified Total Visits.
visits_fixture <- data.frame(
  report_year = c(2025L, 2025L),
  site = c("Health & Fitness Clayton", "Peninsula"),
  direct = c(315, 40), indirect = c(30, NA_real_),
  stringsAsFactors = FALSE
)
membership_fixture <- data.frame(
  report_year = 2025L, site = "Health & Fitness Clayton",
  active_members = 510, stringsAsFactors = FALSE
)
enrolment_fixture <- data.frame(
  report_year = c(2025L, 2025L),
  site = c("Health & Fitness Clayton", "Participation Programs"),
  measure = c("Total Active", "Total Enrolments"),
  actual = c(90, 20), target = c(100, 40),
  stringsAsFactors = FALSE
)

stopifnot(
  identical(executive_site_plot_labels(visits_fixture), visits_fixture$site),
  identical(executive_site_plot_labels(
    rbind(visits_fixture, transform(visits_fixture, report_year = 2026L)))[[1L]],
    "Health & Fitness Clayton (2025)"),
  identical(executive_site_format_count(1234), "1,234"),
  identical(executive_site_format_count(NA_real_), "Not reported"),
  isTRUE(all.equal(site_detail_visit_value(site_fixture$visits[
    site_fixture$visits$report_unit == "Health & Fitness" &
      site_fixture$visits$report_year == 2025L, , drop = FALSE
  ], "Direct", "Total"), 315)),
  identical(site_detail_latest_membership_value(site_fixture$memberships,
                                                 "Total Active"), 510)
)

# Every new Executive Site plot must be able to draw its documented input.
plot_file <- tempfile(fileext = ".pdf")
grDevices::pdf(plot_file, width = 12, height = 8, onefile = TRUE)
plot_error <- tryCatch({
  plot_executive_site_finances(
    annual_site_finance[annual_site_finance$report_year == 2025L, ]
  )
  plot_executive_site_visits(visits_fixture)
  plot_executive_site_memberships(membership_fixture)
  plot_executive_site_enrolments(enrolment_fixture)
  NULL
}, error = function(e) e)
grDevices::dev.off()
unlink(plot_file)
if (!is.null(plot_error)) stop(plot_error)

# Explicitly reject duplicate site/year inputs, which would overcount a site.
duplicate_error <- tryCatch({
  plot_executive_site_visits(rbind(visits_fixture, visits_fixture[1L, ]))
  NULL
}, error = function(e) conditionMessage(e))
stopifnot(is.character(duplicate_error),
          grepl("duplicate", duplicate_error, ignore.case = TRUE))
cat("PASS fixed Sites visual layout and plot inputs\n")

# -------------------------------------------------------------------------
# Executive module regression: protect Overview and verify Site behaviour
# -------------------------------------------------------------------------
shiny::testServer(
  mod_executive_dashboard_server,
  args = list(data = executive_fixture),
  {
    session$setInputs(dashboard_section = "finance")
    session$flushReact()
    stopifnot(identical(active_focus(), "Overview"),
              identical(active_category(), "Finance"),
              identical(available_executive_years(), c(2025L, 2026L)))

    session$setInputs(view_mode = "Year Review", year = "2025",
                      site = "Peninsula")
    session$flushReact()
    overview_context <- executive_context()
    overview_kpis <- executive_kpi_specs()
    stopifnot(
      identical(overview_context$site, "All Monash Sport"),
      identical(overview_context$view_mode, "Year Review"),
      is.null(overview_context$comparison_year),
      identical(vapply(overview_kpis, `[[`, character(1), "title"),
                expected_overview_kpi_titles),
      identical(vapply(overview_kpis, `[[`, character(1), "output_id"),
                c("kpi_total_income", "kpi_total_expenses",
                  "kpi_net_result", "kpi_budget_variance"))
    )

    # The displayed financial KPIs must be backed by the same annual figures
    # as the Overview charts, not independently recomputed Site totals.
    kpi_finance <- executive_kpi_finance()
    stopifnot(
      !is.null(kpi_finance), nrow(kpi_finance) == 1L,
      kpi_finance$report_year[[1L]] == 2025L,
      isTRUE(all.equal(kpi_finance$net_actual[[1L]],
                       kpi_finance$income_actual[[1L]] -
                         kpi_finance$expenses_actual[[1L]]))
    )

    # Overview's selected-year figures and historical series remain separate.
    stopifnot(
      identical(executive_selected_years(), 2025L),
      all(executive_monthly_finance()$report_year == 2025L),
      all(executive_annual_finance()$report_year <= 2025L)
    )

    session$setInputs(view_mode = "Comparison", year = "2026",
                      comparison_year = "2025")
    session$flushReact()
    stopifnot(
      identical(executive_context_years(executive_context()),
                c(2025L, 2026L)),
      identical(executive_context()$site, "All Monash Sport"),
      identical(executive_context()$view_mode, "Comparison"),
      length(executive_kpi_specs()) == 4L
    )
    comparison_finance <- executive_kpi_comparison_finance()
    stopifnot(
      !is.null(comparison_finance), nrow(comparison_finance) == 1L,
      comparison_finance$report_year[[1L]] == 2025L,
      executive_kpi_finance()$report_year[[1L]] == 2026L
    )
    cat("PASS Overview KPI and Year Review/Comparison regression\n")


    # ---------------------------------------------------------------------
    # Retained Overview regression: exact KPI labels and period boundaries
    # ---------------------------------------------------------------------
    # A UI that merely renders four cards could still display the wrong
    # comparison values. Verify the signed differences against report data.
    current_finance <- executive_kpi_finance()
    previous_finance <- executive_kpi_comparison_finance()
    comparison_specs <- executive_kpi_specs()
    overview_fields <- c("income_actual", "expenses_actual", "net_actual",
                         "budget_variance")
    stopifnot(!is.null(current_finance), !is.null(previous_finance),
              length(comparison_specs) == length(overview_fields))

    for (index in seq_along(overview_fields)) {
      field <- overview_fields[[index]]
      current_value <- current_finance[[field]][[1L]]
      previous_value <- previous_finance[[field]][[1L]]
      actual_label <- comparison_specs[[index]]$comparison
      if (is.finite(current_value) && is.finite(previous_value)) {
        difference <- current_value - previous_value
        expected_label <- paste0(
          if (difference > 0) "+" else "",
          format_compact_currency(difference), " vs 2025"
        )
        stopifnot(identical(actual_label, expected_label))
      } else {
        stopifnot(is.null(actual_label))
      }
    }

    # Year Review must use only its selected year for monthly plots, but
    # historical years up to that year for annual charts. Switching back to
    # Comparison must use the two selected years, without leaking context.
    session$setInputs(view_mode = "Year Review", year = "2026")
    session$flushReact()
    stopifnot(
      identical(executive_selected_years(), 2026L),
      all(executive_monthly_finance()$report_year == 2026L),
      all(executive_annual_finance()$report_year <= 2026L),
      is.null(executive_kpi_comparison_finance()),
      all(vapply(executive_kpi_specs(), function(x) is.null(x$comparison),
                 logical(1)))
    )

    year_review_finance <- executive_kpi_finance()
    year_review_specs <- executive_kpi_specs()
    for (index in seq_along(overview_fields)) {
      value <- year_review_finance[[overview_fields[[index]]]][[1L]]
      expected_value <- if (is.finite(value)) {
        format_compact_currency(value)
      } else {
        "—"
      }
      stopifnot(identical(year_review_specs[[index]]$value, expected_value))
    }

    session$setInputs(view_mode = "Comparison", year = "2026",
                      comparison_year = "2025")
    session$flushReact()
    stopifnot(
      identical(executive_context()$view_mode, "Comparison"),
      identical(executive_context()$site, "All Monash Sport"),
      identical(executive_selected_years(), c(2025L, 2026L)),
      all(executive_monthly_finance()$report_year %in% c(2025L, 2026L)),
      all(executive_annual_finance()$report_year %in% c(2025L, 2026L))
    )
    cat("PASS detailed Overview KPI and period-boundary regression\n")

    # Site: all reporting units are visible without an individual site filter.
    session$setInputs(dashboard_section = "site")
    session$flushReact()
    session$setInputs(view_mode = "Year Review", year = "2025")
    session$flushReact()
    stopifnot(
      identical(active_focus(), "Site"),
      identical(executive_context()$site, "All Monash Sport"),
      identical(executive_selected_years(), 2025L),
      nrow(executive_site_financial_rows()) == 3L,
      length(unique(executive_site_financial_rows()$site)) == 3L
    )

    operational <- executive_site_operational_rows()
    hf <- operational[operational$report_unit == "Health & Fitness" &
                        operational$report_year == 2025L, , drop = FALSE]
    prog <- operational[operational$report_unit == "Participation Programs" &
                          operational$report_year == 2025L, , drop = FALSE]
    stopifnot(
      nrow(hf) == 1L, nrow(prog) == 1L,
      isTRUE(all.equal(hf$direct, 315)),
      isTRUE(all.equal(hf$indirect, 30)),
      isTRUE(all.equal(hf$active_members, 510)),
      isTRUE(all.equal(hf$actual, 90)),
      identical(hf$measure[[1L]], "Total Active"),
      isTRUE(all.equal(prog$actual, 20)),
      identical(prog$measure[[1L]], "Total Enrolments")
    )

    current <- executive_site_snapshot_kpis(2025L)
    site_kpis <- executive_kpi_specs()
    stopifnot(
      isTRUE(all.equal(current$visits, 345)),
      current$visit_sites == 1L,   # Peninsula reports Direct only.
      isTRUE(all.equal(current$members, 510)),
      current$member_sites == 1L,
      isTRUE(all.equal(current$programmes, 20)),
      current$programme_sites == 1L,
      current$sites >= 3L,
      identical(vapply(site_kpis, `[[`, character(1), "title"),
                expected_site_kpi_titles),
      length(site_kpis) == 4L,
      identical(site_kpis[[1L]]$value, "345"),
      identical(site_kpis[[2L]]$value, "510"),
      # The fixture has no 2024 comparison visits.
      identical(site_kpis[[3L]]$value, "—"),
      # All three financial sites have a positive Net Result.
      identical(site_kpis[[4L]]$value, "3 / 3")
    )

    # Only one selected year is used for the Site board in Year Review.
    stopifnot(
      all(executive_site_visits_rows()$report_year == 2025L),
      all(executive_site_membership_rows()$report_year == 2025L),
      all(executive_site_enrolment_rows()$report_year == 2025L)
    )

    # Site Comparison must use exactly the two selected years; rankings are
    # calculated within each year, not across the two years combined.
    session$setInputs(view_mode = "Comparison", year = "2026",
                      comparison_year = "2025")
    session$flushReact()
    stopifnot(
      identical(executive_context_years(executive_context()),
                c(2025L, 2026L)),
      all(executive_site_financial_rows()$report_year %in% c(2025L, 2026L)),
      nrow(executive_site_financial_rows()) == 6L,
      length(executive_kpi_specs()) == 4L
    )

    # The set of sites contributing visits changes between years. A numerical
    # comparison would be misleading; the KPI must acknowledge that coverage.
    compare_site_kpis <- executive_kpi_specs()
    stopifnot(grepl("Reporting coverage differs",
                    compare_site_kpis[[1L]]$comparison, fixed = TRUE))
    cat("PASS Sites operational measures, rankings and comparison context\n")
  }
)


# -------------------------------------------------------------------------
# Newly discovered reporting units and future years remain supported
# -------------------------------------------------------------------------
# The Executive Sites dashboard must not hard-code the six sample site names,
# nor ignore future reporting years not listed in the current templates.
future_finance <- finance_fixture[
  finance_fixture$report_year == 2026L &
    finance_fixture$site == "Peninsula", , drop = FALSE
]
future_finance$report_year <- 2027L
future_finance$site <- "Esports"
future_finance$date <- as.Date("2027-12-01")
future_reports <- shiny::reactiveVal(list(
  monthly_reports = list(finance = rbind(finance_fixture, future_finance)),
  site_reports = NULL,
  venues_reports = NULL
))

shiny::testServer(mod_executive_dashboard_server,
                  args = list(data = future_reports), {
  session$setInputs(dashboard_section = "site")
  session$flushReact()
  stopifnot(2027L %in% available_executive_years(),
            "Esports" %in% available_executive_sites())

  session$setInputs(view_mode = "Year Review", year = "2027")
  session$flushReact()
  stopifnot(
    identical(executive_context()$year, "2027"),
    identical(executive_context()$site, "All Monash Sport"),
    "Esports" %in% executive_site_financial_rows()$site,
    all(executive_site_financial_rows()$report_year == 2027L)
  )
  # Replacing the uploaded set must reconcile a now-missing selected year.
  future_reports(list(monthly_reports = list(finance = finance_fixture),
                      site_reports = NULL, venues_reports = NULL))
  session$flushReact()
  stopifnot(
    !2027L %in% available_executive_years(),
    !"Esports" %in% available_executive_sites()
  )
})
cat("PASS newly discovered Site and future-year regression\n")

cat("PASS test_executive_dashboard.R\n")


# -------------------------------------------------------------------------
# Executive Sites presentation regressions (card legends, hover, insights)
# -------------------------------------------------------------------------
site_ui_html <- paste(as.character(executive_site_dashboard_ui(
  shiny::NS("test-site"), "Year Review · 2025 · All Sites"
)), collapse = "")
site_insights_html <- paste(as.character(
  executive_site_insights_card_ui(shiny::NS("test-site"))
), collapse = "")
stopifnot(
  grepl("executive-site-header-legend", site_ui_html, fixed = TRUE),
  grepl("executive_hover_site_finances", site_ui_html, fixed = TRUE),
  grepl("executive_hover_site_visits", site_ui_html, fixed = TRUE),
  grepl("executive_hover_site_memberships", site_ui_html, fixed = TRUE),
  grepl("executive_hover_site_enrolments", site_ui_html, fixed = TRUE),
  grepl("executive_hover_value_site_finances", site_ui_html, fixed = TRUE),
  grepl("site_key_insights", site_insights_html, fixed = TRUE)
)

shiny::testServer(mod_executive_dashboard_server,
                  args = list(data = executive_fixture), {
  session$setInputs(dashboard_section = "site", view_mode = "Year Review",
                    year = "2025")
  session$flushReact()
  rows <- executive_site_visits_rows()
  prepared <- executive_site_hover_rows(rows, "site_visits")
  text <- executive_site_hover_text(rows,
                                    list(x = 1, y = nrow(prepared)),
                                    "site_visits")
  stopifnot(
    grepl("Direct", text, fixed = TRUE),
    grepl("Indirect", text, fixed = TRUE),
    grepl("Total", text, fixed = TRUE)
  )
  stopifnot(grepl("Hover over", executive_site_hover_text(rows, NULL,
                "site_visits"), fixed = TRUE))
  insights <- output$site_key_insights$html
  stopifnot(grepl("Highest Net Result", insights, fixed = TRUE),
            grepl("Most visits", insights, fixed = TRUE))
})
cat("PASS Sites legends, exact-value hover and site-specific insights\n")


# -------------------------------------------------------------------------
# Executive Venues: layout, hover controls and facility coverage
# -------------------------------------------------------------------------

venues_ns <- shiny::NS("test-venues")

# Check the four presentation charts and hover controls.
venues_ui <- paste(
  as.character(
    executive_venues_dashboard_ui(
      venues_ns,
      "Year Review · 2025 · All Venues",
      2025L
    )
  ),
  collapse = ""
)

venues_chart_ids <- c(
  "venues_attendance",
  "venues_facility_hire",
  "venues_wages",
  "venues_merchandise"
)

for (chart_id in venues_chart_ids) {

  stopifnot(
    grepl(
      paste0("executive_plot_", chart_id),
      venues_ui,
      fixed = TRUE
    ),
    grepl(
      paste0("executive_hover_", chart_id),
      venues_ui,
      fixed = TRUE
    ),
    grepl(
      paste0("executive_hover_value_", chart_id),
      venues_ui,
      fixed = TRUE
    )
  )
}

# Check that Comparison displays both reporting years.
venues_comparison_ui <- paste(
  as.character(
    executive_venues_dashboard_ui(
      venues_ns,
      "Comparison · 2025 vs 2024 · All Venues",
      c(2024L, 2025L)
    )
  ),
  collapse = ""
)

stopifnot(
  grepl("2024", venues_comparison_ui, fixed = TRUE),
  grepl("2025", venues_comparison_ui, fixed = TRUE)
)

# Check the Operational Summary and Key Insights cards.
venues_support_ui <- paste(
  as.character(
    executive_venues_supporting_ui(venues_ns)
  ),
  collapse = ""
)

stopifnot(
  grepl("venues_summary_table", venues_support_ui, fixed = TRUE),
  grepl("venues_key_insights", venues_support_ui, fixed = TRUE)
)

cat("PASS Venues layout, comparison and hover controls\n")


# -------------------------------------------------------------------------
# Facility Hire: all reported facilities must be represented
# -------------------------------------------------------------------------

facility_test <- expand.grid(
  report_year = c(2024L, 2025L),
  facility = paste0("Facility ", 1:10),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

facility_test$revenue <- (
  as.integer(sub("Facility ", "", facility_test$facility)) *
    100 *
    ifelse(facility_test$report_year == 2024L, 0.5, 1)
)

facility_chart_rows <- executive_venues_chart_rows(
  facility_test,
  "facility",
  "revenue",
  "facility hire regression"
)

# Seven individual facilities plus one combined category.
stopifnot(
  length(unique(facility_chart_rows$category)) == 8L,
  "Other Facilities" %in% facility_chart_rows$category
)

# Every year's chart values must reconcile to the input.
for (year in c(2024L, 2025L)) {

  source_total <- sum(
    facility_test$revenue[
      facility_test$report_year == year
    ]
  )

  chart_total <- sum(
    facility_chart_rows$value[
      facility_chart_rows$report_year == year
    ]
  )

  stopifnot(
    isTRUE(all.equal(source_total, chart_total))
  )
}

cat("PASS Venues facility-hire chart coverage\n")


# -------------------------------------------------------------------------
# Executive Venues: KPI calculations and missing-data regression
# -------------------------------------------------------------------------

# Reuse the existing validated Venues Operations test fixture.
venues_kpi_fixture <- make_venues_fixture()

# Add merchandise data with Section Totals and Item details.
# Item values must not be added again to their Section Totals.
venues_kpi_fixture$merchandise <- data.frame(
  report_year = rep(2025L, 6L),
  date = as.Date(rep(NA_character_, 6L)),
  period_type = rep("YTD", 6L),
  section = c(
    "Retail", "Retail", "Equipment",
    "Retail", "Retail", "Equipment"
  ),
  item = c(
    NA, "Cap", NA,
    NA, "Cap", NA
  ),
  row_type = c(
    "Section Total", "Item", "Section Total",
    "Section Total", "Item", "Section Total"
  ),
  metric = c(
    "Revenue", "Revenue", "Revenue",
    "Transactions", "Transactions", "Transactions"
  ),
  value = c(300, 180, 125, 30, 10, 5),
  stringsAsFactors = FALSE
)

venues_kpi_data <- shiny::reactiveVal(
  list(venues_reports = venues_kpi_fixture)
)

shiny::testServer(
  mod_executive_dashboard_server,
  args = list(data = venues_kpi_data),
  {

    session$setInputs(dashboard_section = "metrics")
    session$flushReact()

    session$setInputs(
      view_mode = "Year Review",
      year = "2025"
    )
    session$flushReact()

    stopifnot(
      identical(active_focus(), "Metrics"),
      identical(executive_selected_years(), 2025L)
    )

    # ---------------------------------------------------------------
    # Verify all four headline KPIs against known source values.
    # ---------------------------------------------------------------

    values <- executive_venues_kpis_for_year(2025L)

    stopifnot(
      # Reported attendance YTD takes priority over monthly sums.
      isTRUE(all.equal(values$total_attendance, 3800)),

      # Courts 5000 + Rooms 1500, excluding Courts detail rows.
      isTRUE(all.equal(values$hire_revenue, 6500)),

      # Reported casual wage total, not monthly amounts added again.
      isTRUE(all.equal(values$actual_wages, 2100)),

      # Retail 300 + Equipment 125, excluding Retail item detail.
      isTRUE(all.equal(values$merchandise_sales, 425)),

      # Supporting measures.
      isTRUE(all.equal(values$hire_transactions, 130)),
      isTRUE(all.equal(values$paid_hours, 205)),
      isTRUE(all.equal(values$wage_variance, 50))
    )

    # ---------------------------------------------------------------
    # Verify that the KPI cards display the expected values.
    # ---------------------------------------------------------------

    specs <- executive_kpi_specs()

    stopifnot(
      identical(
        vapply(specs, `[[`, character(1), "title"),
        c(
          "Total Attendance",
          "Facility Hire Revenue",
          "Casual Wages",
          "Merchandise Revenue"
        )
      ),

      identical(specs[[1L]]$value, "3,800"),

      identical(
        specs[[2L]]$value,
        format_compact_currency(6500)
      ),

      identical(
        specs[[3L]]$value,
        format_compact_currency(2100)
      ),

      identical(
        specs[[4L]]$value,
        format_compact_currency(425)
      )
    )

    # ---------------------------------------------------------------
    # Verify merchandise chart totals reconcile to the KPI.
    # ---------------------------------------------------------------

    merchandise_rows <- executive_venues_merchandise_rows()

    stopifnot(
      nrow(merchandise_rows) == 2L,
      isTRUE(all.equal(
        sum(merchandise_rows$revenue),
        values$merchandise_sales
      ))
    )

    # Wage budget is absent from this fixture.
    # Missing data must stay NA, not become zero.
    wage_rows <- executive_venues_wages_rows()

    stopifnot(
      nrow(wage_rows) == 1L,
      is.na(wage_rows$budget[[1L]]),
      isTRUE(all.equal(wage_rows$actual[[1L]], 2100))
    )
  }
)

cat("PASS Venues KPI values and merchandise reconciliation\n")


# -------------------------------------------------------------------------
# Missing merchandise data must not be displayed as zero.
# -------------------------------------------------------------------------

venues_missing_fixture <- make_venues_fixture()

shiny::testServer(
  mod_executive_dashboard_server,
  args = list(
    data = shiny::reactiveVal(
      list(venues_reports = venues_missing_fixture)
    )
  ),
  {

    session$setInputs(
      dashboard_section = "metrics"
    )
    session$flushReact()

    session$setInputs(
      view_mode = "Year Review",
      year = "2025"
    )
    session$flushReact()

    values <- executive_venues_kpis_for_year(2025L)
    specs <- executive_kpi_specs()

    stopifnot(
      is.na(values$merchandise_sales),
      identical(specs[[4L]]$value, "—"),
      isTRUE(all.equal(values$hire_revenue, 6500)),
      isTRUE(all.equal(values$actual_wages, 2100))
    )
  }
)

cat("PASS Venues missing-data safeguards\n")


# -------------------------------------------------------------------------
# Executive Venues: Comparison mode regression
# -------------------------------------------------------------------------

# Reuse the verified 2025 fixture from Change 14.
# Create 2024 values at half the 2025 values so that incorrect
# year selection and comparison calculations are detectable.

venues_comparison_2024 <- lapply(
  venues_kpi_fixture,
  function(rows) {

    if (!is.data.frame(rows) ||
        !"report_year" %in% names(rows)) {
      return(rows)
    }

    rows$report_year[] <- 2024L

    if ("date" %in% names(rows)) {
      valid_dates <- !is.na(rows$date)

      rows$date[valid_dates] <- as.Date(
        sub(
          "^2025-",
          "2024-",
          as.character(rows$date[valid_dates])
        )
      )
    }

    if ("value" %in% names(rows)) {
      rows$value <- as.numeric(rows$value) / 2
    }

    rows
  }
)

# Combine both years without modifying the original fixture.
venues_comparison_fixture <- Map(
  function(previous, current) {
    if (is.data.frame(previous) && is.data.frame(current)) {
      rbind(previous, current)
    } else {
      current
    }
  },
  venues_comparison_2024,
  venues_kpi_fixture
)

shiny::testServer(
  mod_executive_dashboard_server,
  args = list(
    data = shiny::reactiveVal(
      list(venues_reports = venues_comparison_fixture)
    )
  ),
  {

    session$setInputs(dashboard_section = "metrics")
    session$flushReact()

    session$setInputs(
      view_mode = "Comparison",
      year = "2025",
      comparison_year = "2024"
    )
    session$flushReact()

    # -------------------------------------------------------------
    # Comparison context
    # -------------------------------------------------------------

    stopifnot(
      identical(active_focus(), "Metrics"),
      identical(executive_context()$view_mode, "Comparison"),
      identical(executive_selected_years(), c(2024L, 2025L))
    )

    # -------------------------------------------------------------
    # Known KPI values for both years
    # -------------------------------------------------------------

    current <- executive_venues_kpis_for_year(2025L)
    previous <- executive_venues_kpis_for_year(2024L)

    stopifnot(
      isTRUE(all.equal(current$total_attendance, 3800)),
      isTRUE(all.equal(previous$total_attendance, 1900)),

      isTRUE(all.equal(current$hire_revenue, 6500)),
      isTRUE(all.equal(previous$hire_revenue, 3250)),

      isTRUE(all.equal(current$actual_wages, 2100)),
      isTRUE(all.equal(previous$actual_wages, 1050)),

      isTRUE(all.equal(current$merchandise_sales, 425)),
      isTRUE(all.equal(previous$merchandise_sales, 212.5))
    )

    # -------------------------------------------------------------
    # KPI comparison labels
    # -------------------------------------------------------------

    specs <- executive_kpi_specs()

    stopifnot(
      length(specs) == 4L,

      identical(
        specs[[1L]]$comparison,
        executive_count_change_label(3800, 1900, "2024")
      ),

      identical(
        specs[[2L]]$comparison,
        executive_currency_change_label(6500, 3250, "2024")
      ),

      identical(
        specs[[3L]]$comparison,
        executive_currency_change_label(2100, 1050, "2024")
      ),

      identical(
        specs[[4L]]$comparison,
        executive_currency_change_label(425, 212.5, "2024")
      )
    )

    # -------------------------------------------------------------
    # All four charts contain both reporting years
    # -------------------------------------------------------------

    attendance <- executive_venues_attendance_rows()
    hire <- executive_venues_hire_rows()
    wages <- executive_venues_wages_rows()
    merchandise <- executive_venues_merchandise_rows()

    both_years <- function(rows) {
      identical(
        sort(unique(as.integer(rows$report_year))),
        c(2024L, 2025L)
      )
    }

    stopifnot(
      both_years(attendance),
      both_years(hire),
      both_years(wages),
      both_years(merchandise)
    )

    # -------------------------------------------------------------
    # Facility Hire and Merchandise chart reconciliation
    # -------------------------------------------------------------

    for (year in c(2024L, 2025L)) {

      annual <- executive_venues_kpis_for_year(year)

      hire_year <- hire[
        hire$report_year == year, , drop = FALSE
      ]

      merchandise_year <- merchandise[
        merchandise$report_year == year, , drop = FALSE
      ]

      wages_year <- wages[
        wages$report_year == year, , drop = FALSE
      ]

      stopifnot(
        isTRUE(all.equal(
          sum(hire_year$revenue),
          annual$hire_revenue
        )),

        isTRUE(all.equal(
          sum(merchandise_year$revenue),
          annual$merchandise_sales
        )),

        nrow(wages_year) == 1L,

        isTRUE(all.equal(
          wages_year$actual[[1L]],
          annual$actual_wages
        ))
      )
    }

    # -------------------------------------------------------------
    # Exact-value hover shows both years
    # -------------------------------------------------------------

    hire_chart <- executive_venues_chart_rows(
      hire,
      "facility",
      "revenue",
      "Facility Hire Comparison"
    )

    hover_text <- executive_venues_category_hover(
      list(x = 1, y = length(unique(hire_chart$category))),
      hire,
      "facility",
      "revenue",
      TRUE
    )

    stopifnot(
      grepl("2024:", hover_text, fixed = TRUE),
      grepl("2025:", hover_text, fixed = TRUE)
    )

    # -------------------------------------------------------------
    # Key Insights use the primary selected year
    # -------------------------------------------------------------

    insights_html <- output$venues_key_insights$html

    stopifnot(
      grepl("Badminton", insights_html, fixed = TRUE),
      grepl("Courts", insights_html, fixed = TRUE),
      grepl("Retail", insights_html, fixed = TRUE)
    )
  }
)

cat("PASS Venues Comparison, KPIs, charts and insights\n")

