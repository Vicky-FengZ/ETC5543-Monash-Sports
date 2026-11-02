suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
})

# -------------------------------------------------------------------------
# Executive Dashboard registry tests
# -------------------------------------------------------------------------

# Registry structure ------------------------------------------------------

registry <- executive_registry_table()

stopifnot(
  is.data.frame(registry),
  nrow(registry) == 17L,
  length(unique(registry$id)) == nrow(registry),
  all(
    c(
      "id",
      "title",
      "category",
      "focus",
      "component_type",
      "source_group",
      "source_label",
      "required_data",
      "default_selected",
      "preferred_size",
      "supports_year_review",
      "supports_comparison"
    ) %in% names(registry)
  )
)

# Executive focus grouping ------------------------------------------------

overview_ids <- executive_visual_ids(
  focus = "Overview"
)

site_ids <- executive_visual_ids(
  focus = "Site"
)

metrics_ids <- executive_visual_ids(
  focus = "Metrics"
)


stopifnot(

  identical(
    overview_ids,
    c(
      "monthly_income_expenses",
      "annual_performance_trend",
      "budget_vs_actual",
      "revenue_capture",
      "key_insights"
    )
  ),

  identical(
    site_ids,
    c(
      "site_performance",
      "site_ranking",
      "site_budget_performance",
      "site_revenue_opportunity",
      "site_discount_mix",
      "site_values",
      "facility_ranking",
      "facility_performance",
      "facility_discount_mix",
      "facility_discount_trend",
      "facility_revenue_opportunity",
      "facility_capture_rate"
    )
  ),

  length(
    metrics_ids
  ) == 0L,

  length(
    overview_ids
  ) +
    length(
      site_ids
    ) ==
    17L
)


# Source grouping ---------------------------------------------------------

monthly_ids <- executive_visual_ids(
  source_group = "monthly"
)

detailed_ids <- executive_visual_ids(
  source_group = "detailed"
)

stopifnot(
  length(monthly_ids) == 10L,
  length(detailed_ids) == 6L,
  length(
    intersect(
      monthly_ids,
      detailed_ids
    )
  ) == 0L
)

supporting_ids <- executive_visual_ids(
  source_group = "supporting"
)

stopifnot(
  identical(
    supporting_ids,
    "key_insights"
  )
)

stopifnot(
  length(
    executive_visual_ids(
      component_type = "graph"
    )
  ) == 15L,

  identical(
    executive_visual_ids(
      component_type = "table"
    ),
    "site_values"
  ),

  identical(
    executive_visual_ids(
      component_type = "insight"
    ),
    "key_insights"
  )
)

stopifnot(
  identical(
    executive_visual_source_label("monthly"),
    "Monthly Site Reports"
  ),
  identical(
    executive_visual_source_label("detailed"),
    "Detailed Site Reports"
  ),
  identical(
    executive_visual_source_label("supporting"),
    "Supporting Components"
  )
)


# Default Executive Finance set ------------------------------------------

expected_defaults <- c(
  "monthly_income_expenses",
  "annual_performance_trend",
  "budget_vs_actual",
  "site_performance",
  "site_ranking",
  "site_budget_performance",
  "site_revenue_opportunity",
  "site_discount_mix",
  "revenue_capture",
  "site_values",
  "key_insights"
)

expected_overview_defaults <- c(
  "monthly_income_expenses",
  "annual_performance_trend",
  "budget_vs_actual",
  "revenue_capture",
  "key_insights"
)

expected_overview_kpi_titles <- c(
  "Total Income",
  "Total Expenses",
  "Net Result",
  "Budget Variance"
)

expected_site_defaults <- c(
  "site_performance",
  "site_ranking",
  "site_budget_performance",
  "site_revenue_opportunity",
  "site_discount_mix",
  "site_values"
)

stopifnot(
  identical(
    executive_default_visual_ids(),
    expected_defaults
  )
)

stopifnot(

  identical(
    executive_default_visual_ids(
      category = "Finance",
      focus = "Overview"
    ),
    expected_overview_defaults
  ),

  identical(
    executive_default_visual_ids(
      category = "Finance",
      focus = "Site"
    ),
    expected_site_defaults
  )
)


# -------------------------------------------------------------------------
# No uploaded data
# -------------------------------------------------------------------------

empty_summary <-
  executive_source_availability_summary(
    NULL
  )

stopifnot(
  nrow(empty_summary) == 3L
)

empty_supporting <- empty_summary[
  empty_summary$source_group == "supporting",
  ,
  drop = FALSE
]

stopifnot(
  nrow(empty_supporting) == 1L,
  empty_supporting$available_visuals[[1L]] == 1L,
  empty_supporting$total_visuals[[1L]] == 1L
)

empty_monthly <- empty_summary[
  empty_summary$source_group == "monthly",
  ,
  drop = FALSE
]

empty_detailed <- empty_summary[
  empty_summary$source_group == "detailed",
  ,
  drop = FALSE
]

stopifnot(
  nrow(empty_monthly) == 1L,
  empty_monthly$available_visuals[[1L]] == 0L,
  empty_monthly$total_visuals[[1L]] == 10L,

  nrow(empty_detailed) == 1L,
  empty_detailed$available_visuals[[1L]] == 0L,
  empty_detailed$total_visuals[[1L]] == 6L
)


# -------------------------------------------------------------------------
# Monthly Site Reports only
# -------------------------------------------------------------------------

monthly_only_data <- list(

  monthly_reports = list(

    finance = data.frame(
      value = 1,
      stringsAsFactors = FALSE
    ),

    discounts = data.frame(
      value = 1,
      stringsAsFactors = FALSE
    )
  ),

  site_reports = NULL,

  venues_reports = NULL
)

monthly_summary <-
  executive_source_availability_summary(
    monthly_only_data
  )

monthly_row <- monthly_summary[
  monthly_summary$source_group == "monthly",
  ,
  drop = FALSE
]

detailed_row <- monthly_summary[
  monthly_summary$source_group == "detailed",
  ,
  drop = FALSE
]

stopifnot(
  monthly_row$available_visuals[[1L]] == 10L,
  monthly_row$total_visuals[[1L]] == 10L,

  detailed_row$available_visuals[[1L]] == 0L,
  detailed_row$total_visuals[[1L]] == 6L
)


monthly_availability <-
  executive_registry_availability_table(
    monthly_only_data
  )

stopifnot(
  all(
    monthly_availability$available[
      monthly_availability$source_group ==
        "monthly"
    ]
  ),
  !any(
    monthly_availability$available[
      monthly_availability$source_group ==
        "detailed"
    ]
  )
)


# -------------------------------------------------------------------------
# Required-dataset availability
# -------------------------------------------------------------------------

finance_only_data <- list(

  monthly_reports = list(

    finance = data.frame(
      value = 1,
      stringsAsFactors = FALSE
    ),

    discounts = data.frame()
  )
)

finance_only_availability <-
  executive_registry_availability_table(
    finance_only_data
  )

income_expenses_row <-
  finance_only_availability[
    finance_only_availability$id ==
      "monthly_income_expenses",
    ,
    drop = FALSE
  ]

discount_mix_row <-
  finance_only_availability[
    finance_only_availability$id ==
      "site_discount_mix",
    ,
    drop = FALSE
  ]

revenue_opportunity_row <-
  finance_only_availability[
    finance_only_availability$id ==
      "site_revenue_opportunity",
    ,
    drop = FALSE
  ]

stopifnot(
  income_expenses_row$available[[1L]],

  !discount_mix_row$available[[1L]],
  grepl(
    "discounts",
    discount_mix_row$availability_reason[[1L]],
    fixed = TRUE
  ),

  !revenue_opportunity_row$available[[1L]],
  grepl(
    "discounts",
    revenue_opportunity_row$availability_reason[[1L]],
    fixed = TRUE
  )
)


# -------------------------------------------------------------------------
# Monthly + Detailed Site Reports
# -------------------------------------------------------------------------

complete_test_data <- monthly_only_data

complete_test_data$site_reports <- list(

  finance = data.frame(
    value = 1,
    stringsAsFactors = FALSE
  ),

  discounts = data.frame(
    value = 1,
    stringsAsFactors = FALSE
  )
)

complete_summary <-
  executive_source_availability_summary(
    complete_test_data
  )

complete_monthly <- complete_summary[
  complete_summary$source_group == "monthly",
  ,
  drop = FALSE
]

complete_detailed <- complete_summary[
  complete_summary$source_group == "detailed",
  ,
  drop = FALSE
]

stopifnot(
  complete_monthly$available_visuals[[1L]] == 10L,
  complete_monthly$total_visuals[[1L]] == 10L,

  complete_detailed$available_visuals[[1L]] == 6L,
  complete_detailed$total_visuals[[1L]] == 6L
)


# Direct report-family objects are also supported -------------------------

direct_monthly <- structure(
  list(
    finance = data.frame(
      value = 1,
      stringsAsFactors = FALSE
    ),
    discounts = data.frame(
      value = 1,
      stringsAsFactors = FALSE
    )
  ),
  class = "monthly_site_report_data"
)

stopifnot(
  executive_visual_availability(
    direct_monthly,
    "site_ranking"
  )$available
)

# -------------------------------------------------------------------------
# Context-aware Executive visual availability
# -------------------------------------------------------------------------

context_detailed_data <- list(

  site_reports = list(

    finance = data.frame(
      report_year = c(
        2025L,
        2027L,
        2027L
      ),
      report_unit = c(
        "Aquatics",
        "Aquatics",
        "Esports"
      ),
      campus = c(
        "Clayton",
        "Clayton",
        "Clayton"
      ),
      business_unit = c(
        "Pool Hire",
        "Pool Hire",
        "Esports Arena"
      ),
      stringsAsFactors = FALSE
    )
  )
)


aquatics_2027_context <- list(
  view_mode = "Year Review",
  year = "2027",
  comparison_year = NULL,
  site = "Aquatics Clayton"
)


stopifnot(
  executive_visual_availability(
    dashboard_data =
      context_detailed_data,
    visual_id =
      "facility_ranking",
    context =
      aquatics_2027_context
  )$available
)


aquatics_2026_context <- list(
  view_mode = "Year Review",
  year = "2026",
  comparison_year = NULL,
  site = "Aquatics Clayton"
)


aquatics_2026_availability <-
  executive_visual_availability(
    dashboard_data =
      context_detailed_data,
    visual_id =
      "facility_ranking",
    context =
      aquatics_2026_context
  )


stopifnot(
  !aquatics_2026_availability$available,
  grepl(
    "selected context",
    aquatics_2026_availability$reason,
    fixed = TRUE
  )
)


esports_2027_context <- list(
  view_mode = "Year Review",
  year = "2027",
  comparison_year = NULL,
  site = "Esports"
)


stopifnot(
  executive_visual_availability(
    dashboard_data =
      context_detailed_data,
    visual_id =
      "facility_ranking",
    context =
      esports_2027_context
  )$available
)


esports_2025_context <- list(
  view_mode = "Year Review",
  year = "2025",
  comparison_year = NULL,
  site = "Esports"
)


stopifnot(
  !executive_visual_availability(
    dashboard_data =
      context_detailed_data,
    visual_id =
      "facility_ranking",
    context =
      esports_2025_context
  )$available
)


aquatics_comparison_context <- list(
  view_mode = "Comparison",
  year = "2027",
  comparison_year = "2025",
  site = "Aquatics Clayton"
)


stopifnot(
  executive_visual_availability(
    dashboard_data =
      context_detailed_data,
    visual_id =
      "facility_ranking",
    context =
      aquatics_comparison_context
  )$available
)


missing_comparison_context <- list(
  view_mode = "Comparison",
  year = "2027",
  comparison_year = "2026",
  site = "Aquatics Clayton"
)


stopifnot(
  !executive_visual_availability(
    dashboard_data =
      context_detailed_data,
    visual_id =
      "facility_ranking",
    context =
      missing_comparison_context
  )$available
)

cat(
  "PASS Executive Dashboard visual registry\n"
)

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
# Executive Dashboard visual picker
# -------------------------------------------------------------------------

picker_test_data <- shiny::reactiveVal(
  list(

    monthly_reports = list(

      finance = data.frame(
        report_year = c(
          2025L,
          2026L
        ),
        date = as.Date(c(
          "2025-01-01",
          "2026-01-01"
        )),
        site = c(
          "Aquatics Clayton",
          "Aquatics Clayton"
        ),
        is_summary = c(
          TRUE,
          TRUE
        ),
        metric = c(
          "Income",
          "Income"
        ),
        value_type = c(
          "Actual",
          "Actual"
        ),
        value = c(
          1,
          1
        ),
        stringsAsFactors = FALSE
      ),

      discounts = data.frame(
        report_year = c(
          2025L,
          2026L
        ),
        value = c(
          1,
          1
        ),
        stringsAsFactors = FALSE
      )
    ),

    site_reports = NULL,

    venues_reports = NULL
  )
)


shiny::testServer(
  mod_executive_dashboard_server,
  args = list(
    data = picker_test_data
  ),
  {

    # ---------------------------------------------------------------------
    # Finance is the default Executive Dashboard category
    # ---------------------------------------------------------------------

    session$setInputs(
      dashboard_section = "finance"
    )

    session$flushReact()

    stopifnot(
      identical(
        active_category(),
        "Finance"
      )
    )

    stopifnot(
      identical(
        active_focus(),
        "Overview"
      )
    )

    # ---------------------------------------------------------------------
    # Year Review context
    # ---------------------------------------------------------------------

    session$setInputs(
      view_mode = "Year Review",
      year = "2026",
      comparison_year = "2025",
      site = "All Monash Sport"
    )

    session$flushReact()

    year_review_context <- executive_context()

    stopifnot(
      identical(
        year_review_context$view_mode,
        "Year Review"
      ),
      identical(
        year_review_context$year,
        "2026"
      ),
      is.null(
        year_review_context$comparison_year
      ),
      identical(
        year_review_context$site,
        "All Monash Sport"
      ),
      !"metric" %in%
        names(year_review_context)
    )

    # Overview must remain organisation-wide even if a site input is supplied.
      session$setInputs(
        site = "Aquatics Clayton"
      )

      session$flushReact()

      overview_scope_context <-
        executive_context()

      stopifnot(
        identical(
          overview_scope_context$site,
          "All Monash Sport"
        )
      )

      # Restore the browser input for the remaining tests in this block.
      session$setInputs(
        site = "All Monash Sport"
      )

      session$flushReact()
    
    # ---------------------------------------------------------------------
    # Overview content contract
    # ---------------------------------------------------------------------

    overview_kpi_specs <-
      executive_kpi_specs()

    stopifnot(

      length(
        overview_kpi_specs
      ) == 4L,

      identical(
        vapply(
          overview_kpi_specs,
          function(spec) {
            spec$title
          },
          character(1)
        ),
        expected_overview_kpi_titles
      ),

      identical(
        vapply(
          overview_kpi_specs,
          function(spec) {
            spec$output_id
          },
          character(1)
        ),
        c(
          "kpi_total_income",
          "kpi_total_expenses",
          "kpi_net_result",
          "kpi_budget_variance"
        )
      ),

      identical(
        executive_context()$site,
        "All Monash Sport"
      )
    )


    # ---------------------------------------------------------------------
    # Comparison context
    # ---------------------------------------------------------------------

    session$setInputs(
      view_mode = "Comparison",
      comparison_year = "2025"
    )

    session$flushReact()

    comparison_context <- executive_context()

    stopifnot(
      identical(
        comparison_context$view_mode,
        "Comparison"
      ),
      identical(
        comparison_context$year,
        "2026"
      ),
      identical(
        comparison_context$comparison_year,
        "2025"
      ),
      identical(
        comparison_context$site,
        "All Monash Sport"
      ),
      !"metric" %in%
        names(comparison_context)
    )

    # ---------------------------------------------------------------------
    # Overview Year Review vs Comparison year selection
    # ---------------------------------------------------------------------

    # Year Review includes only the selected year
    # for monthly finance calculations.

    stopifnot(
      identical(
        executive_context_years(
          year_review_context
        ),
        2026L
      )
    )

    # Historical trend visuals retain available years
    # up to the selected reporting year.

    stopifnot(
      identical(
        executive_context_trend_years(
          context = year_review_context,
          available_years = c(
            2024L,
            2025L,
            2026L,
            2027L
          )
        ),
        c(
          2024L,
          2025L,
          2026L
        )
      )
    )

    # Comparison mode uses exactly the two
    # selected reporting years.

    stopifnot(
      identical(
        executive_context_years(
          comparison_context
        ),
        c(
          2025L,
          2026L
        )
      )
    )

    stopifnot(
      identical(
        executive_context_trend_years(
          context = comparison_context,
          available_years = c(
            2024L,
            2025L,
            2026L,
            2027L
          )
        ),
        c(
          2025L,
          2026L
        )
      )
    )

    # ---------------------------------------------------------------------
    # Overview KPI values and comparison labels
    # ---------------------------------------------------------------------

    current_finance <- executive_kpi_finance()

    previous_finance <-
      executive_kpi_comparison_finance()

    comparison_kpis <- executive_kpi_specs()

    kpi_fields <- c(
      "income_actual",
      "expenses_actual",
      "net_actual",
      "budget_variance"
    )

    stopifnot(
      length(comparison_kpis) == 4L,
      identical(
        executive_context()$site,
        "All Monash Sport"
      )
    )

    # Current and comparison KPI values must come
    # from their respective selected reporting years.

    if (!is.null(current_finance)) {
      stopifnot(
        nrow(current_finance) == 1L,
        current_finance$report_year[[1L]] == 2026L
      )
    }

    if (!is.null(previous_finance)) {
      stopifnot(
        nrow(previous_finance) == 1L,
        previous_finance$report_year[[1L]] == 2025L
      )
    }

    # Every KPI must show a comparison only when
    # both years have a finite corresponding value.

    for (i in seq_along(kpi_fields)) {

      field <- kpi_fields[[i]]

      has_comparable_values <-
        !is.null(current_finance) &&
        !is.null(previous_finance) &&
        is.finite(current_finance[[field]][[1L]]) &&
        is.finite(previous_finance[[field]][[1L]])

      comparison_label <-
        comparison_kpis[[i]]$comparison

      if (has_comparable_values) {

        expected_difference <-
          current_finance[[field]][[1L]] -
          previous_finance[[field]][[1L]]

        expected_label <- paste0(
          if (expected_difference > 0) "+" else "",
          format_compact_currency(expected_difference),
          " vs 2025"
        )

        stopifnot(
          identical(
            comparison_label,
            expected_label
          )
        )

      } else {

        stopifnot(
          is.null(comparison_label)
        )
      }
    }

    # ---------------------------------------------------------------------
    # Overview visual data and context consistency
    # ---------------------------------------------------------------------

    overview_data_checks <- function(
      view_mode,
      expected_years
    ) {

      context <- executive_context()

      stopifnot(
        identical(context$view_mode, view_mode),
        identical(context$site, "All Monash Sport")
      )

      monthly_finance <- executive_monthly_finance()
      annual_finance <- executive_annual_finance()
      annual_revenue <- executive_annual_revenue()

      check_years <- function(x, allowed_years) {
        stopifnot(
          is.data.frame(x),
          "report_year" %in% names(x)
        )

        if (nrow(x) > 0L) {
          stopifnot(
            !anyNA(x$report_year),
            all(
              x$report_year %in% allowed_years
            )
          )
        }
      }

      # Income vs Expenses and Budget vs Actual
      # use the selected year(s).
      check_years(
        monthly_finance,
        expected_years
      )

      # Annual Performance Trend and Revenue Capture
      # use historical trend years in Year Review,
      # or exactly two years in Comparison.
      trend_years <- executive_context_trend_years(
        context = context,
        available_years = c(
          2024L, 2025L, 2026L, 2027L
        )
      )

      check_years(
        annual_finance,
        trend_years
      )

      check_years(
        annual_revenue,
        trend_years
      )

      invisible(TRUE)
    }


    # Comparison mode
    overview_data_checks(
      view_mode = "Comparison",
      expected_years = c(2025L, 2026L)
    )


    # Year Review mode
    session$setInputs(
      view_mode = "Year Review",
      year = "2026"
    )

    session$flushReact()

    overview_data_checks(
      view_mode = "Year Review",
      expected_years = 2026L
    )


    # Restore Comparison mode so subsequent
    # existing tests retain their expected context.
    session$setInputs(
      view_mode = "Comparison",
      year = "2026",
      comparison_year = "2025"
    )

    session$flushReact()

    stopifnot(
      identical(
        executive_context()$view_mode,
        "Comparison"
      )
    )

    # ---------------------------------------------------------------------
    # Final Executive Overview validation
    # ---------------------------------------------------------------------

    # Verify that KPI values remain consistent with
    # the currently selected reporting year.

    session$setInputs(
      view_mode = "Year Review",
      year = "2026"
    )

    session$flushReact()

    year_review_kpis <- executive_kpi_specs()
    year_review_finance <- executive_kpi_finance()

    kpi_fields <- c(
      "income_actual",
      "expenses_actual",
      "net_actual",
      "budget_variance"
    )

    stopifnot(
      length(year_review_kpis) == 4L,

      identical(
        executive_context()$view_mode,
        "Year Review"
      ),

      is.null(
        executive_kpi_comparison_finance()
      ),

      all(
        vapply(
          year_review_kpis,
          function(spec) {
            is.null(spec$comparison)
          },
          logical(1)
        )
      )
    )

    # Compare each displayed KPI with the
    # underlying annual finance value.

    for (i in seq_along(kpi_fields)) {

      expected_value <- "—"

      if (
        !is.null(year_review_finance) &&
        is.data.frame(year_review_finance) &&
        nrow(year_review_finance) > 0L
      ) {

        field <- kpi_fields[[i]]

        value <- year_review_finance[[field]][[1L]]

        if (is.finite(value)) {
          expected_value <-
            format_compact_currency(value)
        }
      }

      stopifnot(
        identical(
          year_review_kpis[[i]]$value,
          expected_value
        )
      )
    }


    # Switch back to Comparison and verify
    # the selected reporting context.

    session$setInputs(
      view_mode = "Comparison",
      year = "2026",
      comparison_year = "2025"
    )

    session$flushReact()

    comparison_kpis <- executive_kpi_specs()

    stopifnot(
      identical(
        executive_context()$view_mode,
        "Comparison"
      ),

      identical(
        executive_context()$year,
        "2026"
      ),

      identical(
        executive_context()$comparison_year,
        "2025"
      ),

      identical(
        executive_context()$site,
        "All Monash Sport"
      ),

      length(comparison_kpis) == 4L,

      identical(
        vapply(
          comparison_kpis,
          function(spec) {
            spec$title
          },
          character(1)
        ),
        expected_overview_kpi_titles
      )
    )

    # ---------------------------------------------------------------------
    # Visual metrics are independent of the dashboard context
    # ---------------------------------------------------------------------

    session$setInputs(
      metric_annual_performance_trend = c(
        "Income",
        "Expenses",
        "Net Result"
      ),
      metric_budget_vs_actual = "Expenses",
      metric_site_ranking = "Income",
      metric_facility_performance = "Expenses"
    )
    session$flushReact()

    stopifnot(
      identical(
        input$metric_annual_performance_trend,
        c(
          "Income",
          "Expenses",
          "Net Result"
        )
      ),

      identical(
        input$metric_budget_vs_actual,
        "Expenses"
      ),

      identical(
        input$metric_site_ranking,
        "Income"
      ),

      identical(
        input$metric_facility_performance,
        "Expenses"
      ),

      !"metric" %in%
        names(
          executive_context()
        )
    )

    # ---------------------------------------------------------------------
    # Default seven graphs + table + insight are retained
    # ---------------------------------------------------------------------

    stopifnot(
      identical(
        selected_visuals$Overview,
        expected_overview_defaults
      )
    )


    # ---------------------------------------------------------------------
    # Monthly data makes all 10 Monthly Site visuals available
    # ---------------------------------------------------------------------

    overview_registry <- picker_registry()

      stopifnot(
        identical(
          overview_registry$id,
          c(
            "monthly_income_expenses",
            "annual_performance_trend",
            "budget_vs_actual",
            "revenue_capture",
            "key_insights"
          )
        )
      )
    
      # ---------------------------------------------------------------------
      # Overview visual configuration contract
      # ---------------------------------------------------------------------

      expected_visual_ids <- c(
        "monthly_income_expenses",
        "annual_performance_trend",
        "budget_vs_actual",
        "revenue_capture",
        "key_insights"
      )

      stopifnot(
        all(
          overview_registry$focus == "Overview"
        ),

        all(
          vapply(
            EXECUTIVE_VISUAL_REGISTRY[expected_visual_ids],
            function(spec) {
              isTRUE(spec$default_selected)
            },
            logical(1)
          )
        ),

        all(
          vapply(
            EXECUTIVE_VISUAL_REGISTRY[expected_visual_ids],
            function(spec) {
              isTRUE(spec$supports_year_review)
            },
            logical(1)
          )
        ),

        all(
          vapply(
            EXECUTIVE_VISUAL_REGISTRY[expected_visual_ids],
            function(spec) {
              isTRUE(spec$supports_comparison)
            },
            logical(1)
          )
        ),

        all(
          overview_registry$source_group[
            overview_registry$id != "key_insights"
          ] == "monthly"
        ),

        identical(
          EXECUTIVE_VISUAL_REGISTRY[["key_insights"]]$component_type,
          "insight"
        ),

        identical(
          EXECUTIVE_VISUAL_REGISTRY[["revenue_capture"]]$required_data,
          c("finance", "discounts")
        )
      )

      monthly_registry <- overview_registry[
        overview_registry$source_group ==
          "monthly",
        ,
        drop = FALSE
      ]

      detailed_registry <- overview_registry[
        overview_registry$source_group ==
          "detailed",
        ,
        drop = FALSE
      ]

      supporting_registry <- overview_registry[
        overview_registry$source_group ==
          "supporting",
        ,
        drop = FALSE
      ]


      stopifnot(

        nrow(monthly_registry) == 4L,
        all(
          monthly_registry$available
        ),

        nrow(detailed_registry) == 0L,

        nrow(supporting_registry) == 1L,
        supporting_registry$available[[1L]],

        identical(
          supporting_registry$id[[1L]],
          "key_insights"
        ),

        all(
          overview_registry$focus ==
            "Overview"
        )
      ) 

    # ---------------------------------------------------------------------
    # Apply a custom visual selection
    # ---------------------------------------------------------------------

    session$setInputs(
      visuals_monthly = c(
        "monthly_income_expenses",
        "budget_vs_actual"
      ),
      apply_visuals = 1
    )

    session$flushReact()

      stopifnot(
        identical(
          selected_visuals$Overview,
          c(
            "monthly_income_expenses",
            "budget_vs_actual"
          )
        )
      )

      session$setInputs(
        dashboard_section = "site"
      )

      session$flushReact()
    
      stopifnot(
        identical(
          dashboard_contexts$Overview$site,
          "All Monash Sport"
        ),

        identical(
          dashboard_contexts$Overview$year,
          "2026"
        ),

        identical(
          dashboard_contexts$Overview$view_mode,
          "Comparison"
        ),

        identical(
          dashboard_contexts$Overview$comparison_year,
          "2025"
        )
      )

      session$setInputs(
        view_mode = "Year Review",
        year = "2026",
        site = "Aquatics Clayton"
      )

      session$flushReact()

      site_scope_context <-
        executive_context()

      stopifnot(
        identical(
          active_category(),
          "Finance"
        ),

        identical(
          active_focus(),
          "Site"
        ),

        identical(
          site_scope_context$site,
          "Aquatics Clayton"
        )
      )

      stopifnot(
        identical(
          selected_visuals$Site,
          expected_site_defaults
        )
      )
    
    site_registry <- picker_registry()

      stopifnot(
        nrow(site_registry) > 0L,
        all(
          site_registry$focus == "Site"
        )
      )

    # ---------------------------------------------------------------------
    # Table and insight components can be selected
    # ---------------------------------------------------------------------

    session$setInputs(
      visuals_monthly = "site_values",
      visuals_detailed = character(),
      apply_visuals = 2
    )

    session$flushReact()

    stopifnot(
      identical(
        selected_visuals$Site,
        "site_values"
      )
    )


    # ---------------------------------------------------------------------
    # Unavailable visuals must not be applied
    # ---------------------------------------------------------------------
    # Facility Ranking requires Detailed Site Report finance data.
    # It is intentionally supplied here even though Detailed Site Reports
    # are absent, to ensure the module removes unavailable selections.

    session$setInputs(
      visuals_monthly = "site_ranking",
      visuals_detailed = "facility_ranking",
      visuals_supporting = character(),
      apply_visuals = 3
    )

    session$flushReact()

    stopifnot(
      identical(
        selected_visuals$Site,
        "site_ranking"
      )
    )

    # ---------------------------------------------------------------------
    # Overview, Site and Metrics maintain independent selections
    # ---------------------------------------------------------------------

    session$setInputs(
      dashboard_section = "metrics"
    )

    session$flushReact()

    stopifnot(
      identical(
        dashboard_contexts$Site$site,
        "Aquatics Clayton"
      ),

      identical(
        dashboard_contexts$Site$view_mode,
        "Year Review"
      ),

      identical(
        dashboard_contexts$Site$year,
        "2026"
      )
    )

    metrics_scope_context <-
      executive_context()

    stopifnot(

      identical(
        active_category(),
        "Metrics"
      ),

      identical(
        active_focus(),
        "Metrics"
      ),

      identical(
        metrics_scope_context$site,
        "All Venues"
      ),

      length(
        selected_visuals$Metrics
      ) == 0L,

      nrow(
        picker_registry()
      ) == 0L,

      identical(
        selected_visuals$Overview,
        c(
          "monthly_income_expenses",
          "budget_vs_actual"
        )
      ),
          identical(
            selected_visuals$Site,
            "site_ranking"
          )
        )

      # ---------------------------------------------------------------------
      # Focus scopes remain independent after switching
      # ---------------------------------------------------------------------

      session$setInputs(
        dashboard_section = "site"
      )

      session$flushReact()
    
      stopifnot(
        identical(
          dashboard_contexts$Metrics$site,
          "All Venues"
        ),

        identical(
          dashboard_contexts$Metrics$view_mode,
          "Year Review"
        )
      )

      site_round_trip_context <-
        executive_context()

      stopifnot(
        identical(
          active_focus(),
          "Site"
        ),

        identical(
          site_round_trip_context$site,
          "Aquatics Clayton"
        ),

        identical(
          dashboard_contexts$Site$site,
          "Aquatics Clayton"
        )
      )

      # ---------------------------------------------------------------------
      # Return to Overview and confirm selections were preserved
      # ---------------------------------------------------------------------

      session$setInputs(
        dashboard_section = "finance"
      )

      session$flushReact()

      overview_round_trip_context <-
        executive_context()

      stopifnot(

        identical(
          active_category(),
          "Finance"
        ),

        identical(
          active_focus(),
          "Overview"
        ),

        identical(
          overview_round_trip_context$site,
          "All Monash Sport"
        ),

        identical(
          selected_visuals$Overview,
          c(
            "monthly_income_expenses",
            "budget_vs_actual"
          )
        ),

      identical(
        selected_visuals$Site,
        "site_ranking"
      )
    )
  }
)

# -------------------------------------------------------------------------
# Visual picker respects selected Executive context
# -------------------------------------------------------------------------

context_picker_data <- shiny::reactiveVal(
  list(

    monthly_reports = list(

      finance = data.frame(
        report_year = c(
          2026L,
          2027L,
          2027L
        ),
        site = c(
          "Aquatics Clayton",
          "Aquatics Clayton",
          "Esports"
        ),
        value = c(
          1,
          1,
          1
        ),
        stringsAsFactors = FALSE
      )
    ),

    site_reports = list(

      finance = data.frame(
        report_year = c(
          2025L,
          2027L
        ),
        report_unit = c(
          "Aquatics",
          "Aquatics"
        ),
        campus = c(
          "Clayton",
          "Clayton"
        ),
        business_unit = c(
          "Pool Hire",
          "Pool Hire"
        ),
        stringsAsFactors = FALSE
      )
    ),

    venues_reports = NULL
  )
)


shiny::testServer(
  mod_executive_dashboard_server,
  args = list(
    data = context_picker_data
  ),
  {

    # Move to Site focus first.
    session$setInputs(
      dashboard_section = "site"
    )

    session$flushReact()


    # Aquatics has Detailed finance data in 2027.
    session$setInputs(
      view_mode = "Year Review",
      year = "2027",
      site = "Aquatics Clayton"
    )

    session$flushReact()


    registry_2027 <- picker_registry()

    facility_2027 <- registry_2027[
      registry_2027$id ==
        "facility_ranking",
      ,
      drop = FALSE
    ]


    stopifnot(
      nrow(facility_2027) == 1L,
      isTRUE(
        facility_2027$available[[1L]]
      )
    )


    # The same visual must become unavailable when the selected
    # year has no Detailed Site Report data.
    session$setInputs(
      year = "2026"
    )

    session$flushReact()


    registry_2026 <- picker_registry()

    facility_2026 <- registry_2026[
      registry_2026$id ==
        "facility_ranking",
      ,
      drop = FALSE
    ]


    stopifnot(
      nrow(facility_2026) == 1L,
      !facility_2026$available[[1L]],
      grepl(
        "selected context",
        facility_2026$availability_reason[[1L]],
        fixed = TRUE
      )
    )


    # Selecting a site with no Detailed report must also make the
    # facility visual unavailable.
    session$setInputs(
      year = "2027",
      site = "Esports"
    )

    session$flushReact()


    registry_esports <- picker_registry()

    facility_esports <- registry_esports[
      registry_esports$id ==
        "facility_ranking",
      ,
      drop = FALSE
    ]


    stopifnot(
      nrow(facility_esports) == 1L,
      !facility_esports$available[[1L]]
    )
  }
)

# -------------------------------------------------------------------------
# Future years and newly discovered sites
# -------------------------------------------------------------------------

future_executive_data <- shiny::reactiveVal(
  list(

    monthly_reports = list(

      finance = data.frame(
        report_year = c(
          2027L,
          2028L
        ),
        site = c(
          "Esports",
          "Esports"
        ),
        value = c(
          100,
          125
        ),
        stringsAsFactors = FALSE
      ),

      discounts = data.frame(
        report_year = c(
          2027L,
          2028L
        ),
        site = c(
          "Esports",
          "Esports"
        ),
        value = c(
          10,
          12
        ),
        stringsAsFactors = FALSE
      )
    ),

    site_reports = list(

      finance = data.frame(
        report_year = c(
          2027L,
          2028L
        ),
        report_unit = c(
          "Esports",
          "Esports"
        ),
        campus = c(
          "Clayton",
          "Clayton"
        ),
        business_unit = c(
          "Esports Arena",
          "Esports Arena"
        ),
        stringsAsFactors = FALSE
      )
    ),

    venues_reports = NULL
  )
)

future_monthly_fixture <- list(

  finance = data.frame(
    report_year = c(
      2028L,
      2028L
    ),
    site = c(
      "Aquatics Clayton",
      "Esports"
    ),
    value = c(
      100,
      200
    ),
    stringsAsFactors = FALSE
  ),

  discounts = data.frame(
    report_year = c(
      2028L,
      2028L
    ),
    site = c(
      "Aquatics Clayton",
      "Esports"
    ),
    value = c(
      10,
      20
    ),
    stringsAsFactors = FALSE
  )
)


future_esports_reports <-
  executive_filter_monthly_reports_by_site(
    monthly_reports =
      future_monthly_fixture,
    site =
      "Esports"
  )


stopifnot(
  nrow(
    future_esports_reports$finance
  ) == 1L,

  identical(
    future_esports_reports$finance$site[[1L]],
    "Esports"
  ),

  identical(
    future_esports_reports$finance$report_year[[1L]],
    2028L
  ),

  nrow(
    future_esports_reports$discounts
  ) == 1L
)

shiny::testServer(
  mod_executive_dashboard_server,
  args = list(
    data = future_executive_data
  ),
  {

    session$setInputs(
      dashboard_section = "site",
      view_mode = "Year Review",
      year = "2028",
      site = "Esports"
    )

    session$flushReact()


    stopifnot(
      identical(
        available_executive_sites(),
        c(
          "All Monash Sport",
          "Esports"
        )
      )
    )


    future_site_selector_html <-
      output$site_selector$html


    stopifnot(
      grepl(
        "All Monash Sport",
        future_site_selector_html,
        fixed = TRUE
      ),

      grepl(
        "Esports",
        future_site_selector_html,
        fixed = TRUE
      )
    )

    future_context <-
      executive_context()


    stopifnot(
      identical(
        future_context$year,
        "2028"
      ),
      identical(
        future_context$site,
        "Esports"
      )
    )

    stopifnot(
      identical(
        executive_context_years(
          future_context
        ),
        2028L
      )
    )

    # ---------------------------------------------------------------------
    # Removing the selected Site must clear stale Executive scope.
    # ---------------------------------------------------------------------

    future_executive_data(
      list(

        monthly_reports = list(

          finance = data.frame(
            report_year = 2028L,
            site = "Aquatics Clayton",
            value = 100,
            stringsAsFactors = FALSE
          ),

          discounts = data.frame(
            report_year = 2028L,
            site = "Aquatics Clayton",
            value = 10,
            stringsAsFactors = FALSE
          )
        ),

        site_reports = NULL,

        venues_reports = NULL
      )
    )

    session$flushReact()


    stopifnot(
      identical(
        available_executive_sites(),
        c(
          "All Monash Sport",
          "Aquatics Clayton"
        )
      )
    )


    removed_site_selector_html <-
      output$site_selector$html


    stopifnot(
      grepl(
        "All Monash Sport",
        removed_site_selector_html,
        fixed = TRUE
      ),

      !grepl(
        "Esports",
        removed_site_selector_html,
        fixed = TRUE
      )
    )


    # Explicitly simulate stale browser state.
    session$setInputs(
      site = "Esports"
    )

    session$flushReact()


    removed_site_context <-
      executive_context()


    stopifnot(
      identical(
        removed_site_context$site,
        "All Monash Sport"
      )
    )

    # Restore the future-site fixture for the remaining tests.

      future_executive_data(
        list(

          monthly_reports = list(

            finance = data.frame(
              report_year = c(
                2027L,
                2028L
              ),
              site = c(
                "Esports",
                "Esports"
              ),
              value = c(
                100,
                125
              ),
              stringsAsFactors = FALSE
            ),

            discounts = data.frame(
              report_year = c(
                2027L,
                2028L
              ),
              site = c(
                "Esports",
                "Esports"
              ),
              value = c(
                10,
                12
              ),
              stringsAsFactors = FALSE
            )
          ),

          site_reports = list(

            finance = data.frame(
              report_year = c(
                2027L,
                2028L
              ),
              report_unit = c(
                "Esports",
                "Esports"
              ),
              campus = c(
                "Clayton",
                "Clayton"
              ),
              business_unit = c(
                "Esports Arena",
                "Esports Arena"
              ),
              stringsAsFactors = FALSE
            )
          ),

          venues_reports = NULL
        )
      )

      session$flushReact()

      session$setInputs(
        site = "Esports"
      )

      session$flushReact()

    future_registry <- picker_registry()

    future_site_ranking <- future_registry[
      future_registry$id ==
        "site_ranking",
      ,
      drop = FALSE
    ]

    future_facility_ranking <- future_registry[
      future_registry$id ==
        "facility_ranking",
      ,
      drop = FALSE
    ]


    stopifnot(
      nrow(future_site_ranking) == 1L,
      isTRUE(
        future_site_ranking$available[[1L]]
      ),

      nrow(future_facility_ranking) == 1L,
      isTRUE(
        future_facility_ranking$available[[1L]]
      )
    )


    # Future-year comparison must also work without any APP_YEARS
    # assumption inside the Executive context/availability layer.
    session$setInputs(
      view_mode = "Comparison",
      year = "2028",
      comparison_year = "2027",
      site = "Esports"
    )

    session$flushReact()


    future_comparison_context <-
      executive_context()


    stopifnot(
      identical(
        executive_context_years(
          future_comparison_context
        ),
        c(
          2027L,
          2028L
        )
      )
    )


    comparison_registry <-
      picker_registry()


    comparison_facility_ranking <-
      comparison_registry[
        comparison_registry$id ==
          "facility_ranking",
        ,
        drop = FALSE
      ]


    stopifnot(
      nrow(
        comparison_facility_ranking
      ) == 1L,

      isTRUE(
        comparison_facility_ranking$available[[1L]]
      )
    )

    # ---------------------------------------------------------------------
    # Dynamic Site scope remains independent across focus round trips.
    # ---------------------------------------------------------------------

    session$setInputs(
      dashboard_section = "metrics"
    )

    session$flushReact()

    future_metrics_context <-
      executive_context()

    stopifnot(
      identical(
        active_focus(),
        "Metrics"
      ),

      identical(
        future_metrics_context$site,
        "All Venues"
      )
    )


    session$setInputs(
      dashboard_section = "finance"
    )

    session$flushReact()

    future_overview_context <-
      executive_context()

    stopifnot(
      identical(
        active_focus(),
        "Overview"
      ),

      identical(
        future_overview_context$site,
        "All Monash Sport"
      )
    )


    session$setInputs(
      dashboard_section = "site"
    )

    session$flushReact()

    future_site_round_trip_context <-
      executive_context()

    stopifnot(
      identical(
        active_focus(),
        "Site"
      ),

      identical(
        future_site_round_trip_context$site,
        "Esports"
      ),

      "Esports" %in%
        available_executive_sites()
    )
      }
    )

  cat(
    "PASS Executive Dashboard visual picker\n"
  )