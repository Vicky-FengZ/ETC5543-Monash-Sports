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
  nrow(registry) == 15L,
  length(unique(registry$id)) == nrow(registry),
  all(
    c(
      "id",
      "title",
      "category",
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


# Source grouping ---------------------------------------------------------

monthly_ids <- executive_visual_ids(
  source_group = "monthly"
)

detailed_ids <- executive_visual_ids(
  source_group = "detailed"
)

stopifnot(
  length(monthly_ids) == 9L,
  length(detailed_ids) == 6L,
  length(
    intersect(
      monthly_ids,
      detailed_ids
    )
  ) == 0L
)

stopifnot(
  identical(
    executive_visual_source_label("monthly"),
    "Monthly Site Reports"
  ),
  identical(
    executive_visual_source_label("detailed"),
    "Detailed Site Reports"
  )
)


# Default Executive Finance set ------------------------------------------

expected_defaults <- c(
  "monthly_income_expenses",
  "annual_performance_trend",
  "budget_vs_actual",
  "site_ranking",
  "site_revenue_opportunity",
  "site_discount_mix",
  "revenue_capture"
)

stopifnot(
  identical(
    executive_default_visual_ids(),
    expected_defaults
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
  nrow(empty_summary) == 2L
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
  empty_monthly$total_visuals[[1L]] == 9L,

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
  monthly_row$available_visuals[[1L]] == 9L,
  monthly_row$total_visuals[[1L]] == 9L,

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
  complete_monthly$available_visuals[[1L]] == 9L,
  complete_monthly$total_visuals[[1L]] == 9L,

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


cat(
  "PASS Executive Dashboard visual registry\n"
)

# -------------------------------------------------------------------------
# Executive Dashboard visual picker
# -------------------------------------------------------------------------

picker_test_data <- shiny::reactiveVal(
  list(

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


    # ---------------------------------------------------------------------
    # Default seven visuals are retained
    # ---------------------------------------------------------------------

    stopifnot(
      identical(
        selected_visuals$Finance,
        expected_defaults
      )
    )


    # ---------------------------------------------------------------------
    # Monthly data makes all 9 Monthly Site visuals available
    # ---------------------------------------------------------------------

    finance_registry <- picker_registry()

    monthly_registry <- finance_registry[
      finance_registry$source_group ==
        "monthly",
      ,
      drop = FALSE
    ]

    detailed_registry <- finance_registry[
      finance_registry$source_group ==
        "detailed",
      ,
      drop = FALSE
    ]

    stopifnot(
      nrow(monthly_registry) == 9L,
      all(monthly_registry$available),

      nrow(detailed_registry) == 6L,
      !any(detailed_registry$available)
    )


    # ---------------------------------------------------------------------
    # Apply a custom visual selection
    # ---------------------------------------------------------------------

    session$setInputs(
      visuals_monthly = c(
        "site_performance",
        "site_ranking"
      ),
      apply_visuals = 1
    )

    session$flushReact()

    stopifnot(
      identical(
        selected_visuals$Finance,
        c(
          "site_performance",
          "site_ranking"
        )
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
      apply_visuals = 2
    )

    session$flushReact()

    stopifnot(
      identical(
        selected_visuals$Finance,
        "site_ranking"
      )
    )


    # ---------------------------------------------------------------------
    # Finance and Metrics maintain independent selections
    # ---------------------------------------------------------------------

    session$setInputs(
      dashboard_section = "metrics"
    )

    session$flushReact()

    stopifnot(
      identical(
        active_category(),
        "Metrics"
      ),

      length(
        selected_visuals$Metrics
      ) == 0L,

      nrow(
        picker_registry()
      ) == 0L,

      identical(
        selected_visuals$Finance,
        "site_ranking"
      )
    )


    # Return to Finance and confirm its selection was preserved.
    session$setInputs(
      dashboard_section = "finance"
    )

    session$flushReact()

    stopifnot(
      identical(
        selected_visuals$Finance,
        "site_ranking"
      )
    )
  }
)


cat(
  "PASS Executive Dashboard visual picker\n"
)