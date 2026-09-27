# -------------------------------------------------------------------------
# Site Comparison calculations
# -------------------------------------------------------------------------

make_site_finance_rows <- function(
    year,
    site,
    is_summary,
    income_actual,
    income_budget,
    expenses_actual,
    expenses_budget
) {

  data.frame(
    report_year = rep(
      as.integer(year),
      4L
    ),
    site = rep(
      site,
      4L
    ),
    is_summary = rep(
      is_summary,
      4L
    ),
    metric = c(
      "Income",
      "Income",
      "Expenses",
      "Expenses"
    ),
    value_type = c(
      "Actual",
      "Budget",
      "Actual",
      "Budget"
    ),
    value = c(
      income_actual,
      income_budget,
      expenses_actual,
      expenses_budget
    ),
    stringsAsFactors = FALSE
  )
}


site_finance_fixture <- list(
  finance = rbind(

    make_site_finance_rows(
      2024,
      "H&F Clayton",
      TRUE,
      100,
      95,
      60,
      55
    ),

    make_site_finance_rows(
      2024,
      "Venues Clayton",
      TRUE,
      80,
      85,
      50,
      48
    ),

    make_site_finance_rows(
      2025,
      "H&F Clayton",
      TRUE,
      120,
      110,
      70,
      65
    ),

    make_site_finance_rows(
      2025,
      "Venues Clayton",
      TRUE,
      90,
      88,
      55,
      52
    ),

    # Organisation-wide values must not appear as another site.
    make_site_finance_rows(
      2024,
      "Monash Sport",
      TRUE,
      999,
      999,
      999,
      999
    ),

    # Non-summary values must also be ignored.
    make_site_finance_rows(
      2024,
      "H&F Clayton",
      FALSE,
      888,
      888,
      888,
      888
    )
  )
)


site_annual <- calculate_site_annual_finance(
  site_finance_fixture
)


stopifnot(
  nrow(site_annual) == 4L,
  !any(site_annual$site == "Monash Sport"),
  all(
    site_annual$aggregation_source ==
      "Site YTD Summary"
  )
)


hf_2024 <- site_annual[
  site_annual$report_year == 2024L &
    site_annual$site == "H&F Clayton",
  ,
  drop = FALSE
]


stopifnot(
  nrow(hf_2024) == 1L,
  hf_2024$income_actual[[1L]] == 100,
  hf_2024$income_budget[[1L]] == 95,
  hf_2024$expenses_actual[[1L]] == 60,
  hf_2024$expenses_budget[[1L]] == 55,
  hf_2024$net_actual[[1L]] == 40,
  hf_2024$net_budget[[1L]] == 40,
  hf_2024$budget_variance[[1L]] == 0
)


venues_2024 <- site_annual[
  site_annual$report_year == 2024L &
    site_annual$site == "Venues Clayton",
  ,
  drop = FALSE
]


stopifnot(
  nrow(venues_2024) == 1L,
  venues_2024$income_actual[[1L]] == 80,
  venues_2024$income_budget[[1L]] == 85,
  venues_2024$expenses_actual[[1L]] == 50,
  venues_2024$expenses_budget[[1L]] == 48,
  venues_2024$net_actual[[1L]] == 30,
  venues_2024$net_budget[[1L]] == 37,
  venues_2024$budget_variance[[1L]] == -7
)


hf_2025 <- site_annual[
  site_annual$report_year == 2025L &
    site_annual$site == "H&F Clayton",
  ,
  drop = FALSE
]


stopifnot(
  hf_2025$net_actual[[1L]] == 50,
  hf_2025$net_budget[[1L]] == 45,
  hf_2025$budget_variance[[1L]] == 5
)


stopifnot(
  nrow(
    calculate_site_annual_finance(NULL)
  ) == 0L
)


# -------------------------------------------------------------------------
# Site metric extraction
# -------------------------------------------------------------------------

stopifnot(
  identical(
    site_finance_values(
      site_annual,
      "Income",
      "Actual"
    ),
    c(100, 80, 120, 90)
  ),

  identical(
    site_finance_values(
      site_annual,
      "Expenses",
      "Actual"
    ),
    c(60, 50, 70, 55)
  ),

  identical(
    site_finance_values(
      site_annual,
      "Net Result",
      "Actual"
    ),
    c(40, 30, 50, 35)
  ),

  identical(
    site_finance_values(
      site_annual,
      "Net Result",
      "Budget"
    ),
    c(40, 37, 45, 36)
  )
)


# -------------------------------------------------------------------------
# Site Performance Over Time plot
# -------------------------------------------------------------------------

site_trend_file <- tempfile(
  fileext = ".png"
)

grDevices::png(
  site_trend_file,
  width = 1100,
  height = 600,
  res = 96
)

plot_site_performance_over_time(
  site_annual,
  "Income"
)

grDevices::dev.off()


stopifnot(
  file.exists(site_trend_file),
  file.info(site_trend_file)$size > 0L
)


unlink(site_trend_file)

# -------------------------------------------------------------------------
# Site Ranking plot
# -------------------------------------------------------------------------

ranking_2025 <- site_annual[
  site_annual$report_year == 2025L,
  ,
  drop = FALSE
]


site_ranking_file <- tempfile(
  fileext = ".png"
)

grDevices::png(
  site_ranking_file,
  width = 850,
  height = 520,
  res = 96
)

plot_site_ranking(
  ranking_2025,
  "Income"
)

grDevices::dev.off()


stopifnot(
  file.exists(site_ranking_file),
  file.info(site_ranking_file)$size > 0L
)


unlink(site_ranking_file)

# -------------------------------------------------------------------------
# Site Budget Performance plot
# -------------------------------------------------------------------------

site_budget_file <- tempfile(
  fileext = ".png"
)

grDevices::png(
  site_budget_file,
  width = 1000,
  height = 520,
  res = 96
)

plot_site_budget_performance(
  ranking_2025,
  "Income"
)

grDevices::dev.off()


stopifnot(
  file.exists(site_budget_file),
  file.info(site_budget_file)$size > 0L
)


unlink(site_budget_file)

# -------------------------------------------------------------------------
# Site Comparison module
# -------------------------------------------------------------------------

site_dashboard_data <- shiny::reactiveVal(
  list(
    monthly_reports = site_finance_fixture
  )
)


shiny::testServer(
  mod_site_comparison_server,
  args = list(
    data = site_dashboard_data
  ),
  {

    session$setInputs(
      from_year = "2024",
      to_year = "2025",
      metric = "Income",
      selected_site = "H&F Clayton"
    )

    session$flushReact()

    # Accessing the outputs forces the renderPlot expressions to execute.
    stopifnot(
      !is.null(
        output$site_performance_trend
      ),
      !is.null(
        output$site_ranking
      ),
      !is.null(
        output$site_budget_performance
      )
    )
  }
)

cat("PASS test_site_comparison.R\n")
