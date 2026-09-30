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
# Site Revenue Opportunity calculations
# -------------------------------------------------------------------------

make_site_revenue_row <- function(
    year,
    site,
    is_summary,
    actual_income,
    discount_opportunity,
    reported_total_revenue,
    validation_status = "pass"
) {

  data.frame(
    report_year = as.integer(year),
    site = site,
    is_summary = is_summary,
    actual_income = actual_income,
    discount_opportunity = discount_opportunity,
    reported_total_revenue = reported_total_revenue,
    validation_status = validation_status,
    stringsAsFactors = FALSE
  )
}


site_revenue_fixture <- list(
  total_revenue = rbind(

    make_site_revenue_row(
      2025,
      "H&F Clayton",
      TRUE,
      120,
      30,
      150
    ),

    make_site_revenue_row(
      2025,
      "Venues Clayton",
      TRUE,
      90,
      10,
      100
    ),

    # Organisation total must not appear as another site.
    make_site_revenue_row(
      2025,
      "Monash Sport",
      TRUE,
      999,
      999,
      1998
    ),

    # Non-summary rows must be ignored.
    make_site_revenue_row(
      2025,
      "H&F Clayton",
      FALSE,
      888,
      888,
      1776
    )
  )
)


site_revenue <- calculate_site_annual_revenue(
  site_revenue_fixture
)


stopifnot(
  nrow(site_revenue) == 2L,
  !any(
    site_revenue$site == "Monash Sport"
  )
)


hf_revenue_2025 <- site_revenue[
  site_revenue$report_year == 2025L &
    site_revenue$site == "H&F Clayton",
  ,
  drop = FALSE
]


stopifnot(
  nrow(hf_revenue_2025) == 1L,
  hf_revenue_2025$actual_income[[1L]] == 120,
  hf_revenue_2025$discount_opportunity[[1L]] == 30,
  hf_revenue_2025$potential_revenue[[1L]] == 150,
  hf_revenue_2025$reported_total_revenue[[1L]] == 150,
  hf_revenue_2025$validation_difference[[1L]] == 0,
  hf_revenue_2025$capture_rate[[1L]] == 0.8,
  hf_revenue_2025$validation_status[[1L]] == "pass",
  hf_revenue_2025$aggregation_source[[1L]] ==
    "Site YTD Summary"
)


stopifnot(
  nrow(
    calculate_site_annual_revenue(NULL)
  ) == 0L
)

# -------------------------------------------------------------------------
# Site Discount Mix calculations
# -------------------------------------------------------------------------

make_site_discount_rows <- function(
    year,
    site,
    is_summary,
    student,
    staff,
    other
) {

  data.frame(
    report_year = rep(
      as.integer(year),
      3L
    ),
    site = rep(
      site,
      3L
    ),
    is_summary = rep(
      is_summary,
      3L
    ),
    discount_type = c(
      "Student",
      "Staff",
      "Other"
    ),
    value = c(
      student,
      staff,
      other
    ),
    stringsAsFactors = FALSE
  )
}


site_discount_fixture <- list(
  discounts = rbind(

    # Two periods for H&F Clayton.
    make_site_discount_rows(
      2025,
      "H&F Clayton",
      TRUE,
      10,
      5,
      2
    ),

    make_site_discount_rows(
      2025,
      "H&F Clayton",
      TRUE,
      20,
      6,
      3
    ),

    # Two periods for Venues Clayton.
    make_site_discount_rows(
      2025,
      "Venues Clayton",
      TRUE,
      5,
      2,
      1
    ),

    make_site_discount_rows(
      2025,
      "Venues Clayton",
      TRUE,
      7,
      3,
      2
    ),

    # Organisation total must not appear as another site.
    make_site_discount_rows(
      2025,
      "Monash Sport",
      TRUE,
      999,
      999,
      999
    ),

    # Non-summary rows must be ignored.
    make_site_discount_rows(
      2025,
      "H&F Clayton",
      FALSE,
      888,
      888,
      888
    )
  )
)


site_discounts <- calculate_site_annual_discounts(
  site_discount_fixture
)


stopifnot(
  nrow(site_discounts) == 2L,
  !any(
    site_discounts$site == "Monash Sport"
  )
)


hf_discounts_2025 <- site_discounts[
  site_discounts$report_year == 2025L &
    site_discounts$site == "H&F Clayton",
  ,
  drop = FALSE
]


stopifnot(
  nrow(hf_discounts_2025) == 1L,
  hf_discounts_2025$student_discount[[1L]] == 30,
  hf_discounts_2025$staff_discount[[1L]] == 11,
  hf_discounts_2025$other_discount[[1L]] == 5,
  hf_discounts_2025$total_discount[[1L]] == 46,
  hf_discounts_2025$aggregation_source[[1L]] ==
    "Site YTD Summary"
)


venues_discounts_2025 <- site_discounts[
  site_discounts$report_year == 2025L &
    site_discounts$site == "Venues Clayton",
  ,
  drop = FALSE
]


stopifnot(
  venues_discounts_2025$student_discount[[1L]] == 12,
  venues_discounts_2025$staff_discount[[1L]] == 5,
  venues_discounts_2025$other_discount[[1L]] == 3,
  venues_discounts_2025$total_discount[[1L]] == 20
)


stopifnot(
  nrow(
    calculate_site_annual_discounts(NULL)
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
# Site Revenue Opportunity plot
# -------------------------------------------------------------------------

site_revenue_file <- tempfile(
  fileext = ".png"
)

grDevices::png(
  site_revenue_file,
  width = 1100,
  height = 550,
  res = 96
)

plot_site_revenue_opportunity(
  site_revenue
)

grDevices::dev.off()


stopifnot(
  file.exists(site_revenue_file),
  file.info(site_revenue_file)$size > 0L
)


unlink(site_revenue_file)

# -------------------------------------------------------------------------
# Site Discount Mix plot
# -------------------------------------------------------------------------

site_discount_mix_file <- tempfile(
  fileext = ".png"
)

grDevices::png(
  site_discount_mix_file,
  width = 900,
  height = 520,
  res = 96
)

plot_site_discount_mix(
  site_discounts
)

grDevices::dev.off()


stopifnot(
  file.exists(site_discount_mix_file),
  file.info(site_discount_mix_file)$size > 0L
)


unlink(site_discount_mix_file)

# -------------------------------------------------------------------------
# Facility annual finance calculations
# -------------------------------------------------------------------------

make_facility_finance_row <- function(
    year,
    report_unit,
    campus,
    business_unit,
    period_type,
    is_summary,
    metric,
    value_type,
    value
) {

  data.frame(
    report_year = as.integer(year),
    period_type = period_type,
    report_unit = report_unit,
    campus = campus,
    business_unit = business_unit,
    is_summary = is_summary,
    metric = metric,
    value_type = value_type,
    value = value,
    stringsAsFactors = FALSE
  )
}


facility_finance_fixture <- list(
  finance = rbind(

    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Reported Total",
      FALSE,
      "Income",
      "Actual",
      120
    ),

    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Reported Total",
      FALSE,
      "Income",
      "Budget",
      110
    ),

    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Reported Total",
      FALSE,
      "Expenses",
      "Actual",
      70
    ),

    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Reported Total",
      FALSE,
      "Expenses",
      "Budget",
      65
    ),

    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Pool Hire",
      "Reported Total",
      FALSE,
      "Income",
      "Actual",
      80
    ),

    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Pool Hire",
      "Reported Total",
      FALSE,
      "Income",
      "Budget",
      75
    ),

    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Pool Hire",
      "Reported Total",
      FALSE,
      "Expenses",
      "Actual",
      40
    ),

    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Pool Hire",
      "Reported Total",
      FALSE,
      "Expenses",
      "Budget",
      38
    ),

    # Summary row must not be ranked as a facility.
    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "YTD Summary",
      "Reported Total",
      TRUE,
      "Income",
      "Actual",
      999
    ),

    # Monthly row must not be mixed with the annual reported total.
    make_facility_finance_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Month",
      FALSE,
      "Income",
      "Actual",
      999
    )
  )
)


facility_finance <- calculate_facility_annual_finance(
  facility_finance_fixture
)


stopifnot(
  nrow(facility_finance) == 2L,
  !any(
    facility_finance$business_unit ==
      "YTD Summary"
  )
)


swim_school_2025 <- facility_finance[
  facility_finance$report_year == 2025L &
    facility_finance$report_unit == "Aquatics" &
    facility_finance$business_unit == "Swim School",
  ,
  drop = FALSE
]


stopifnot(
  nrow(swim_school_2025) == 1L,
  swim_school_2025$income_actual[[1L]] == 120,
  swim_school_2025$income_budget[[1L]] == 110,
  swim_school_2025$expenses_actual[[1L]] == 70,
  swim_school_2025$expenses_budget[[1L]] == 65,
  swim_school_2025$net_actual[[1L]] == 50,
  swim_school_2025$net_budget[[1L]] == 45,
  swim_school_2025$budget_variance[[1L]] == 5
)


stopifnot(
  nrow(
    calculate_facility_annual_finance(NULL)
  ) == 0L
)

# Facility site mapping ----------------------------------------------------

aquatics_key <- facility_site_key(
  "Aquatics Clayton"
)

stopifnot(
  identical(
    aquatics_key$report_unit,
    "Aquatics"
  ),
  identical(
    aquatics_key$campus,
    "Clayton"
  )
)


hf_key <- facility_site_key(
  "H&F Clayton"
)

stopifnot(
  identical(
    hf_key$report_unit,
    "Health & Fitness"
  ),
  identical(
    hf_key$campus,
    "Clayton"
  )
)


participation_key <- facility_site_key(
  "Participation Programs"
)

stopifnot(
  identical(
    participation_key$report_unit,
    "Participation Programs"
  ),
  identical(
    participation_key$campus,
    "Multi-campus"
  )
)


filtered_aquatics <- filter_facility_finance_by_site(
  facility_finance,
  "Aquatics Clayton"
)

stopifnot(
  nrow(filtered_aquatics) == 2L,
  all(
    filtered_aquatics$report_unit ==
      "Aquatics"
  ),
  all(
    filtered_aquatics$campus ==
      "Clayton"
  )
)


stopifnot(
  nrow(
    filter_facility_finance_by_site(
      facility_finance,
      "Unknown Site"
    )
  ) == 0L
)

# -------------------------------------------------------------------------
# Facility Ranking plot
# -------------------------------------------------------------------------

stopifnot(
  identical(
    facility_finance_values(
      filtered_aquatics,
      "Income",
      "Actual"
    ),
    c(
      80,
      120
    )
  ) ||
    identical(
      sort(
        facility_finance_values(
          filtered_aquatics,
          "Income",
          "Actual"
        )
      ),
      c(
        80,
        120
      )
    )
)


facility_ranking_file <- tempfile(
  fileext = ".png"
)

grDevices::png(
  facility_ranking_file,
  width = 900,
  height = 520,
  res = 96
)

plot_facility_ranking(
  filtered_aquatics,
  "Income"
)

grDevices::dev.off()


stopifnot(
  file.exists(
    facility_ranking_file
  ),
  file.info(
    facility_ranking_file
  )$size > 0L
)


unlink(
  facility_ranking_file
)

# -------------------------------------------------------------------------
# Facility annual discount calculations
# -------------------------------------------------------------------------

make_facility_discount_row <- function(
    year,
    report_unit,
    campus,
    business_unit,
    period_type,
    is_summary,
    discount_type,
    value
) {

  data.frame(
    report_year = as.integer(year),
    period_type = period_type,
    report_unit = report_unit,
    campus = campus,
    business_unit = business_unit,
    is_summary = is_summary,
    discount_type = discount_type,
    value = value,
    stringsAsFactors = FALSE
  )
}


facility_discount_fixture <- list(
  discounts = rbind(

    make_facility_discount_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Reported Total",
      FALSE,
      "Student",
      30
    ),

    make_facility_discount_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Reported Total",
      FALSE,
      "Staff",
      10
    ),

    make_facility_discount_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Reported Total",
      FALSE,
      "Other",
      5
    ),

    make_facility_discount_row(
      2025,
      "Aquatics",
      "Clayton",
      "Pool Hire",
      "Reported Total",
      FALSE,
      "Student",
      12
    ),

    make_facility_discount_row(
      2025,
      "Aquatics",
      "Clayton",
      "Pool Hire",
      "Reported Total",
      FALSE,
      "Staff",
      4
    ),

    make_facility_discount_row(
      2025,
      "Aquatics",
      "Clayton",
      "Pool Hire",
      "Reported Total",
      FALSE,
      "Other",
      2
    ),

    # Summary value must not be treated as a facility.
    make_facility_discount_row(
      2025,
      "Aquatics",
      "Clayton",
      "YTD Summary",
      "Reported Total",
      TRUE,
      "Student",
      999
    ),

    # Monthly value must not be added to the reported annual total.
    make_facility_discount_row(
      2025,
      "Aquatics",
      "Clayton",
      "Swim School",
      "Month",
      FALSE,
      "Student",
      999
    )
  )
)


facility_discounts <- calculate_facility_annual_discounts(
  facility_discount_fixture
)


stopifnot(
  nrow(facility_discounts) == 2L,
  !any(
    facility_discounts$business_unit ==
      "YTD Summary"
  )
)


swim_school_discounts <- facility_discounts[
  facility_discounts$business_unit ==
    "Swim School",
  ,
  drop = FALSE
]


stopifnot(
  nrow(swim_school_discounts) == 1L,
  swim_school_discounts$student_discount[[1L]] == 30,
  swim_school_discounts$staff_discount[[1L]] == 10,
  swim_school_discounts$other_discount[[1L]] == 5,
  swim_school_discounts$total_discount[[1L]] == 45
)


filtered_aquatics_discounts <-
  filter_facility_discounts_by_site(
    facility_discounts,
    "Aquatics Clayton"
  )


stopifnot(
  nrow(filtered_aquatics_discounts) == 2L,
  all(
    filtered_aquatics_discounts$report_unit ==
      "Aquatics"
  ),
  all(
    filtered_aquatics_discounts$campus ==
      "Clayton"
  )
)


stopifnot(
  nrow(
    calculate_facility_annual_discounts(NULL)
  ) == 0L,
  nrow(
    filter_facility_discounts_by_site(
      facility_discounts,
      "Unknown Site"
    )
  ) == 0L
)

# -------------------------------------------------------------------------
# Facility Discount Mix plot
# -------------------------------------------------------------------------

facility_discount_mix_file <- tempfile(
  fileext = ".png"
)


grDevices::png(
  facility_discount_mix_file,
  width = 900,
  height = 520,
  res = 96
)


plot_facility_discount_mix(
  filtered_aquatics_discounts
)


grDevices::dev.off()


stopifnot(
  file.exists(
    facility_discount_mix_file
  ),
  file.info(
    facility_discount_mix_file
  )$size > 0L
)


unlink(
  facility_discount_mix_file
)

# -------------------------------------------------------------------------
# Site Comparison module
# -------------------------------------------------------------------------

site_dashboard_reports <- list(
  finance = site_finance_fixture$finance,
  total_revenue = site_revenue_fixture$total_revenue,
  discounts = site_discount_fixture$discounts
)

facility_dashboard_reports <- list(
  finance = facility_finance_fixture$finance,
  discounts = facility_discount_fixture$discounts
)

site_dashboard_data <- shiny::reactiveVal(
  list(
    monthly_reports = site_dashboard_reports,
    site_reports = facility_dashboard_reports
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
      selected_site = "Aquatics Clayton"
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
      ),
      !is.null(
        output$site_revenue_opportunity
      ),
      !is.null(
        output$site_discount_mix
      ),
      !is.null(
        output$facility_ranking
      ),
      !is.null(
        output$facility_ranking
      )
    )
  }
)

cat("PASS test_site_comparison.R\n")
