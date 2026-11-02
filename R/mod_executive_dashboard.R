# -------------------------------------------------------------------------
# Executive Dashboard module
# Overview, Site, and Venues share the same year controls but intentionally
# have separate KPI definitions and presentation layouts.
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Executive Dashboard visual UI helpers
# -------------------------------------------------------------------------

executive_visual_context_label <- function(
    view_mode,
    year,
    site,
    comparison_year = NULL,
    metric = NULL
) {

  parts <- character()

  view_mode_value <- NULL

  if (
    !is.null(view_mode) &&
      length(view_mode) > 0L &&
      !is.na(view_mode[[1L]]) &&
      nzchar(as.character(view_mode[[1L]]))
  ) {

    view_mode_value <-
      as.character(
        view_mode[[1L]]
      )

    parts <- c(
      parts,
      view_mode_value
    )
  }

  year_value <- NULL

  if (
    !is.null(year) &&
      length(year) > 0L &&
      !is.na(year[[1L]]) &&
      nzchar(as.character(year[[1L]]))
  ) {

    year_value <-
      as.character(
        year[[1L]]
      )
  }

  comparison_value <- NULL

  if (
    !is.null(comparison_year) &&
      length(comparison_year) > 0L &&
      !is.na(comparison_year[[1L]]) &&
      nzchar(
        as.character(
          comparison_year[[1L]]
        )
      )
  ) {

    comparison_value <-
      as.character(
        comparison_year[[1L]]
      )
  }

  if (
    identical(
      view_mode_value,
      "Comparison"
    ) &&
      !is.null(year_value) &&
      !is.null(comparison_value)
  ) {

    parts <- c(
      parts,
      paste0(
        year_value,
        " vs ",
        comparison_value
      )
    )

  } else if (!is.null(year_value)) {

    parts <- c(
      parts,
      year_value
    )
  }

  if (
    !is.null(site) &&
      length(site) > 0L &&
      !is.na(site[[1L]]) &&
      nzchar(as.character(site[[1L]]))
  ) {

    parts <- c(
      parts,
      as.character(
        site[[1L]]
      )
    )
  }

  if (
    !is.null(metric) &&
      length(metric) > 0L &&
      !is.na(metric[[1L]]) &&
      nzchar(as.character(metric[[1L]]))
  ) {

    parts <- c(
      parts,
      as.character(
        metric[[1L]]
      )
    )
  }

  paste(
    parts,
    collapse = " · "
  )
}

executive_default_comparison_year <- function(
    year,
    years
) {

  if (
    is.null(year) ||
      length(year) == 0L ||
      is.na(year[[1L]])
  ) {
    return(NULL)
  }

  year_value <-
    as.character(
      year[[1L]]
    )

  candidates <- setdiff(
    unique(
      as.character(
        years
      )
    ),
    year_value
  )

  if (length(candidates) == 0L) {
    return(NULL)
  }

  year_number <-
    suppressWarnings(
      as.numeric(
        year_value
      )
    )

  candidate_numbers <-
    suppressWarnings(
      as.numeric(
        candidates
      )
    )

  if (
    !is.na(year_number) &&
      all(
        !is.na(
          candidate_numbers
        )
      )
  ) {

    return(
      candidates[
        which.min(
          abs(
            candidate_numbers -
              year_number
          )
        )
      ]
    )
  }

  candidates[[1L]]
}

# Each focus has a different data source: Overview from monthly finance,
# Sites from monthly plus detailed reports, Venues from tracking reports.
executive_available_years <- function(
    dashboard_data,
    focus
) {

  if (
    is.null(focus) ||
      length(focus) == 0L ||
      is.na(focus[[1L]])
  ) {
    return(integer())
  }

  focus <- as.character(
    focus[[1L]]
  )


  if (identical(focus, "Overview")) {
    monthly_reports <- executive_source_data(
      dashboard_data = dashboard_data, source_group = "monthly"
    )
    return(reporting_monthly_years(monthly_reports))
  }

  if (identical(focus, "Site")) {
    monthly_reports <- executive_source_data(
      dashboard_data = dashboard_data, source_group = "monthly"
    )
    detailed_reports <- executive_source_data(
      dashboard_data = dashboard_data, source_group = "detailed"
    )
    return(reporting_normalize_years(c(
      reporting_monthly_years(monthly_reports),
      reporting_detailed_years(detailed_reports)
    )))
  }


  if (identical(
    focus,
    "Metrics"
  )) {

    venues_reports <-
      executive_source_data(
        dashboard_data = dashboard_data,
        source_group = "venues"
      )

    return(
      reporting_venues_years(
        venues_reports
      )
    )
  }


  integer()
}

executive_site_choices <- function(
    monthly_reports
) {

  site_values <- unlist(
    lapply(
      c(
        "finance",
        "discounts",
        "total_revenue"
      ),
      function(table_name) {

        if (
          is.null(monthly_reports) ||
            !is.list(monthly_reports) ||
            !table_name %in%
              names(monthly_reports)
        ) {
          return(character())
        }

        table_data <-
          monthly_reports[[table_name]]

        if (
          is.null(table_data) ||
            !is.data.frame(table_data) ||
            nrow(table_data) == 0L ||
            !"site" %in%
              names(table_data)
        ) {
          return(character())
        }

        sites <- trimws(
          as.character(
            table_data$site
          )
        )

        sites[
          !is.na(sites) &
            nzchar(sites)
        ]
      }
    ),
    use.names = FALSE
  )

  site_values <- unique(
    site_values
  )

  if (length(site_values) == 0L) {
    return(
      "All Monash Sport"
    )
  }

  known_sites <-
    SITE_NAMES[
      SITE_NAMES %in%
        site_values
    ]

  discovered_sites <-
    sort(
      setdiff(
        site_values,
        SITE_NAMES
      )
    )

  c(
    "All Monash Sport",
    known_sites,
    discovered_sites
  )
}

executive_reconcile_site_selection <- function(
    site,
    choices
) {

  choices <- unique(
    as.character(
      choices
    )
  )

  choices <- choices[
    !is.na(choices) &
      nzchar(
        trimws(choices)
      )
  ]

  fallback <- if (
    "All Monash Sport" %in% choices
  ) {
    "All Monash Sport"
  } else if (
    length(choices) > 0L
  ) {
    choices[[1L]]
  } else {
    "All Monash Sport"
  }

  if (
    is.null(site) ||
      length(site) == 0L ||
      is.na(site[[1L]]) ||
      !nzchar(
        trimws(
          as.character(
            site[[1L]]
          )
        )
      )
  ) {
    return(fallback)
  }

  current_site <-
    as.character(
      site[[1L]]
    )

  if (current_site %in% choices) {
    return(current_site)
  }

  fallback
}


executive_comparison_year_choices <- function(
    year,
    years
) {

  years <-
    reporting_normalize_years(
      years
    )

  if (length(years) == 0L) {
    return(character())
  }


  if (
    is.null(year) ||
      length(year) == 0L ||
      is.na(year[[1L]]) ||
      !nzchar(
        as.character(
          year[[1L]]
        )
      )
  ) {

    return(
      as.character(
        years
      )
    )
  }


  setdiff(
    as.character(years),
    as.character(year[[1L]])
  )
}

executive_reconcile_year_selection <- function(
    year,
    years
) {

  years <-
    reporting_normalize_years(
      years
    )

  if (length(years) == 0L) {
    return("")
  }

  current_year <- if (
    !is.null(year) &&
      length(year) > 0L &&
      !is.na(year[[1L]]) &&
      nzchar(
        as.character(
          year[[1L]]
        )
      )
  ) {
    as.character(
      year[[1L]]
    )
  } else {
    ""
  }

  if (
    current_year %in%
      as.character(years)
  ) {
    return(current_year)
  }

  as.character(
    reporting_latest_year(
      years
    )
  )
}

executive_reconcile_comparison_year <- function(
    year,
    comparison_year,
    years
) {

  choices <-
    executive_comparison_year_choices(
      year = year,
      years = years
    )

  if (length(choices) == 0L) {
    return("")
  }

  current_comparison <- if (
    !is.null(comparison_year) &&
      length(comparison_year) > 0L &&
      !is.na(comparison_year[[1L]]) &&
      nzchar(
        as.character(
          comparison_year[[1L]]
        )
      )
  ) {
    as.character(
      comparison_year[[1L]]
    )
  } else {
    ""
  }

  if (current_comparison %in% choices) {
    return(current_comparison)
  }

  executive_default_comparison_year(
    year = year,
    years = years
  )
}

executive_reconcile_view_mode <- function(
    view_mode,
    years
) {

  years <-
    reporting_normalize_years(
      years
    )

  if (length(years) < 2L) {
    return("Year Review")
  }

  current_view <- if (
    !is.null(view_mode) &&
      length(view_mode) > 0L &&
      !is.na(view_mode[[1L]])
  ) {
    as.character(
      view_mode[[1L]]
    )
  } else {
    ""
  }

  if (
    current_view %in%
      c(
        "Year Review",
        "Comparison"
      )
  ) {
    return(current_view)
  }

  "Year Review"
}

executive_filter_monthly_reports_by_site <- function(
    monthly_reports,
    site
) {

  if (
    is.null(monthly_reports) ||
      !is.list(monthly_reports)
  ) {
    return(monthly_reports)
  }

  if (
    is.null(site) ||
      length(site) == 0L ||
      identical(
        as.character(site[[1L]]),
        "All Monash Sport"
      )
  ) {
    return(monthly_reports)
  }

  selected_site <-
    as.character(site[[1L]])

  result <- monthly_reports

  for (
    dataset in c(
      "finance",
      "discounts",
      "total_revenue"
    )
  ) {

    x <- result[[dataset]]

    if (
      is.null(x) ||
        !is.data.frame(x) ||
        !"site" %in% names(x)
    ) {
      next
    }

    site_values <-
      trimws(
        as.character(
          x$site
        )
      )

    result[[dataset]] <-
      x[
        !is.na(site_values) &
          site_values == selected_site,
        ,
        drop = FALSE
      ]
  }

  class(result) <-
    class(monthly_reports)

  result
}


executive_context_years <- function(
    context
) {

  if (
    is.null(context) ||
      is.null(context$year)
  ) {
    return(integer())
  }

  years <- context$year

  if (
    identical(
      context$view_mode,
      "Comparison"
    ) &&
      !is.null(
        context$comparison_year
      )
  ) {

    years <- c(
      years,
      context$comparison_year
    )
  }

  years <-
    suppressWarnings(
      as.integer(
        years
      )
    )

  sort(
    unique(
      years[
        !is.na(years)
      ]
    )
  )
}

executive_context_trend_years <- function(
    context,
    available_years
  ) {

    available_years <-
      suppressWarnings(
        as.integer(
          available_years
        )
      )

    available_years <-
      sort(
        unique(
          available_years[
            !is.na(available_years)
          ]
        )
      )

    if (
      is.null(context) ||
        is.null(context$year)
    ) {
      return(integer())
    }

    selected_year <-
      suppressWarnings(
        as.integer(
          context$year[[1L]]
        )
      )

    if (is.na(selected_year)) {
      return(integer())
    }

    if (
      identical(
        context$view_mode,
        "Comparison"
      )
    ) {

      return(
        executive_context_years(
          context
        )
      )
    }

    available_years[
      available_years <= selected_year
    ]
  }

executive_visual_metric_control_ui <- function(
    visual_id,
    ns
) {

  metric_choices <- FINANCE_METRICS

  switch(
    visual_id,

    annual_performance_trend = NULL,

    budget_vs_actual =
      div(
        class =
          "chart-header-control executive-visual-metric-control",

        selectInput(
          ns("metric_budget_vs_actual"),
          label = NULL,
          choices = metric_choices,
          selected = "Net Result"
        )
      ),

    site_performance =
      div(
        class =
          "chart-header-control executive-visual-metric-control",

        selectInput(
          ns("metric_site_performance"),
          label = NULL,
          choices = metric_choices,
          selected = "Net Result"
        )
      ),

    site_ranking =
      div(
        class =
          "chart-header-control executive-visual-metric-control",

        selectInput(
          ns("metric_site_ranking"),
          label = NULL,
          choices = metric_choices,
          selected = "Net Result"
        )
      ),

    facility_ranking =
      div(
        class =
          "chart-header-control executive-visual-metric-control",

        selectInput(
          ns("metric_facility_ranking"),
          label = NULL,
          choices = metric_choices,
          selected = "Net Result"
        )
      ),

    facility_performance =
      div(
        class =
          "chart-header-control executive-visual-metric-control",

        selectInput(
          ns("metric_facility_performance"),
          label = NULL,
          choices = metric_choices,
          selected = "Net Result"
        )
      ),  

    site_budget_performance =
      div(
        class =
          "chart-header-control executive-visual-metric-control",

        selectInput(
          ns("metric_site_budget_performance"),
          label = NULL,
          choices = metric_choices,
          selected = "Net Result"
        )
      ),

    NULL
  )
}

executive_visual_card_ui <- function(
    visual_id,
    context_label = NULL,
    ns
) {

  if (
    length(visual_id) != 1L ||
      !visual_id %in%
        names(EXECUTIVE_VISUAL_REGISTRY)
  ) {
    return(NULL)
  }

  visual <-
    EXECUTIVE_VISUAL_REGISTRY[[visual_id]]

  source_label <-
    executive_visual_source_label(
      visual$source_group
    )

  subtitle_ui <- NULL

  if (
    !is.null(context_label) &&
      length(context_label) > 0L &&
      nzchar(context_label[[1L]])
  ) {

    subtitle_ui <- shiny::div(
      class = "chart-header-subtitle",
      context_label[[1L]]
    )
  }

  metric_control <-
  executive_visual_metric_control_ui(
    visual_id = visual_id,
    ns = ns
  )

  bslib::card(
    class = paste(
      "executive-visual-card",

      paste0(
        "executive-visual-size-",
        visual$preferred_size
      ),

      paste0(
        "executive-visual-id-",
        visual$id
      )
    ),

    full_screen = TRUE,

    card_header(
      div(
        class = "executive-visual-card-header",

        div(
          class = "executive-visual-card-heading",

          div(
            class = "chart-header-title",
            visual$title
          ),

          subtitle_ui
        ),

        metric_control
      )
    ),

    plotOutput(
      ns(paste0("executive_plot_", visual_id)),
      height = "320px",
      hover = if (visual_id %in% c(
        "monthly_income_expenses",
        "annual_performance_trend",
        "budget_vs_actual",
        "revenue_capture"
      )) {
        hoverOpts(
          id = ns(paste0("executive_hover_", visual_id)),
          delay = 150,
          delayType = "debounce"
        )
      } else {
        NULL
      }
    ),

    if (visual_id %in% c(
      "monthly_income_expenses",
      "annual_performance_trend",
      "budget_vs_actual",
      "revenue_capture"
    )) {
      div(
        class = "executive-chart-hover-readout",
        textOutput(
          ns(paste0("executive_hover_value_", visual_id)),
          inline = TRUE
        )
      )
    }
  )
}

executive_visual_grid_ui <- function(
    visual_ids,
    context_label = NULL,
    context_label_overrides = NULL,
    ns
) {

  all_visual_ids <- unique(
    as.character(
      visual_ids
    )
  )

  all_visual_ids <- all_visual_ids[
    all_visual_ids %in%
      names(EXECUTIVE_VISUAL_REGISTRY)
  ]

  # Nothing at all has been selected.
  if (length(all_visual_ids) == 0L) { 

    return(
      div(
        class = "executive-dashboard-grid-empty",

        bs_icon(
          "grid",
          size = "26px"
        ),

        div(
          tags$strong(
            "No visuals selected"
          ),

          tags$p(
            paste(
              "Choose Visuals to add content",
              "to this Executive Dashboard."
            )
          )
        )
      )
    )
  }

  # Keep only graph-type components for the graph grid.
  graph_ids <- all_visual_ids[
    vapply(
      all_visual_ids, 
      function(visual_id) {

        identical(
          executive_visual_component_type(
            EXECUTIVE_VISUAL_REGISTRY[[visual_id]]
          ),
          "graph"
        )
      },
      logical(1)
    )
  ]

  # Tables/insights may be intentionally selected without any graphs.
  if (length(graph_ids) == 0L) {
    return(NULL)
  }

  shiny::div(
    class = "executive-dashboard-grid",

    tagList(
      lapply(
        graph_ids,
        function(visual_id) {

          visual_context_label <-
            context_label

          if (
            !is.null(context_label_overrides) &&
              visual_id %in%
                names(context_label_overrides)
          ) {

            visual_context_label <-
              context_label_overrides[[visual_id]]
          }

          executive_visual_card_ui(
            visual_id = visual_id,
            context_label = visual_context_label,
            ns = ns
          )
        }
      )
    )
  )
}

executive_site_values_card_ui <- function(
    ns
) {

  bslib::card(
    class = "executive-dashboard-table-card",
    full_screen = TRUE,

    card_header(
      div(
        class = "chart-header-layout",

        div(
          div(
            class = "chart-header-title",
            "Site Values"
          ),

          div(
            class = "chart-header-subtitle",
            "Exact values supporting the dashboard charts"
          )
        ),

        div(
          class = "chart-header-control",

          selectInput(
            ns("table_metric"),
            "Table metric",
            choices = c(
              "Net Result",
              "Budget Variance",
              "Discount Opportunity",
              "Revenue Capture Rate"
            ),
            selected = "Net Result"
          )
        )
      )
    ),

    div(
      class = "executive-site-values-table",

      tableOutput(
        ns("site_values_table")
      )
    )
  )
}

# Financial Summary ------------------------------------------------------
# Shows exact annual finance values for the selected reporting context.

executive_financial_summary_card_ui <- function(ns) {

  bslib::card(
    class = "executive-dashboard-table-card executive-financial-summary-card",
    full_screen = TRUE,

    card_header(
      div(
        class = "chart-header-title",
        "Financial Summary"
      )
    ),

    div(
      class = "executive-financial-summary-table",
      tableOutput(
        ns("financial_summary_table")
      )
    )
  )
}

executive_key_insights_card_ui <- function(
  ns
) {

  bslib::card(
    class = "executive-insights-card",
    full_screen = TRUE,

    card_header(
      "Key Insights"
    ),

    div(
      class = "executive-insights-placeholder",

      uiOutput(
        ns("key_insights")
      )
    )
  )
}


executive_supporting_components_ui <- function(
    visual_ids,
    ns
) {

  visual_ids <- unique(
    as.character(
      visual_ids
    )
  )

  supporting_ids <- visual_ids[
    visual_ids %in%
      c(
        "site_values",
        "key_insights"
      )
  ]

  if (length(supporting_ids) == 0L) {
    return(NULL)
  }

  components <- lapply(
    supporting_ids,
    function(visual_id) {

      switch(
        visual_id,

        site_values =
          executive_site_values_card_ui(
            ns
          ),

        key_insights =
          executive_key_insights_card_ui(
            ns
          ),

        NULL
      )
    }
  )

  components <- Filter(
    Negate(is.null),
    components
  )

  layout_class <- if (
    length(components) == 1L
  ) {
    "executive-dashboard-bottom-grid single-component"
  } else {
    "executive-dashboard-bottom-grid"
  }

  shiny::div(
    class = layout_class,
    tagList(
      components
    )
  )
}


# -------------------------------------------------------------------------
# Executive Sites: fixed presentation cards (no visual-selection controls)
# -------------------------------------------------------------------------
# All four plots show every reporting unit for the selected year(s). The
# more detailed activity, audience and facility breakdowns remain on the
# existing Site Detail and Site Comparison pages.

executive_site_dashboard_ui <- function(ns, context_label) {
  # Keep the shared Overview chart UI untouched. Only Sites receives the
  # fixed header legends and chart-level exact-value hover readouts.
  charts <- list(
    list(id = "site_finances", title = "Income, Expenses & Net Result",
         legend = c("Income", "Expenses")),
    list(id = "site_visits", title = "Visits Across Sites",
         legend = c("Direct", "Indirect")),
    list(id = "site_memberships", title = "Active Memberships",
         legend = character()),
    list(id = "site_enrolments", title = "Enrolments & Targets",
         legend = c("Reported", "Target"))
  )

  shiny::div(
    class = "executive-site-grid",
    style = paste0("display:grid;grid-template-columns:",
                   "repeat(auto-fit,minmax(min(100%,430px),1fr));",
                   "gap:10px;margin-bottom:10px;"),
    shiny::tagList(lapply(charts, function(chart) {
      legend <- if (length(chart$legend) == 0L) NULL else {
        shiny::div(
          class = "executive-site-header-legend",
          shiny::tagList(lapply(seq_along(chart$legend), function(i) {
            shiny::span(
              class = "executive-site-legend-item",
              shiny::span(class = paste0("executive-site-legend-swatch ",
                                         if (i == 1L) "primary" else "secondary")),
              chart$legend[[i]]
            )
          }))
        )
      }
      bslib::card(
        class = "executive-visual-card executive-site-visual-card",
        full_screen = TRUE,
        bslib::card_header(
          shiny::div(
            class = "executive-visual-card-header",
            shiny::div(
              class = "executive-visual-card-heading",
              shiny::div(class = "chart-header-title", chart$title),
              shiny::div(class = "chart-header-subtitle", context_label)
            ),
            legend
          )
        ),
        shiny::plotOutput(
          ns(paste0("executive_plot_", chart$id)),
          height = "305px",
          hover = shiny::hoverOpts(
            id = ns(paste0("executive_hover_", chart$id)),
            delay = 125, delayType = "debounce"
          )
        ),
        shiny::div(
          class = "executive-site-hover-readout",
          shiny::textOutput(ns(paste0("executive_hover_value_", chart$id)),
                            inline = TRUE)
        )
      )
    }))
  )
}

executive_site_rankings_card_ui <- function(ns) {
  bslib::card(
    class = "executive-dashboard-table-card executive-site-rankings-card",
    full_screen = TRUE,
    bslib::card_header(
      shiny::div(class = "chart-header-title", "Site Financial Rankings")
    ),
    shiny::div(class = "executive-site-rankings-table",
               shiny::tableOutput(ns("site_rankings_table")))
  )
}

# Supporting insights are intentionally independent from Overview insights:
# they explain site rankings and operational coverage for the chosen year.
executive_site_insights_card_ui <- function(ns) {
  bslib::card(
    class = "executive-insights-card executive-site-insights-card",
    full_screen = TRUE,
    bslib::card_header("Site Key Insights"),
    shiny::div(
      class = "executive-site-insights-content",
      shiny::uiOutput(ns("site_key_insights"))
    )
  )
}


# -------------------------------------------------------------------------
# Executive Venues: four-chart dashboard layout
# -------------------------------------------------------------------------

# Fixed chart cards deliberately use the same focus/year selection as the
# KPIs. Per-chart hover readouts do not introduce additional filters.
executive_venues_dashboard_ui <- function(ns, context_label, years) {

  years <- sort(unique(suppressWarnings(as.integer(years))))
  years <- years[!is.na(years)]

  comparison_legend <- if (length(years) == 2L) {
    list(
      labels = as.character(years),
      colours = c("secondary", "primary")
    )
  } else {
    list(
      labels = if (length(years)) as.character(years) else "Reported",
      colours = "primary"
    )
  }

  charts <- list(
    list(
      id = "venues_attendance",
      title = "Attendance by Activity",
      legend = comparison_legend
    ),
    list(
      id = "venues_facility_hire",
      title = "Facility Hire Revenue",
      legend = comparison_legend
    ),
    list(
      id = "venues_wages",
      title = "Casual Wages: Budget vs Actual",
      legend = list(
        labels = c("Budget", "Actual"),
        colours = c("primary", "secondary")
      )
    ),
    list(
      id = "venues_merchandise",
      title = "Merchandise Revenue by Section",
      legend = comparison_legend
    )
  )

  shiny::div(
    class = "executive-site-grid executive-venues-grid",

    shiny::tagList(lapply(charts, function(chart) {

      legend <- shiny::div(
        class = "executive-site-header-legend",

        shiny::tagList(lapply(
          seq_along(chart$legend$labels),
          function(i) {
            shiny::span(
              class = "executive-site-legend-item",

              shiny::span(
                class = paste(
                  "executive-site-legend-swatch",
                  chart$legend$colours[[i]]
                )
              ),

              chart$legend$labels[[i]]
            )
          }
        ))
      )

      bslib::card(
        class = paste(
          "executive-visual-card",
          "executive-site-visual-card",
          "executive-venues-visual-card"
        ),

        full_screen = TRUE,

        bslib::card_header(
          shiny::div(
            class = "executive-visual-card-header",

            shiny::div(
              class = "executive-visual-card-heading",

              shiny::div(
                class = "chart-header-title",
                chart$title
              ),

              shiny::div(
                class = "chart-header-subtitle",
                context_label
              )
            ),

            legend
          )
        ),

        shiny::plotOutput(
          ns(paste0("executive_plot_", chart$id)),
          height = "305px",

          hover = shiny::hoverOpts(
            id = ns(paste0("executive_hover_", chart$id)),
            delay = 125,
            delayType = "debounce"
          )
        ),

        shiny::div(
          class = "executive-site-hover-readout",

          shiny::textOutput(
            ns(paste0("executive_hover_value_", chart$id)),
            inline = TRUE
          )
        )
      )
    }))
  )
}


# -------------------------------------------------------------------------
# Executive Venues: Operational Summary and Key Insights
# -------------------------------------------------------------------------

executive_venues_supporting_ui <- function(ns) {

  shiny::div(
    class = "executive-site-bottom-grid executive-venues-bottom-grid",

    # Left: Operational Summary
    bslib::card(
      class = paste(
        "executive-dashboard-table-card",
        "executive-site-rankings-card"
      ),

      full_screen = TRUE,

      bslib::card_header(
        shiny::div(
          class = "chart-header-title",
          "Venues Operational Summary"
        )
      ),

      shiny::div(
        class = "executive-site-rankings-table",

        shiny::tableOutput(
          ns("venues_summary_table")
        )
      )
    ),

    # Right: Key Insights
    bslib::card(
      class = paste(
        "executive-insights-card",
        "executive-site-insights-card"
      ),

      full_screen = TRUE,

      bslib::card_header(
        "Venues Key Insights"
      ),

      shiny::div(
        class = "executive-site-insights-content",

        shiny::uiOutput(
          ns("venues_key_insights")
        )
      )
    )
  )
}



mod_executive_dashboard_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::nav_panel(
    "Executive Dashboard",
    icon = bs_icon("grid-1x2"),

    div(
      class = "p-4 executive-dashboard-page",

      # Page heading -------------------------------------------------------
      div(
        class = "page-heading executive-dashboard-heading",

        div(
          h2("Executive Dashboard"),

          p(
            paste(
              "A presentation-ready view of Monash Sport performance,",
              "bringing key results, trends and supporting detail",
              "together on one page."
            )
          )
        )
      ),

      # Executive Dashboard focus switcher -------------------------------
      div(
        class = "executive-dashboard-navigation",

        radioButtons(
          ns("dashboard_section"),
          label = NULL,
          choices = c(
            "Overview" = "finance",
            "Site" = "site",
            "Venues" = "metrics"
          ),
          selected = "finance",
          inline = TRUE
        )
      ),

      div(
        class = "pt-3",

              # Dashboard controls ----------------------------------------
              div(
                class = "filter-bar executive-dashboard-filter-bar",

                div(
                  class = "filter-section-title",
                  "Dashboard view"
                ),

                layout_columns(

                  uiOutput(
                    ns("view_mode_selector")
                  ),

                  div(
                    class = "executive-dashboard-year-controls",

                    uiOutput(
                      ns("year_selector")
                    ),

                    conditionalPanel(
                      condition =
                        "input.view_mode == 'Comparison'",

                      uiOutput(
                        ns("comparison_year_selector")
                      ),

                      ns = ns
                    )
                  ),

                  # The Sites presentation compares all reporting units.
                  # Individual-site drill-downs remain on Site Detail.

                  div(
                    class = "executive-dashboard-actions",

                    # Fixed Overview/Sites charts need no visual picker.
                    # The legacy picker remains for the later Venues redesign.
                    conditionalPanel(
                      condition = "input.dashboard_section == 'metrics'",
                      actionButton(
                        ns("choose_visuals"),
                        "Choose Visuals",
                        icon = bs_icon("sliders")
                      ),
                      ns = ns
                    ),

                    actionButton(
                      ns("presentation_mode"),
                      "Presentation View",
                      icon = bs_icon("arrows-fullscreen"),
                      class = "btn-primary",
                      title = "Press Esc to exit Presentation View",
                      onclick = "
                        const page = this.closest('.executive-dashboard-page');

                        if (!page) {
                          return;
                        }

                        if (!document.fullscreenElement) {
                          if (page.requestFullscreen) {
                            page.requestFullscreen();
                          }
                        } else {
                          if (document.exitFullscreen) {
                            document.exitFullscreen();
                          }
                        }
                      "
                    )
                  ),

                  col_widths = c(
                    2,
                    5,
                    5
                  )
                )
              ),

              # KPI strip -------------------------------------------------

              uiOutput(
                ns("executive_kpi_grid")
              ),
                              
              # Responsive visual board -----------------------------------
              uiOutput(
                ns("finance_visual_grid")
              ),

              # Optional table / insight components -----------------------
              uiOutput(
                ns("finance_supporting_components")
              )
            )
          )
        )
      }

# The visible Venues tab still uses the internal focus key "Metrics" for
# backward compatibility with saved context and existing test helpers.
mod_executive_dashboard_server <- function(
    id,
    data = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns


      # -------------------------------------------------------------------
      # Dashboard data
      # -------------------------------------------------------------------

      dashboard_data_value <- reactive({

        if (is.null(data)) {
          return(NULL)
        }

        if (shiny::is.reactive(data)) {
          return(
            data()
          )
        }

        data
      })


      # -------------------------------------------------------------------
      # Active Executive Dashboard section
      # -------------------------------------------------------------------

      active_category <- reactive({

        if (
          identical(
            input$dashboard_section,
            "metrics"
          )
        ) {
          return(
            "Metrics"
          )
        }

        "Finance"
      })

      active_focus <- reactive({

        section <- input$dashboard_section

        if (
          is.null(section) ||
            length(section) == 0L ||
            is.na(section[[1L]])
        ) {
          return("Overview")
        }

        switch(
          as.character(section[[1L]]),

          site = "Site",

          metrics = "Metrics",

          "Overview"
        )
      })

      available_executive_years <- reactive({

        executive_available_years(
          dashboard_data =
            dashboard_data_value(),
          focus =
            active_focus()
        )
      })

      available_executive_sites <- reactive({

        monthly_reports <-
          executive_source_data(
            dashboard_data =
              dashboard_data_value(),
            source_group =
              "monthly"
          )

        executive_site_choices(
          monthly_reports
        )
      })

      # -------------------------------------------------------------------
      # Independent dashboard context by focus
      # -------------------------------------------------------------------

      dashboard_contexts <- reactiveValues(

        Overview = list(
          view_mode = "Year Review",
          year = "",
          comparison_year = NULL,
          site = "All Monash Sport"
        ),

        Site = list(
          view_mode = "Year Review",
          year = "",
          comparison_year = NULL,
          site = "All Monash Sport"
        ),

        Metrics = list(
          view_mode = "Year Review",
          year = "",
          comparison_year = NULL,
          site = "All Venues"
        )
      )

      previous_focus <- reactiveVal(
        "Overview"
      )

      save_dashboard_context <- function(
          focus
      ) {

        if (
          is.null(focus) ||
            length(focus) == 0L ||
            !focus %in%
              c(
                "Overview",
                "Site",
                "Metrics"
              )
        ) {
          return(
            invisible(NULL)
          )
        }


        focus_years <-
          executive_available_years(
            dashboard_data =
              dashboard_data_value(),
            focus =
              focus
          )


        selected_year <-
          executive_reconcile_year_selection(
            year =
              input$year,
            years =
              focus_years
          )


        selected_view_mode <-
          executive_reconcile_view_mode(
            view_mode =
              input$view_mode,
            years =
              focus_years
          )


        comparison_year <-
          executive_reconcile_comparison_year(
            year =
              selected_year,
            comparison_year =
              input$comparison_year,
            years =
              focus_years
          )


        selected_site <-
          if (
            identical(
              focus,
              "Overview"
            )
          ) {

            "All Monash Sport"

          } else if (identical(focus, "Metrics")) {
            "All Venues"
          } else {
            # Site is a fixed all-sites executive comparison.
            "All Monash Sport"
          }


        dashboard_contexts[[focus]] <-
          list(
            view_mode =
              selected_view_mode,
            year =
              selected_year,
            comparison_year =
              comparison_year,
            site =
              selected_site
          )


        invisible(NULL)
      }
      
      # -------------------------------------------------------------------
      # Dynamic Executive year selector
      # -------------------------------------------------------------------

      output$view_mode_selector <- renderUI({

        years <-
          available_executive_years()

        focus <-
          active_focus()

        context <- if (
          !is.null(focus) &&
            focus %in%
              c(
                "Overview",
                "Site",
                "Metrics"
              )
        ) {

          dashboard_contexts[[focus]]

        } else {

          NULL
        }

        context_view <- if (
          !is.null(context) &&
            !is.null(context$view_mode) &&
            length(context$view_mode) > 0L
        ) {
          as.character(
            context$view_mode[[1L]]
          )
        } else {
          "Year Review"
        }

        selected_view <-
          executive_reconcile_view_mode(
            view_mode =
              context_view,
            years =
              years
          )

        view_choices <- if (
          length(years) >= 2L
        ) {
          c(
            "Year Review",
            "Comparison"
          )
        } else {
          "Year Review"
        }

        selectInput(
          ns("view_mode"),
          "View",
          choices =
            view_choices,
          selected =
            selected_view
        )
      })


      output$year_selector <- renderUI({

        years <-
          available_executive_years()


        if (length(years) == 0L) {

          return(
            selectInput(
              ns("year"),
              "Year",
              choices = c(
                "No data loaded" = ""
              ),
              selected = ""
            )
          )
        }


        focus <-
          active_focus()


        context <- if (
          !is.null(focus) &&
            focus %in%
              c(
                "Overview",
                "Site",
                "Metrics"
              )
        ) {

          dashboard_contexts[[focus]]

        } else {

          NULL
        }

        context_year <- if (
          !is.null(context) &&
            !is.null(context$year) &&
            length(context$year) > 0L
        ) {
          as.character(
            context$year[[1L]]
          )
        } else {
          ""
        }

        selected_year <-
          executive_reconcile_year_selection(
            year =
              context_year,
            years =
              years
          )

        selectInput(
          ns("year"),
          "Year",
          choices =
            as.character(years),
          selected =
            selected_year
        )
      })
      
      output$comparison_year_selector <- renderUI({

        years <-
          available_executive_years()


        selected_year <- input$year


        choices <-
          executive_comparison_year_choices(
            year =
              selected_year,
            years =
              years
          )


        if (length(choices) == 0L) {

          return(
            selectInput(
              ns("comparison_year"),
              "Compare To",
              choices = c(
                "No comparison available" = ""
              ),
              selected = ""
            )
          )
        }

        current_value <- isolate(
          input$comparison_year
        )


        selected_value <-
          executive_reconcile_comparison_year(
            year =
              selected_year,
            comparison_year =
              current_value,
            years =
              years
          )


        selectInput(
          ns("comparison_year"),
          "Compare To",
          choices =
            choices,
          selected =
            selected_value
        )
      })

      output$site_selector <- renderUI({

        choices <-
          available_executive_sites()

        context <-
          dashboard_contexts$Site

        current_site <- isolate(
          input$site
        )

        context_site <- if (
          !is.null(context) &&
            !is.null(context$site) &&
            length(context$site) > 0L
        ) {
          context$site
        } else {
          "All Monash Sport"
        }

        candidate_site <- if (
          !is.null(current_site) &&
            length(current_site) > 0L &&
            !is.na(current_site[[1L]]) &&
            nzchar(
              as.character(
                current_site[[1L]]
              )
            )
        ) {
          current_site
        } else {
          context_site
        }

        selected_site <-
          executive_reconcile_site_selection(
            site =
              candidate_site,
            choices =
              choices
          )

        selectInput(
          ns("site"),
          "Site",
          choices =
            choices,
          selected =
            selected_site
        )
      })

      # -------------------------------------------------------------------
      # Selected visuals
      # -------------------------------------------------------------------
      # Finance and Metrics keep their own selections so future operational
      # visuals do not interfere with the Finance presentation layout.

      selected_visuals <- reactiveValues(

        Overview =
          executive_default_visual_ids(
            category = "Finance",
            focus = "Overview"
          ),

        Site =
          executive_default_visual_ids(
            category = "Finance",
            focus = "Site"
          ),

        Metrics =
          executive_default_visual_ids(
            category = "Metrics",
            focus = "Metrics"
          )
        )
      
      observeEvent(
        input$dashboard_section,
        {

          new_focus <-
            active_focus()

          old_focus <-
            previous_focus()

          if (
            !identical(
              old_focus,
              new_focus
            )
          ) {

            save_dashboard_context(
              old_focus
            )
          }

          context <-
            dashboard_contexts[[new_focus]]

          focus_years <-
            executive_available_years(
              dashboard_data =
                dashboard_data_value(),
              focus =
                new_focus
            )


          selected_year <-
            executive_reconcile_year_selection(
              year =
                context$year,
              years =
                focus_years
            )


          selected_view_mode <-
            executive_reconcile_view_mode(
              view_mode =
                context$view_mode,
              years =
                focus_years
            )


          comparison_choices <-
            executive_comparison_year_choices(
              year =
                selected_year,
              years =
                focus_years
            )


          comparison_year <-
            executive_reconcile_comparison_year(
              year =
                selected_year,
              comparison_year =
                context$comparison_year,
              years =
                focus_years
            )


          comparison_choices_ui <- if (
            length(comparison_choices) == 0L
          ) {

            c(
              "No comparison available" = ""
            )

          } else {

            comparison_choices
          }

          freezeReactiveValue(
            input,
            "view_mode"
          )

          freezeReactiveValue(
            input,
            "comparison_year"
          )

          freezeReactiveValue(
            input,
            "site"
          )

          updateSelectInput(
            session,
            "view_mode",
            selected =
              selected_view_mode
          )

          updateSelectInput(
            session,
            "comparison_year",
            choices =
              comparison_choices_ui,
            selected =
              comparison_year
          )

          updateSelectInput(
            session,
            "site",
            selected =
              context$site
          )


          previous_focus(
            new_focus
          )
        },
        ignoreInit = TRUE
      )


      # -------------------------------------------------------------------
      # Registry for the currently active category
      # -------------------------------------------------------------------

      picker_registry <- reactive({

        registry <-
          executive_registry_availability_table(
            dashboard_data =
              dashboard_data_value(),
            category =
              active_category(),
            context =
              executive_context()
          )

        focus <- active_focus()

        registry[
          registry$focus == focus,
          ,
          drop = FALSE
        ]
      })

      # -------------------------------------------------------------------
      # Executive Dashboard context
      # -------------------------------------------------------------------

      executive_context <- reactive({

        effective_view_mode <-
          executive_reconcile_view_mode(
            view_mode =
              input$view_mode,
            years =
              available_executive_years()
          )

        comparison_year <- NULL

        if (
          identical(
            effective_view_mode,
            "Comparison"
          )
        ) {

          comparison_year <-
            input$comparison_year
        }

        focus <-
          active_focus()

        effective_site <-
          if (
            identical(
              focus,
              "Overview"
            )
          ) {

            "All Monash Sport"

          } else if (identical(focus, "Metrics")) {
            "All Venues"
          } else {
            # No individual Site selector on the executive presentation.
            "All Monash Sport"
          }

        list(
          view_mode =
            effective_view_mode,

          year =
            input$year,

          comparison_year =
            comparison_year,

          site =
            effective_site
        )
      })


      # ---------------------------------------------------------------
      # Executive Site presentation data
      # ---------------------------------------------------------------
      # Finance uses the existing validated Annual Site Summary. Operational
      # metrics are grouped by report year + reporting unit + campus, then
      # calculated using the same helpers as Site Detail. We never sum YTD
      # and monthly values or add successive membership snapshots together.

      executive_site_financial_rows <- reactive({
        rows <- calculate_site_annual_finance(executive_monthly_reports())
        years <- executive_selected_years()
        rows[rows$report_year %in% years, , drop = FALSE]
      })

      # Reusable operational data preparation for any requested year.
      # Site charts still use only the selected reporting year(s).
      executive_site_operational_for_years <- function(years) {
        reports <- executive_detailed_site_reports()
        years <- suppressWarnings(as.integer(years))
        years <- years[!is.na(years)]
        empty <- data.frame(
          report_year = integer(), site = character(),
          direct = double(), indirect = double(),
          active_members = double(), measure = character(),
          actual = double(), target = double(),
          report_unit = character(), campus = character(),
          stringsAsFactors = FALSE
        )
        if (is.null(reports) || !is.list(reports) ||
            length(years) == 0L) return(empty)

        table_names <- c("visits", "memberships", "enrollments")
        datasets <- lapply(table_names, function(name) {
          data <- reports[[name]]
          if (is.null(data) || !is.data.frame(data) ||
              !all(c("report_year", "report_unit", "campus") %in% names(data))) {
            return(NULL)
          }
          data[!is.na(data$report_year) & data$report_year %in% years &
                 !is.na(data$report_unit) & nzchar(trimws(data$report_unit)),
               , drop = FALSE]
        })
        names(datasets) <- table_names
        keys <- lapply(datasets, function(data) {
          if (is.null(data) || nrow(data) == 0L) return(NULL)
          unique(data[c("report_year", "report_unit", "campus")])
        })
        keys <- Filter(Negate(is.null), keys)
        if (length(keys) == 0L) return(empty)
        keys <- unique(do.call(rbind, keys))
        keys$campus <- as.character(keys$campus)
        keys$campus[is.na(keys$campus) | !nzchar(keys$campus)] <- "Unspecified"

        catalogue <- reporting_detailed_site_catalogue(reports)
        catalogue_keys <- if (nrow(catalogue) > 0L) {
          reporting_site_identity_key(catalogue$report_unit, catalogue$campus)
        } else character()
        known <- if (exists("KNOWN_SITE_CATALOGUE", inherits = TRUE)) {
          get("KNOWN_SITE_CATALOGUE", inherits = TRUE)
        } else NULL

        result <- lapply(seq_len(nrow(keys)), function(i) {
          year <- as.integer(keys$report_year[[i]])
          unit <- trimws(as.character(keys$report_unit[[i]]))
          campus <- keys$campus[[i]]
          key <- reporting_site_identity_key(unit, campus)
          catalogue_index <- match(key, catalogue_keys)
          site_name <- if (!is.na(catalogue_index)) {
            as.character(catalogue$display_name[[catalogue_index]])
          } else if (!is.null(known)) {
            known_index <- match(key, reporting_site_identity_key(
              known$report_unit, known$campus))
            if (!is.na(known_index)) as.character(known$display_name[[known_index]])
            else paste(unit, campus, sep = " · ")
          } else paste(unit, campus, sep = " · ")

          subset_for <- function(name) {
            data <- datasets[[name]]
            if (is.null(data) || nrow(data) == 0L) return(NULL)
            mask <- !is.na(data$report_year) & data$report_year == year &
              reporting_site_identity_key(data$report_unit, data$campus) == key
            data[!is.na(mask) & mask, , drop = FALSE]
          }
          visits <- subset_for("visits")
          memberships <- subset_for("memberships")
          enrollments <- subset_for("enrollments")

          direct <- site_detail_visit_value(visits, "Direct", "Total")
          indirect <- site_detail_visit_value(visits, "Indirect", "Total")
          active <- site_detail_latest_membership_value(memberships, "Total Active")

          # Preserve distinct enrolment definitions, especially Aquatics and
          # Participation Programs, instead of combining unlike measures.
          enrolment_measure <- "Total Active"
          if (!is.null(enrollments) && nrow(enrollments) > 0L &&
              "metric" %in% names(enrollments)) {
            actual_choices <- c("Total Active", "Total Enrolments", "Enrolments")
            available <- unique(as.character(enrollments$metric))
            matched <- actual_choices[actual_choices %in% available]
            if (length(matched) > 0L) enrolment_measure <- matched[[1L]]
          }
          enrolment <- site_detail_latest_enrolment_metric(enrollments, enrolment_measure)
          target <- site_detail_latest_enrolment_metric(enrollments, "Target")
          data.frame(
            report_year = year, site = site_name, direct = direct,
            indirect = indirect, active_members = active,
            measure = enrolment_measure, actual = enrolment$value,
            target = target$value, report_unit = unit, campus = campus,
            stringsAsFactors = FALSE
          )
        })
        rows <- do.call(rbind, result)
        rownames(rows) <- NULL
        rows
      }

      # Preserve the existing selected-year behaviour for charts.
      executive_site_operational_rows <- reactive({
        executive_site_operational_for_years(
          executive_selected_years()
        )
      })

      executive_site_visits_rows <- reactive({
        x <- executive_site_operational_rows()
        x[c("report_year", "site", "direct", "indirect")]
      })
      executive_site_membership_rows <- reactive({
        x <- executive_site_operational_rows()
        x[c("report_year", "site", "active_members")]
      })
      executive_site_enrolment_rows <- reactive({
        x <- executive_site_operational_rows()
        x <- x[is.finite(x$actual) | is.finite(x$target), , drop = FALSE]
        x[c("report_year", "site", "measure", "actual", "target")]
      })

      executive_site_snapshot_kpis <- function(year) {
        # Retrieve the requested year's operational records,
        # even when it isn't selected in the chart filters.
        x <- executive_site_operational_for_years(year)
        finance <- executive_site_financial_rows()
        finance <- executive_site_financial_rows()
        if (length(year) == 0L || is.na(year)) {
          return(list(visits = NA_real_, visit_sites = 0L, visit_names = character(),
                      members = NA_real_, member_sites = 0L, member_names = character(),
                      programmes = NA_real_, programme_sites = 0L, programme_names = character(),
                      sites = 0L))
        }
        x <- x[x$report_year == year, , drop = FALSE]
        finance <- finance[finance$report_year == year, , drop = FALSE]
        complete_visits <- is.finite(x$direct) & is.finite(x$indirect)
        member_rows <- is.finite(x$active_members)
        programme_rows <- grepl("participation program", x$report_unit,
                                ignore.case = TRUE) & is.finite(x$actual)
        reported_sites <- unique(c(as.character(finance$site), as.character(x$site)))
        reported_sites <- reported_sites[!is.na(reported_sites) & nzchar(reported_sites)]
        list(
          visits = if (any(complete_visits)) sum(x$direct[complete_visits] +
                         x$indirect[complete_visits]) else NA_real_,
          visit_sites = sum(complete_visits),
          visit_names = sort(unique(x$site[complete_visits])),
          members = if (any(member_rows)) sum(x$active_members[member_rows]) else NA_real_,
          member_sites = sum(member_rows),
          member_names = sort(unique(x$site[member_rows])),
          programmes = if (any(programme_rows)) sum(x$actual[programme_rows]) else NA_real_,
          programme_sites = sum(programme_rows),
          programme_names = sort(unique(x$site[programme_rows])),
          sites = length(reported_sites)
        )
      }

      output$executive_plot_site_finances <- renderPlot({
        x <- executive_site_financial_rows()
        validate(need(nrow(x) > 0L &&
                      any(is.finite(x$income_actual) & is.finite(x$expenses_actual)),
                      "No site financial totals are available for the selected year(s)."))
        plot_executive_site_finances(x)
      })

      output$executive_plot_site_visits <- renderPlot({
        x <- executive_site_visits_rows()
        validate(need(nrow(x) > 0L &&
                      any(is.finite(x$direct) | is.finite(x$indirect)),
                      "No reported Site visit totals are available for the selected year(s)."))
        plot_executive_site_visits(x)
      })

      output$executive_plot_site_memberships <- renderPlot({
        x <- executive_site_membership_rows()
        validate(need(nrow(x) > 0L && any(is.finite(x$active_members)),
                      "No active membership snapshots are reported for the selected year(s)."))
        plot_executive_site_memberships(x)
      })

      output$executive_plot_site_enrolments <- renderPlot({
        x <- executive_site_enrolment_rows()
        validate(need(nrow(x) > 0L &&
                      any(is.finite(x$actual) | is.finite(x$target)),
                      "No comparable enrolment measures are available for the selected year(s)."))
        plot_executive_site_enrolments(x)
      })

      # ---------------------------------------------------------------
      # Executive Sites: per-row exact-value hover readouts
      # ---------------------------------------------------------------
      # Base R horizontal bar charts place the first row at y = n and the
      # last at y = 1. Match chart sorting before converting hovered y.
      executive_site_hover_rows <- function(data, chart) {
        required <- switch(
          chart,
          site_finances = c("income_actual", "expenses_actual", "net_actual"),
          site_visits = c("direct", "indirect"),
          site_memberships = "active_members",
          site_enrolments = c("measure", "actual", "target")
        )
        key <- if (identical(chart, "site_enrolments")) {
          c("report_year", "site", "measure")
        } else {
          c("report_year", "site")
        }
        rows <- executive_site_plot_rows(data, required, "Site hover", key = key)
        single_year <- length(unique(rows$report_year)) == 1L
        if (identical(chart, "site_finances") && single_year) {
          rows <- rows[order(-rows$income_actual, rows$site,
                             na.last = TRUE), , drop = FALSE]
        }
        if (identical(chart, "site_visits") && single_year) {
          totals <- ifelse(is.finite(rows$direct) & is.finite(rows$indirect),
                           rows$direct + rows$indirect, NA_real_)
          rows <- rows[order(-totals, rows$site, na.last = TRUE), , drop = FALSE]
        }
        if (identical(chart, "site_memberships")) {
          rows <- rows[is.finite(rows$active_members), , drop = FALSE]
          if (single_year) {
            rows <- rows[order(-rows$active_members, rows$site), , drop = FALSE]
          }
        }
        if (identical(chart, "site_enrolments") && single_year) {
          rows <- rows[order(rows$site, rows$measure), , drop = FALSE]
        }
        rows
      }

      executive_site_hover_text <- function(data, hover, chart) {
        instruction <- "Hover over a site bar to inspect exact values."
        if (is.null(hover) || is.null(hover$y) ||
            !is.finite(hover$y)) return(instruction)
        rows <- tryCatch(executive_site_hover_rows(data, chart),
                         error = function(e) NULL)
        if (is.null(rows) || nrow(rows) == 0L) return(instruction)
        y <- round(hover$y)
        if (abs(hover$y - y) > 0.46 || y < 1L || y > nrow(rows)) {
          return(instruction)
        }
        record <- rows[nrow(rows) - as.integer(y) + 1L, , drop = FALSE]
        value_count <- function(field) {
          value <- suppressWarnings(as.numeric(record[[field]][[1L]]))
          if (!is.finite(value)) return("Not reported")
          format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE)
        }
        value_currency <- function(field) {
          format_executive_hover_value(record[[field]])
        }
        label <- paste0(record$site[[1L]], " (", record$report_year[[1L]], ")")
        details <- switch(chart,
          site_finances = paste0(
            "Income ", value_currency("income_actual"),
            " | Expenses ", value_currency("expenses_actual"),
            " | Net Result ", value_currency("net_actual")
          ),
          site_visits = paste0(
            "Direct ", value_count("direct"),
            " | Indirect ", value_count("indirect"),
            " | Total ",
            if (is.finite(record$direct[[1L]]) &&
                is.finite(record$indirect[[1L]])) {
              executive_site_format_count(record$direct[[1L]] +
                                          record$indirect[[1L]])
            } else "Not reported"
          ),
          site_memberships = paste0("Active Members ",
                                    value_count("active_members")),
          site_enrolments = paste0(
            record$measure[[1L]], ": ", value_count("actual"),
            " | Target ", value_count("target")
          )
        )
        paste(label, details, sep = " | ")
      }

      output$executive_hover_value_site_finances <- renderText({
        executive_site_hover_text(executive_site_financial_rows(),
                                  input$executive_hover_site_finances,
                                  "site_finances")
      })
      output$executive_hover_value_site_visits <- renderText({
        executive_site_hover_text(executive_site_visits_rows(),
                                  input$executive_hover_site_visits,
                                  "site_visits")
      })
      output$executive_hover_value_site_memberships <- renderText({
        executive_site_hover_text(executive_site_membership_rows(),
                                  input$executive_hover_site_memberships,
                                  "site_memberships")
      })
      output$executive_hover_value_site_enrolments <- renderText({
        executive_site_hover_text(executive_site_enrolment_rows(),
                                  input$executive_hover_site_enrolments,
                                  "site_enrolments")
      })

      output$site_rankings_table <- renderTable({
        x <- executive_site_financial_rows()
        validate(need(nrow(x) > 0L, "No site financial rankings are available."))
        x <- x[order(x$report_year, -x$income_actual, x$site,
                     na.last = TRUE), , drop = FALSE]
        rank_metric <- function(values, descending = TRUE) {
          result <- rep(NA_integer_, length(values))
          ok <- is.finite(values)
          if (any(ok)) result[ok] <- as.integer(rank(if (descending) -values[ok]
                                                          else values[ok],
                                                      ties.method = "min"))
          result
        }
        rank_for_year <- function(values, years, descending = TRUE) {
          result <- rep(NA_integer_, length(values))
          for (year in unique(years)) {
            idx <- which(years == year)
            result[idx] <- rank_metric(values[idx], descending)
          }
          result
        }
        money <- function(values) {
          vapply(values, function(value) {
            if (!is.finite(value)) return("—")
            paste0(if (value < 0) "-" else "", "$",
                   formatC(abs(value), format = "f", digits = 2, big.mark = ","))
          }, character(1))
        }
        show_year <- length(unique(x$report_year)) > 1L
        table <- data.frame(
          Site = x$site,
          `Income Rank` = rank_for_year(x$income_actual, x$report_year),
          Income = money(x$income_actual),
          `Expense Rank` = rank_for_year(x$expenses_actual, x$report_year),
          Expenses = money(x$expenses_actual),
          `Net Rank` = rank_for_year(x$net_actual, x$report_year),
          `Net Result` = money(x$net_actual),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        if (show_year) table <- cbind(Year = x$report_year, table)
        table
      }, striped = TRUE, hover = TRUE, bordered = FALSE,
      spacing = "s", rownames = FALSE)

      # -------------------------------------------------------------------
      # Executive Finance data
      # -------------------------------------------------------------------

      executive_monthly_reports <- reactive({

        executive_source_data(
          dashboard_data =
            dashboard_data_value(),
          source_group =
            "monthly"
        )
      })

      executive_detailed_site_reports <- reactive({

        executive_source_data(
          dashboard_data =
            dashboard_data_value(),
          source_group =
            "detailed"
        )
      })

      executive_scoped_monthly_reports <- reactive({

        context <- executive_context()

        executive_filter_monthly_reports_by_site(
          monthly_reports =
            executive_monthly_reports(),
          site =
            context$site
        )
      })


      executive_selected_years <- reactive({

        executive_context_years(
          executive_context()
        )
      })


      executive_monthly_finance <- reactive({

        x <-
          calculate_overview_monthly_finance(
            executive_scoped_monthly_reports()
          )

        years <- executive_selected_years()

        if (length(years) == 0L) {
          return(x[0, , drop = FALSE])
        }

        x[
          x$report_year %in% years,
          ,
          drop = FALSE
        ]
      })


      executive_annual_finance <- reactive({

        x <-
          calculate_overview_annual_finance(
            executive_scoped_monthly_reports()
          )

        years <-
          executive_context_trend_years(
            context = executive_context(),
            available_years = x$report_year
          )

        if (length(years) == 0L) {
          return(x[0, , drop = FALSE])
        }

        x[
          x$report_year %in% years,
          ,
          drop = FALSE
        ]
      })

      executive_kpi_finance <- reactive({

        x <- executive_annual_finance()
        context <- executive_context()

        selected_year <-
          suppressWarnings(
            as.integer(
              context$year
            )
          )

        if (
          is.null(x) ||
            !is.data.frame(x) ||
            nrow(x) == 0L ||
            is.na(selected_year)
        ) {
          return(NULL)
        }

        x <-
          x[
            x$report_year == selected_year,
            ,
            drop = FALSE
          ]

        if (nrow(x) == 0L) {
          return(NULL)
        }

        x[1L, , drop = FALSE]
      })

      executive_kpi_comparison_finance <- reactive({

        context <- executive_context()

        if (
          !identical(
            context$view_mode,
            "Comparison"
          ) ||
            is.null(
              context$comparison_year
            )
        ) {
          return(NULL)
        }

        x <- executive_annual_finance()

        comparison_year <-
          suppressWarnings(
            as.integer(
              context$comparison_year
            )
          )

        if (
          is.null(x) ||
            !is.data.frame(x) ||
            nrow(x) == 0L ||
            is.na(comparison_year)
        ) {
          return(NULL)
        }

        x <-
          x[
            x$report_year == comparison_year,
            ,
            drop = FALSE
          ]

        if (nrow(x) == 0L) {
          return(NULL)
        }

        x[1L, , drop = FALSE]
      })

      output$kpi_total_income <- renderText({

        x <- executive_kpi_finance()

        if (
          is.null(x) ||
            !is.finite(x$income_actual[[1L]])
        ) {
          return("—")
        }

        format_compact_currency(
          x$income_actual[[1L]]
        )
      })


      output$kpi_total_expenses <- renderText({

        x <- executive_kpi_finance()

        if (
          is.null(x) ||
            !is.finite(x$expenses_actual[[1L]])
        ) {
          return("—")
        }

        format_compact_currency(
          x$expenses_actual[[1L]]
        )
      })


      output$kpi_net_result <- renderText({

        x <- executive_kpi_finance()

        if (
          is.null(x) ||
            !is.finite(x$net_actual[[1L]])
        ) {
          return("—")
        }

        format_compact_currency(
          x$net_actual[[1L]]
        )
      })


      output$kpi_budget_variance <- renderText({

        x <- executive_kpi_finance()

        if (
          is.null(x) ||
            !is.finite(x$budget_variance[[1L]])
        ) {
          return("—")
        }

        format_compact_currency(
          x$budget_variance[[1L]]
        )
      })

      # -------------------------------------------------------------------
      # Financial Summary
      # -------------------------------------------------------------------
      # Reuse the annual finance calculations used by the KPI cards.
      # Missing values are displayed as unavailable, not as zero.

      output$financial_summary_table <- renderTable({

        context <- executive_context()

        current_finance <- executive_kpi_finance()
        comparison_finance <- executive_kpi_comparison_finance()

        fields <- c(
          "income_actual",
          "expenses_actual",
          "net_actual",
          "budget_variance"
        )

        metric_labels <- c(
          "Total Income",
          "Total Expenses",
          "Net Result",
          "Budget Variance"
        )

        get_values <- function(finance) {

          if (
            is.null(finance) ||
            !is.data.frame(finance) ||
            nrow(finance) == 0L
          ) {
            return(rep(NA_real_, length(fields)))
          }

          vapply(
            fields,
            function(field) {
              if (!field %in% names(finance)) {
                return(NA_real_)
              }

              value <- finance[[field]][[1L]]

              if (is.finite(value)) {
                as.numeric(value)
              } else {
                NA_real_
              }
            },
            numeric(1)
          )
        }

        
        format_values <- function(values, show_plus = FALSE) {
          vapply(
            values,
            function(value) {
              if (!is.finite(value)) {
                return("—")
              }

              rounded <- round(as.numeric(value), 2)

              sign <- if (rounded < 0) {
                "-"
              } else if (show_plus && rounded > 0) {
                "+"
              } else {
                ""
              }

              paste0(
                sign,
                "$",
                formatC(
                  abs(rounded),
                  format = "f",
                  digits = 2,
                  big.mark = ","
                )
              )
            },
            character(1)
          )
        }


        current_values <- get_values(current_finance)

        result <- data.frame(
          Metric = metric_labels,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        selected_year <- as.character(context$year)

        if (
          identical(context$view_mode, "Comparison") &&
          !is.null(context$comparison_year)
        ) {

          comparison_year <- as.character(context$comparison_year)

          previous_values <- get_values(comparison_finance)

          differences <- ifelse(
            is.finite(current_values) & is.finite(previous_values),
            current_values - previous_values,
            NA_real_
          )

          result[[comparison_year]] <- format_values(previous_values)
          result[[selected_year]] <- format_values(current_values)
          result[["Difference"]] <- format_values(
            differences,
            show_plus = TRUE
          )

        } else {

          result[[selected_year]] <- format_values(current_values)

        }

        result

      },
      striped = TRUE,
      hover = TRUE,
      bordered = FALSE,
      spacing = "s",
      rownames = FALSE)

      
      # -------------------------------------------------------------------
      # Executive Venues: reporting-year data and operational KPIs
      # -------------------------------------------------------------------
      # Keep the internal focus value "Metrics" for compatibility with the
      # existing dashboard contexts. The visible tab is now named Venues.
      # Reuse Venues Operations calculations so reported totals are not added
      # again to detail rows or monthly values.

      executive_venues_reports <- reactive({
        executive_source_data(
          dashboard_data = dashboard_data_value(),
          source_group = "venues"
        )
      })

      executive_venues_kpis_for_year <- function(year) {
        empty <- list(
          total_attendance = NA_real_,
          hire_revenue = NA_real_,
          actual_wages = NA_real_,
          merchandise_sales = NA_real_,
          hire_transactions = NA_real_,
          paid_hours = NA_real_,
          wage_variance = NA_real_
        )

        year <- suppressWarnings(as.integer(year))
        reports <- executive_venues_reports()

        if (length(year) != 1L || is.na(year) ||
            is.null(reports) || !is.list(reports)) {
          return(empty)
        }

        # Missing datasets must remain missing, rather than becoming zero.
        rows_for <- function(dataset) {
          rows <- reports[[dataset]]

          if (is.null(rows) || !is.data.frame(rows) ||
              nrow(rows) == 0L || !"report_year" %in% names(rows)) {
            return(NULL)
          }

          venues_operations_filter_year(rows, year = year)
        }

        attendance <- venues_operations_attendance_kpis(
          rows_for("attendance")
        )

        hire <- venues_operations_facility_hire_kpis(
          rows_for("facility_hire")
        )

        workforce <- venues_operations_workforce_kpis(
          rows_for("casual_hours"), rows_for("casual_wages")
        )

        merchandise <- venues_operations_merchandise_kpis(
          rows_for("merchandise")
        )

        list(
          total_attendance = attendance$total_attendance,
          hire_revenue = hire$revenue,
          actual_wages = workforce$actual_wages,
          merchandise_sales = merchandise$revenue,
          hire_transactions = hire$transactions,
          paid_hours = workforce$paid_hours,
          wage_variance = workforce$wage_variance
        )
      }

      # ---------------------------------------------------------------
      # Executive Venues: shared chart data helpers
      # ---------------------------------------------------------------

      executive_venues_rows_for_year <- function(dataset, year) {

        reports <- executive_venues_reports()

        if (is.null(reports) || !is.list(reports)) {
          return(NULL)
        }

        rows <- reports[[dataset]]

        if (is.null(rows) ||
            !is.data.frame(rows) ||
            nrow(rows) == 0L ||
            !"report_year" %in% names(rows)) {
          return(NULL)
        }

        venues_operations_filter_year(
          rows,
          year = year
        )
      }

      executive_venues_bind_years <- function(make_rows, empty) {

        years <- executive_selected_years()

        pieces <- lapply(years, make_rows)

        pieces <- Filter(
          function(x) is.data.frame(x) && nrow(x) > 0L,
          pieces
        )

        if (length(pieces) == 0L) {
          return(empty)
        }

        result <- do.call(rbind, pieces)
        rownames(result) <- NULL

        result
      }

      # ---------------------------------------------------------------
      # Chart 1: Attendance by Activity
      # ---------------------------------------------------------------

      executive_venues_attendance_rows <- reactive({

        empty <- data.frame(
          report_year = integer(),
          activity = character(),
          value = double(),
          stringsAsFactors = FALSE
        )

        executive_venues_bind_years(
          function(year) {

            rows <- venues_operations_attendance_section_totals(
              executive_venues_rows_for_year(
                "attendance",
                year
              )
            )

            if (nrow(rows) == 0L) {
              return(NULL)
            }

            # Reuse validated section totals.
            # Combine activity areas without adding detail rows again.
            totals <- stats::aggregate(
              value ~ activity,
              data = rows,
              FUN = sum
            )

            data.frame(
              report_year = as.integer(year),
              activity = as.character(totals$activity),
              value = as.numeric(totals$value),
              stringsAsFactors = FALSE
            )
          },
          empty
        )
      })


      # ---------------------------------------------------------------
      # Chart 2: Facility Hire Revenue
      # ---------------------------------------------------------------

      executive_venues_hire_rows <- reactive({

        empty <- data.frame(
          report_year = integer(),
          facility = character(),
          revenue = double(),
          transactions = double(),
          stringsAsFactors = FALSE
        )

        executive_venues_bind_years(
          function(year) {

            raw <- executive_venues_rows_for_year(
              "facility_hire", year
            )

            revenue <- venues_operations_facility_hire_by_facility(
              raw, metric = "Revenue"
            )

            transactions <- venues_operations_facility_hire_by_facility(
              raw, metric = "Transactions"
            )

            if (nrow(revenue) == 0L &&
                nrow(transactions) == 0L) {
              return(NULL)
            }

            names(revenue)[names(revenue) == "value"] <- "revenue"
            names(transactions)[names(transactions) == "value"] <- "transactions"

            rows <- merge(
              revenue,
              transactions,
              by = "facility",
              all = TRUE,
              sort = FALSE
            )

            data.frame(
              report_year = as.integer(year),
              facility = as.character(rows$facility),
              revenue = as.numeric(rows$revenue),
              transactions = as.numeric(rows$transactions),
              stringsAsFactors = FALSE
            )
          },
          empty
        )
      })


      # ---------------------------------------------------------------
      # Chart 3: Casual Wages - Budget vs Actual
      # ---------------------------------------------------------------

      executive_venues_wages_rows <- reactive({

        empty <- data.frame(
          report_year = integer(),
          budget = double(),
          actual = double(),
          paid_hours = double(),
          stringsAsFactors = FALSE
        )

        executive_venues_bind_years(
          function(year) {

            wages <- executive_venues_rows_for_year(
              "casual_wages", year
            )

            hours <- executive_venues_rows_for_year(
              "casual_hours", year
            )

            budget <- venues_operations_workforce_annual_value(
              wages, metric = "Budget"
            )

            actual <- venues_operations_workforce_annual_value(
              wages, metric = "Actual"
            )

            paid_hours <- venues_operations_workforce_annual_value(
              hours,
              metric = "Paid Hours",
              record_group = "Monthly Summary"
            )

            if (!any(is.finite(c(budget, actual)))) {
              return(NULL)
            }

            data.frame(
              report_year = as.integer(year),
              budget = budget,
              actual = actual,
              paid_hours = paid_hours,
              stringsAsFactors = FALSE
            )
          },
          empty
        )
      })


      # ---------------------------------------------------------------
      # Chart 4: Merchandise Revenue by Section
      # ---------------------------------------------------------------

      executive_venues_merchandise_rows <- reactive({

        empty <- data.frame(
          report_year = integer(),
          section = character(),
          revenue = double(),
          transactions = double(),
          stringsAsFactors = FALSE
        )

        executive_venues_bind_years(
          function(year) {

            raw <- executive_venues_rows_for_year(
              "merchandise", year
            )

            revenue <- venues_operations_merchandise_breakdown(
              raw,
              metric = "Revenue",
              section = "All Sections"
            )

            transactions <- venues_operations_merchandise_breakdown(
              raw,
              metric = "Transactions",
              section = "All Sections"
            )

            if (nrow(revenue) == 0L &&
                nrow(transactions) == 0L) {
              return(NULL)
            }

            # The source helper calls the section column "facility".
            names(revenue) <- c("section", "revenue")
            names(transactions) <- c("section", "transactions")

            rows <- merge(
              revenue,
              transactions,
              by = "section",
              all = TRUE,
              sort = FALSE
            )

            data.frame(
              report_year = as.integer(year),
              section = as.character(rows$section),
              revenue = as.numeric(rows$revenue),
              transactions = as.numeric(rows$transactions),
              stringsAsFactors = FALSE
            )
          },
          empty
        )
      })


      # ---------------------------------------------------------------
      # Executive Venues: render four presentation charts
      # ---------------------------------------------------------------

      # Chart 1: Attendance by Activity
      output$executive_plot_venues_attendance <- renderPlot({

        rows <- executive_venues_attendance_rows()

        validate(
          need(
            nrow(rows) > 0L && any(is.finite(rows$value)),
            "No reported attendance activity totals for these years."
          )
        )

        plot_executive_venues_attendance(rows)

      }, res = 96, bg = "white")


      # Chart 2: Facility Hire Revenue
      output$executive_plot_venues_facility_hire <- renderPlot({

        rows <- executive_venues_hire_rows()

        validate(
          need(
            nrow(rows) > 0L && any(is.finite(rows$revenue)),
            "No facility-hire revenue breakdown for these years."
          )
        )

        plot_executive_venues_facility_hire(rows)

      }, res = 96, bg = "white")


      # Chart 3: Casual Wages - Budget vs Actual
      output$executive_plot_venues_wages <- renderPlot({

        rows <- executive_venues_wages_rows()

        validate(
          need(
            nrow(rows) > 0L &&
              any(is.finite(c(rows$budget, rows$actual))),
            "No reported casual wage amounts for these years."
          )
        )

        plot_executive_venues_wages(rows)

      }, res = 96, bg = "white")


      # Chart 4: Merchandise Revenue by Section
      output$executive_plot_venues_merchandise <- renderPlot({

        rows <- executive_venues_merchandise_rows()

        validate(
          need(
            nrow(rows) > 0L && any(is.finite(rows$revenue)),
            "No merchandise revenue breakdown for these years."
          )
        )

        plot_executive_venues_merchandise(rows)

      }, res = 96, bg = "white")


      # ---------------------------------------------------------------
      # Executive Venues: exact-value hover readouts
      # ---------------------------------------------------------------

      executive_venues_category_hover <- function(
          hover, data, category, value_col, currency = FALSE
      ) {

        hint <- "Hover over a bar to inspect exact values."

        if (is.null(hover) ||
            is.null(hover$y) ||
            !is.finite(hover$y)) {
          return(hint)
        }

        rows <- tryCatch(
          executive_venues_chart_rows(
            data,
            category,
            value_col,
            "Venues hover"
          ),
          error = function(e) NULL
        )

        if (is.null(rows) || nrow(rows) == 0L) {
          return(hint)
        }

        labels <- unique(rows$category)
        y <- round(hover$y)

        if (abs(hover$y - y) > 0.46 ||
            y < 1L ||
            y > length(labels)) {
          return(hint)
        }

        selected <- labels[[length(labels) - as.integer(y) + 1L]]

        parts <- vapply(
          executive_selected_years(),
          function(year) {

            matched <- rows[
              rows$category == selected &
                rows$report_year == year,
              ,
              drop = FALSE
            ]

            value <- if (nrow(matched) > 0L) {
              matched$value[[1L]]
            } else {
              NA_real_
            }

            formatted <- if (!is.finite(value)) {
              "Not reported"
            } else if (currency) {
              format_executive_hover_value(value)
            } else {
              executive_format_count(value)
            }

            paste0(year, ": ", formatted)
          },
          character(1)
        )

        paste(c(selected, parts), collapse = " | ")
      }


      # Attendance hover
      output$executive_hover_value_venues_attendance <- renderText({

        executive_venues_category_hover(
          input$executive_hover_venues_attendance,
          executive_venues_attendance_rows(),
          "activity",
          "value",
          FALSE
        )
      })


      # Facility Hire hover
      output$executive_hover_value_venues_facility_hire <- renderText({

        executive_venues_category_hover(
          input$executive_hover_venues_facility_hire,
          executive_venues_hire_rows(),
          "facility",
          "revenue",
          TRUE
        )
      })


      # Merchandise hover
      output$executive_hover_value_venues_merchandise <- renderText({

        executive_venues_category_hover(
          input$executive_hover_venues_merchandise,
          executive_venues_merchandise_rows(),
          "section",
          "revenue",
          TRUE
        )
      })


      # Casual Wages hover
      output$executive_hover_value_venues_wages <- renderText({

        hint <- "Hover over a year to inspect Budget and Actual wages."

        hover <- input$executive_hover_venues_wages
        rows <- executive_venues_wages_rows()

        if (is.null(hover) ||
            is.null(hover$y) ||
            !is.finite(hover$y) ||
            nrow(rows) == 0L) {
          return(hint)
        }

        rows <- rows[order(-rows$report_year), , drop = FALSE]

        y <- round(hover$y)

        if (abs(hover$y - y) > 0.46 ||
            y < 1L ||
            y > nrow(rows)) {
          return(hint)
        }

        record <- rows[
          nrow(rows) - as.integer(y) + 1L,
          ,
          drop = FALSE
        ]

        paste0(
          record$report_year[[1L]],
          " | Budget ",
          format_executive_hover_value(record$budget),
          " | Actual ",
          format_executive_hover_value(record$actual)
        )
      })


      # ---------------------------------------------------------------
      # Executive Venues: Operational Summary
      # ---------------------------------------------------------------

      output$venues_summary_table <- renderTable({

        years <- executive_selected_years()

        validate(
          need(
            length(years) > 0L,
            "Select a reporting year."
          )
        )

        metrics <- c(
          "Facility Hire Transactions",
          "Paid Casual Hours",
          "Casual Wage Budget Difference",
          "Merchandise Transactions"
        )

        summary_table <- data.frame(
          Measure = metrics,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        # Format reported numbers without treating missing values as zero.
        number_or_dash <- function(value, currency = FALSE) {

          if (length(value) != 1L || !is.finite(value)) {
            return("—")
          }

          if (currency) {
            return(format_executive_hover_value(value))
          }

          executive_format_count(value)
        }

        # Prepare one column per selected reporting year.
        for (year in sort(years)) {

          values <- executive_venues_kpis_for_year(year)

          # Calculate wage budget difference using the same
          # Budget and Actual figures displayed in the wage chart.
          wage_year <- executive_venues_wages_rows()

          wage_year <- wage_year[
            !is.na(wage_year$report_year) &
              wage_year$report_year == year,
            ,
            drop = FALSE
          ]

          wage_budget_difference <- NA_real_

          if (
            nrow(wage_year) == 1L &&
            is.finite(wage_year$budget[[1L]]) &&
            is.finite(wage_year$actual[[1L]])
          ) {
            wage_budget_difference <-
              wage_year$budget[[1L]] - wage_year$actual[[1L]]
          }

          merchandise <- venues_operations_merchandise_kpis(
            executive_venues_rows_for_year(
              "merchandise",
              year
            )
          )

          summary_table[[as.character(year)]] <- c(

            # Facility hire transactions
            number_or_dash(
              values$hire_transactions
            ),

            # Total paid casual hours
            number_or_dash(
              values$paid_hours
            ),

            # Reported wage variance
            number_or_dash(
              wage_budget_difference,
              currency = TRUE
            ),

            # Merchandise and equipment transactions
            number_or_dash(
              merchandise$transactions
            )
          )
        }

        summary_table

      },
      striped = TRUE,
      hover = TRUE,
      bordered = FALSE,
      spacing = "s",
      rownames = FALSE)


      # ---------------------------------------------------------------
      # Executive Venues: Key Insights
      # ---------------------------------------------------------------

      output$venues_key_insights <- renderUI({

        context <- executive_context()

        year <- suppressWarnings(
          as.integer(context$year)
        )

        if (length(year) != 1L || is.na(year)) {
          return(
            shiny::div(
              class = "text-secondary",
              "Select a reporting year."
            )
          )
        }

        insights <- character()

        # -------------------------------------------------------------
        # 1. Highest Attendance Activity
        # -------------------------------------------------------------

        attendance <- executive_venues_attendance_rows()

        attendance <- attendance[
          !is.na(attendance$report_year) &
            attendance$report_year == year &
            is.finite(attendance$value),
          ,
          drop = FALSE
        ]

        if (nrow(attendance) > 0L) {

          top <- attendance[
            which.max(attendance$value),
            ,
            drop = FALSE
          ]

          insights <- c(
            insights,
            paste0(
              "Highest attendance: ",
              top$activity[[1L]],
              " (",
              executive_format_count(top$value[[1L]]),
              " visits)."
            )
          )
        }

        # -------------------------------------------------------------
        # 2. Highest Facility Hire Revenue
        # -------------------------------------------------------------

        hire <- executive_venues_hire_rows()

        hire <- hire[
          !is.na(hire$report_year) &
            hire$report_year == year &
            is.finite(hire$revenue),
          ,
          drop = FALSE
        ]

        if (nrow(hire) > 0L) {

          top <- hire[
            which.max(hire$revenue),
            ,
            drop = FALSE
          ]

          insights <- c(
            insights,
            paste0(
              "Highest facility-hire revenue: ",
              top$facility[[1L]],
              " (",
              format_executive_hover_value(
                top$revenue[[1L]]
              ),
              ")."
            )
          )
        }

        # -------------------------------------------------------------
        # 3. Casual Wages: Budget vs Actual
        # -------------------------------------------------------------

        wages <- executive_venues_wages_rows()

        wages <- wages[
          !is.na(wages$report_year) &
            wages$report_year == year,
          ,
          drop = FALSE
        ]

        if (
          nrow(wages) > 0L &&
          is.finite(wages$budget[[1L]]) &&
          is.finite(wages$actual[[1L]])
        ) {

          budget <- wages$budget[[1L]]
          actual <- wages$actual[[1L]]

          difference <- actual - budget

          wage_status <- if (difference > 0) {

            paste0(
              "above budget by ",
              format_executive_hover_value(difference)
            )

          } else if (difference < 0) {

            paste0(
              "below budget by ",
              format_executive_hover_value(abs(difference))
            )

          } else {

            "in line with budget"
          }

          insights <- c(
            insights,
            paste0(
              "Casual wages: ",
              format_executive_hover_value(actual),
              " actual, ",
              wage_status,
              "."
            )
          )
        }

        # -------------------------------------------------------------
        # 4. Highest Merchandise Revenue Section
        # -------------------------------------------------------------

        merchandise <- executive_venues_merchandise_rows()

        merchandise <- merchandise[
          !is.na(merchandise$report_year) &
            merchandise$report_year == year &
            is.finite(merchandise$revenue),
          ,
          drop = FALSE
        ]

        if (nrow(merchandise) > 0L) {

          top <- merchandise[
            which.max(merchandise$revenue),
            ,
            drop = FALSE
          ]

          insights <- c(
            insights,
            paste0(
              "Highest merchandise revenue: ",
              top$section[[1L]],
              " (",
              format_executive_hover_value(
                top$revenue[[1L]]
              ),
              ")."
            )
          )
        }

        # -------------------------------------------------------------
        # Display results
        # -------------------------------------------------------------

        if (length(insights) == 0L) {

          return(
            shiny::div(
              class = "text-secondary",
              "No reported Venues insights are available for this year."
            )
          )
        }

        shiny::tags$ul(
          class = "executive-insights-list",
          shiny::tagList(
            lapply(insights, shiny::tags$li)
          )
        )
      })


      executive_format_count <- function(
          value
      ) {

        if (
          is.null(value) ||
            length(value) == 0L ||
            !is.finite(value[[1L]])
        ) {
          return("—")
        }

        format(
          round(
            value[[1L]]
          ),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      }

      executive_currency_change_label <- function(
          current,
          previous,
          comparison_year
      ) {

        if (
          is.null(current) ||
            is.null(previous) ||
            length(current) == 0L ||
            length(previous) == 0L ||
            !is.finite(current[[1L]]) ||
            !is.finite(previous[[1L]])
        ) {
          return(NULL)
        }

        difference <-
          current[[1L]] -
            previous[[1L]]

        prefix <-
          if (difference > 0) {
            "+"
          } else {
            ""
          }

        paste0(
          prefix,
          format_compact_currency(
            difference
          ),
          " vs ",
          comparison_year
        )
      }


      executive_count_change_label <- function(
          current,
          previous,
          comparison_year
      ) {

        if (
          is.null(current) ||
            is.null(previous) ||
            length(current) == 0L ||
            length(previous) == 0L ||
            !is.finite(current[[1L]]) ||
            !is.finite(previous[[1L]])
        ) {
          return(NULL)
        }

        difference <-
          current[[1L]] -
            previous[[1L]]

        prefix <-
          if (difference > 0) {
            "+"
          } else {
            ""
          }

        paste0(
          prefix,
          executive_format_count(
            difference
          ),
          " vs ",
          comparison_year
        )
      }

      executive_kpi_specs <- reactive({

        focus <-
          active_focus()

        context <-
          executive_context()

        # ---------------------------------------------------------------
        # Venues: four headline operational KPIs
        # ---------------------------------------------------------------
        if (identical(focus, "Metrics")) {
          current <- executive_venues_kpis_for_year(context$year)

          previous <- if (
            identical(context$view_mode, "Comparison") &&
              !is.null(context$comparison_year)
          ) {
            executive_venues_kpis_for_year(context$comparison_year)
          } else {
            NULL
          }

          count_value <- function(field) {
            executive_format_count(current[[field]])
          }

          money_value <- function(field) {
            value <- current[[field]]

            if (length(value) == 0L || !is.finite(value[[1L]])) {
              return("—")
            }

            format_compact_currency(value[[1L]])
          }

          count_change <- function(field) {
            if (is.null(previous)) return(NULL)

            executive_count_change_label(
              current[[field]],
              previous[[field]],
              context$comparison_year
            )
          }

          money_change <- function(field) {
            if (is.null(previous)) return(NULL)

            executive_currency_change_label(
              current[[field]],
              previous[[field]],
              context$comparison_year
            )
          }

          return(list(
            list(
              title = "Total Attendance",
              value = count_value("total_attendance"),
              comparison = count_change("total_attendance"),
              icon = "people"
            ),
            list(
              title = "Facility Hire Revenue",
              value = money_value("hire_revenue"),
              comparison = money_change("hire_revenue"),
              icon = "cash-stack"
            ),
            list(
              title = "Casual Wages",
              value = money_value("actual_wages"),
              comparison = money_change("actual_wages"),
              icon = "wallet2"
            ),
            list(
              title = "Merchandise Revenue",
              value = money_value("merchandise_sales"),
              comparison = money_change("merchandise_sales"),
              icon = "bag"
            )
          ))
        }

        # ---------------------------------------------------------------
        # Sites: four operational KPIs, independently sourced from detailed
        # reports. Missing sheets remain unavailable, never zero.
        # ---------------------------------------------------------------
        if (identical(focus, "Site")) {
          year <- suppressWarnings(as.integer(context$year))
          current <- executive_site_snapshot_kpis(year)
          previous <- if (identical(context$view_mode, "Comparison") &&
                          !is.null(context$comparison_year)) {
            executive_site_snapshot_kpis(
              suppressWarnings(as.integer(context$comparison_year))
            )
          } else NULL
          comparison <- function(field, coverage = NULL) {
            if (is.null(previous)) return(NULL)
            # A year-on-year change is misleading if the contributing sites
            # differ. Display an explicit coverage message instead.
            if (!is.null(coverage) &&
                !identical(current[[coverage]], previous[[coverage]])) {
              return("Reporting coverage differs between years")
            }
            executive_count_change_label(current[[field]], previous[[field]],
                                         context$comparison_year)
          }
          count_or_dash <- function(value) {
            if (!is.finite(value)) "—" else executive_format_count(value)
          }
          
          # -------------------------------------------------------------
          # Visits Growth
          # Year Review compares with the preceding calendar year.
          # Comparison uses the explicitly selected comparison year.
          # -------------------------------------------------------------

          baseline_year <- if (
            identical(context$view_mode, "Comparison") &&
              !is.null(context$comparison_year)
          ) {
            as.integer(context$comparison_year)
          } else {
            year - 1L
          }

          baseline <- executive_site_snapshot_kpis(baseline_year)

          same_visit_sites <-
            length(current$visit_names) > 0L &&
            identical(current$visit_names, baseline$visit_names)

          comparable_visits <-
            same_visit_sites &&
            is.finite(current$visits) &&
            is.finite(baseline$visits) &&
            baseline$visits > 0

          if (comparable_visits) {
            growth <- 100 * (
              current$visits - baseline$visits
            ) / baseline$visits

            growth_value <- paste0(
              if (growth > 0) "+" else "",
              formatC(growth, format = "f", digits = 1),
              "%"
            )

            growth_note <- paste0("vs ", baseline_year)

          } else {
            growth_value <- "—"

            growth_note <- if (
              is.finite(current$visits) &&
              is.finite(baseline$visits) &&
              baseline$visits > 0
            ) {
              paste0("Reporting coverage differs vs ", baseline_year)
            } else {
              paste0("No comparable visit data for ", baseline_year)
            }
          }

          # -------------------------------------------------------------
          # Sites with Positive Net Result
          # Count sites where annual Income exceeds Expenses.
          # Exclude sites without a reported financial result.
          # -------------------------------------------------------------

          site_finance <- executive_site_financial_rows()

          positive_sites <- function(selected_year) {
            if (
              length(selected_year) != 1L ||
              is.na(selected_year)
            ) {
              return(list(
                positive = NA_integer_,
                total = 0L,
                names = character()
              ))
            }

            rows <- site_finance[
              !is.na(site_finance$report_year) &
                site_finance$report_year == selected_year &
                !is.na(site_finance$site) &
                is.finite(site_finance$net_actual),
              ,
              drop = FALSE
            ]

            site_names <- sort(unique(as.character(rows$site)))

            list(
              positive = if (nrow(rows) > 0L) {
                sum(rows$net_actual > 0)
              } else {
                NA_integer_
              },
              total = length(site_names),
              names = site_names
            )
          }

          positive_current <- positive_sites(year)

          positive_value <- if (
            is.finite(positive_current$positive)
          ) {
            paste0(
              positive_current$positive,
              " / ",
              positive_current$total
            )
          } else {
            "—"
          }

          positive_note <- paste0(
            "Sites with Net Result above $0 in ", year
          )

          if (
            identical(context$view_mode, "Comparison") &&
            !is.null(context$comparison_year)
          ) {
            positive_baseline <- positive_sites(
              as.integer(context$comparison_year)
            )

            if (
              is.finite(positive_current$positive) &&
              is.finite(positive_baseline$positive) &&
              identical(
                positive_current$names,
                positive_baseline$names
              )
            ) {
              difference <-
                positive_current$positive -
                positive_baseline$positive

              positive_note <- paste0(
                if (difference > 0) "+" else "",
                difference,
                " positive sites vs ",
                context$comparison_year
              )
            } else {
              positive_note <-
                "Comparison unavailable or reporting coverage differs"
            }
          }

          return(list(
            list(title = "Total Visits",
                 value = count_or_dash(current$visits),
                 comparison = if (!is.null(previous)) comparison("visits", "visit_names") else
                   paste0(current$visit_sites, " sites reporting both visit types"),
                 icon = "people"),
            list(title = "Active Members", value = count_or_dash(current$members),
                 comparison = if (!is.null(previous)) comparison("members", "member_names") else
                   paste0(current$member_sites, " sites reporting"),
                 icon = "person-badge"),
            # Change in comparable visits across reporting years.
            list(
              title = "Visits Growth",
              value = growth_value,
              comparison = growth_note,
              icon = "graph-up-arrow"
            ),

            # Share of sites generating a positive financial result.
            list(
              title = "Sites with Positive Net Result",
              value = positive_value,
              comparison = positive_note,
              icon = "check-circle"
            )
          ))
        }

        # ---------------------------------------------------------------
        # Overview financial KPIs (unchanged)
        # ---------------------------------------------------------------

        finance <-
          executive_kpi_finance()

        income <- "—"
        expenses <- "—"
        net_result <- "—"
        budget_variance <- "—"

        if (
          !is.null(finance) &&
            is.data.frame(finance) &&
            nrow(finance) > 0L
        ) {

          if (
            is.finite(
              finance$income_actual[[1L]]
            )
          ) {
            income <-
              format_compact_currency(
                finance$income_actual[[1L]]
              )
          }

          if (
            is.finite(
              finance$expenses_actual[[1L]]
            )
          ) {
            expenses <-
              format_compact_currency(
                finance$expenses_actual[[1L]]
              )
          }

          if (
            is.finite(
              finance$net_actual[[1L]]
            )
          ) {
            net_result <-
              format_compact_currency(
                finance$net_actual[[1L]]
              )
          }

          if (
            is.finite(
              finance$budget_variance[[1L]]
            )
          ) {
            budget_variance <-
              format_compact_currency(
                finance$budget_variance[[1L]]
              )
          }
        }

        comparison_finance <-
          executive_kpi_comparison_finance()

        comparison_year <-
          context$comparison_year

        site_selected <-
          identical(
            focus,
            "Site"
          ) &&
            !is.null(context$site) &&
            !identical(
              context$site,
              "All Monash Sport"
            )
        
        list(
          list(
            title =
              if (site_selected) {
                "Site Income"
              } else {
                "Total Income"
              },
            value = income,
            output_id = "kpi_total_income",
            comparison =
              if (!is.null(comparison_finance)) {
                executive_currency_change_label(
                  finance$income_actual,
                  comparison_finance$income_actual,
                  comparison_year
                )
              } else {
                NULL
              },
            icon = "cash-stack"
          ),

          list(
            title =
              if (site_selected) {
                "Site Expenses"
              } else {
                "Total Expenses"
              },
            value = expenses,
            output_id = "kpi_total_expenses",
            comparison =
              if (!is.null(comparison_finance)) {
                executive_currency_change_label(
                  finance$expenses_actual,
                  comparison_finance$expenses_actual,
                  comparison_year
                )
              } else {
                NULL
              },
            icon = "receipt"
          ),

          list(
            title = "Net Result",
            value = net_result,
            output_id = "kpi_net_result",
            comparison =
              if (!is.null(comparison_finance)) {
                executive_currency_change_label(
                  finance$net_actual,
                  comparison_finance$net_actual,
                  comparison_year
                )
              } else {
                NULL
              },
            icon = "wallet2"
          ),

          list(
            title = "Budget Variance",
            value = budget_variance,
            output_id = "kpi_budget_variance",
            comparison =
              if (!is.null(comparison_finance)) {
                executive_currency_change_label(
                  finance$budget_variance,
                  comparison_finance$budget_variance,
                  comparison_year
                )
              } else {
                NULL
              },
            icon = "bullseye"
          )
        )
      })

      output$executive_kpi_grid <- renderUI({

        specs <-
          executive_kpi_specs()

        div(
          class = "executive-kpi-grid",

          tagList(
            lapply(
              specs,
              function(spec) {

                div(
                  class = "executive-kpi-card",

                  div(
                    class = "executive-kpi-content",

                    div(
                      class = "executive-kpi-label",
                      spec$title
                    ),

                    div(
                      class = "executive-kpi-value",

                      if (
                        !is.null(spec$output_id)
                      ) {

                        textOutput(
                          ns(spec$output_id),
                          inline = TRUE
                        )

                      } else {

                        spec$value
                      }
                    ),

                    if (
                      !is.null(spec$comparison) &&
                        length(spec$comparison) > 0L &&
                        !is.na(spec$comparison[[1L]]) &&
                        nzchar(
                          as.character(
                            spec$comparison[[1L]]
                          )
                        )
                    ) {

                      div(
                        class = "executive-kpi-comparison",

                        tags$small(
                          class = "text-secondary",
                          spec$comparison
                        )
                      )

                    } else {

                      NULL
                    }
                  ),

                  div(
                    class = "executive-kpi-icon",

                    bs_icon(
                      spec$icon
                    )
                  )
                )
              }
            )
          )
        )
      })
      
      executive_annual_revenue <- reactive({

        x <-
          calculate_overview_annual_revenue(
            executive_scoped_monthly_reports()
          )

        years <-
          executive_context_trend_years(
            context = executive_context(),
            available_years = x$report_year
          )

        if (length(years) == 0L) {
          return(x[0, , drop = FALSE])
        }

        x[
          x$report_year %in% years,
          ,
          drop = FALSE
        ]
      })

      # -------------------------------------------------------------------
      # Executive all-site snapshot data
      # -------------------------------------------------------------------

      executive_snapshot_year <- reactive({

        context <- executive_context()

        year <-
          suppressWarnings(
            as.integer(
              context$year
            )
          )

        if (
          length(year) == 0L ||
            is.na(year[[1L]])
        ) {
          return(NA_integer_)
        }

        year[[1L]]
      })


      executive_site_snapshot_finance <- reactive({

        x <-
          calculate_site_annual_finance(
            executive_monthly_reports()
          )

        year <- executive_snapshot_year()

        if (is.na(year)) {
          return(
            x[0, , drop = FALSE]
          )
        }

        x[
          x$report_year == year,
          ,
          drop = FALSE
        ]
      })


      executive_site_snapshot_revenue <- reactive({

        x <-
          calculate_site_annual_revenue(
            executive_monthly_reports()
          )

        year <- executive_snapshot_year()

        if (is.na(year)) {
          return(
            x[0, , drop = FALSE]
          )
        }

        x[
          x$report_year == year,
          ,
          drop = FALSE
        ]
      })


      validate_executive_monthly_data <- function(
          x
      ) {

        reports <-
          executive_monthly_reports()

        duplicate_years <-
          overview_duplicate_report_years(
            reports
          )

        validate(
          need(
            !is.null(reports),
            paste(
              "Upload at least one Monthly Site Report",
              "on the Data page."
            )
          ),

          need(
            length(
              duplicate_years
            ) == 0L,
            paste0(
              "More than one Monthly Site Report is active for: ",
              paste(
                duplicate_years,
                collapse = ", "
              ),
              ". Delete the superseded file on the Data page."
            )
          ),

          need(
            nrow(x) > 0L,
            paste(
              "No finance data is available",
              "for the selected Executive Dashboard context."
            )
          )
        )
      }

      # -------------------------------------------------------------------
      # Executive Overview plots
      # -------------------------------------------------------------------

      output$executive_plot_monthly_income_expenses <-
        renderPlot(
          {

            x <- executive_monthly_finance()

            validate_executive_monthly_data(
              x
            )

            available_values <-
              is.finite(
                x$income_actual
              ) |
              is.finite(
                x$expenses_actual
              )

            validate(
              need(
                any(
                  available_values
                ),
                paste(
                  "No monthly Income or Expenses values",
                  "are available for this selection."
                )
              )
            )

            plot_executive_income_expenses(
              x
            )
          },
          res = 96,
          bg = "white"
        )


      output$executive_plot_annual_performance_trend <-
        renderPlot(
          {

            x <- executive_annual_finance()

            validate_executive_monthly_data(
              x
            )

            metrics <-
              input$metric_annual_performance_trend

            if (
              is.null(metrics) ||
                length(metrics) == 0L
            ) {

              metrics <- c(
                "Income",
                "Expenses",
                "Net Result"
              )
            }

            values <- unlist(
              lapply(
                metrics,
                function(metric) {

                  overview_metric_values(
                    x,
                    metric
                  )
                }
              ),
              use.names = FALSE
            )

            validate(
              need(
                any(
                  is.finite(values)
                ),
                paste(
                  "No values are available for",
                  "the selected performance metrics."
                )
              )
            )

            plot_executive_annual_performance(
              x,
              metrics
            )
          },
          res = 96,
          bg = "white"
        )

      output$executive_plot_budget_vs_actual <-
        renderPlot(
          {

            x <- executive_monthly_finance()

            validate_executive_monthly_data(
              x
            )

            metric <-
              input$metric_budget_vs_actual

            if (
              is.null(metric) ||
                !metric %in% FINANCE_METRICS
            ) {
              metric <- "Net Result"
            }

            columns <- switch(
              metric,

              "Income" = c(
                "income_actual",
                "income_budget"
              ),

              "Expenses" = c(
                "expenses_actual",
                "expenses_budget"
              ),

              "Net Result" = c(
                "net_actual",
                "net_budget"
              )
            )

            actual <-
              x[[columns[[1L]]]]

            budget <-
              x[[columns[[2L]]]]

            complete <-
              is.finite(actual) &
              is.finite(budget)

            validate(
              need(
                any(complete),
                paste(
                  "Complete monthly Budget and Actual",
                  metric,
                  "values are not available for this selection."
                )
              )
            )

            plot_data <- x

            # Reuse the existing Budget vs Actual plotting function.
            plot_data$net_actual <-
              actual

            plot_data$net_budget <-
              budget

            plot_executive_budget_actual(
              plot_data
            )
          },
          res = 96,
          bg = "white"
        )

      output$executive_plot_revenue_capture <-
        renderPlot(
          {

            x <- executive_annual_revenue()

            validate_executive_monthly_data(
              x
            )

            complete <-
              is.finite(
                x$actual_income
              ) &
              is.finite(
                x$discount_opportunity
              ) &
              is.finite(
                x$capture_rate
              )

            validate(
              need(
                any(
                  complete
                ),
                paste(
                  "No complete revenue capture values",
                  "are available for this selection."
                )
              )
            )

            plot_executive_revenue_capture(
              x
            )
          },
          res = 96,
          bg = "white"
        )
      
      # -------------------------------------------------------------------
      # Executive Overview hover values
      # -------------------------------------------------------------------
      # Hover readouts use the same source data as the charts.
      # They show exact figures without adding labels to every plotted point.

      format_executive_hover_value <- function(value) {
        if (length(value) == 0L || !is.finite(value[[1L]])) {
          return("—")
        }

        paste0(
          if (value[[1L]] < 0) "-" else "",
          "$",
          formatC(
            abs(value[[1L]]),
            format = "f",
            digits = 2,
            big.mark = ","
          )
        )
      }

      executive_hover_monthly <- function(
          hover,
          data,
          fields,
          labels
      ) {
        if (is.null(hover) || is.null(hover$x)) {
          return("Hover over the chart to inspect exact values.")
        }

        month <- round(hover$x)

        if (!is.finite(month) || month < 1L || month > 12L) {
          return("")
        }

        years <- sort(unique(as.integer(data$report_year)))
        years <- years[!is.na(years)]

        details <- character()

        for (year in years) {
          year_rows <- data[
            !is.na(data$report_year) & data$report_year == year &
              !is.na(data$date) &
              as.integer(format(as.Date(data$date), "%m")) == month,
            ,
            drop = FALSE
          ]

          if (nrow(year_rows) == 0L) {
            next
          }

          values <- vapply(
            fields,
            function(field) {
              available <- suppressWarnings(
                as.numeric(year_rows[[field]])
              )
              available <- available[is.finite(available)]

              if (length(available) == 0L) {
                return(NA_real_)
              }

              sum(available)
            },
            numeric(1)
          )

          details <- c(
            details,
            paste0(
              year,
              ": ",
              paste(
                paste0(
                  labels,
                  " ",
                  vapply(
                    values,
                    format_executive_hover_value,
                    character(1)
                  )
                ),
                collapse = " | "
              )
            )
          )
        }

        if (length(details) == 0L) {
          return(paste(month.abb[[month]], ": No data available"))
        }

        paste(
          c(month.abb[[month]], details),
          collapse = "  •  "
        )
      }

      executive_hover_annual <- function(
          hover,
          data,
          fields,
          labels
      ) {
        if (is.null(hover) || is.null(hover$x)) {
          return("Hover over the chart to inspect exact values.")
        }

        years <- as.integer(data$report_year)
        valid <- !is.na(years)

        data <- data[valid, , drop = FALSE]
        data <- data[order(as.integer(data$report_year)), , drop = FALSE]

        index <- round(hover$x)

        if (
          !is.finite(index) ||
          index < 1L ||
          index > nrow(data)
        ) {
          return("")
        }

        selected <- data[index, , drop = FALSE]

        values <- vapply(
          fields,
          function(field) {
            if (!field %in% names(selected)) {
              return(NA_real_)
            }
            suppressWarnings(as.numeric(selected[[field]][[1L]]))
          },
          numeric(1)
        )

        paste0(
          selected$report_year[[1L]],
          ": ",
          paste(
            paste0(
              labels,
              " ",
              vapply(
                values,
                format_executive_hover_value,
                character(1)
              )
            ),
            collapse = " | "
          )
        )
      }

      output$executive_hover_value_monthly_income_expenses <- renderText({
        executive_hover_monthly(
          input$executive_hover_monthly_income_expenses,
          executive_monthly_finance(),
          c("income_actual", "expenses_actual"),
          c("Income", "Expenses")
        )
      })

      output$executive_hover_value_annual_performance_trend <- renderText({
        hover <- input$executive_hover_annual_performance_trend

        if (is.null(hover) || is.null(hover$x)) {
          return("Hover over the chart to inspect exact values.")
        }

        data <- executive_annual_finance()

        metrics <- input$metric_annual_performance_trend

        if (is.null(metrics) || length(metrics) == 0L) {
          metrics <- c("Income", "Expenses", "Net Result")
        }

        field_names <- c(
          "Income" = "income_actual",
          "Expenses" = "expenses_actual",
          "Net Result" = "net_actual",
          "Budget Variance" = "budget_variance"
        )

        executive_hover_annual(
          input$executive_hover_annual_performance_trend,
          data,
          unname(field_names[metrics]),
          metrics
        )
      })

      output$executive_hover_value_budget_vs_actual <- renderText({
        hover <- input$executive_hover_budget_vs_actual

        if (is.null(hover) || is.null(hover$x)) {
          return("Hover over the chart to inspect exact values.")
        }

        data <- executive_monthly_finance()

        metric <- input$metric_budget_vs_actual

        if (is.null(metric) || !metric %in% FINANCE_METRICS) {
          metric <- "Net Result"
        }

        fields <- switch(
          metric,
          "Income" = c("income_budget", "income_actual"),
          "Expenses" = c("expenses_budget", "expenses_actual"),
          "Net Result" = c("net_budget", "net_actual")
        )

        executive_hover_monthly(
          input$executive_hover_budget_vs_actual,
          data,
          fields,
          c("Budget", "Actual")
        )
      })

      output$executive_hover_value_revenue_capture <- renderText({
        hover <- input$executive_hover_revenue_capture

        if (is.null(hover) || is.null(hover$x)) {
          return("Hover over the chart to inspect exact values.")
        }

        data <- executive_annual_revenue()

        if (is.null(hover) || is.null(hover$x)) {
          return("Hover over the chart to inspect exact values.")
        }

        complete <- is.finite(data$actual_income) &
          is.finite(data$discount_opportunity) &
          is.finite(data$capture_rate) &
          !is.na(data$report_year)

        data <- data[complete, , drop = FALSE]
        index <- round(hover$x)

        if (!is.finite(index) || index < 1L || index > nrow(data)) {
          return("")
        }

        row <- data[index, , drop = FALSE]

        paste0(
          row$report_year[[1L]],
          ": Income ",
          format_executive_hover_value(row$actual_income),
          " | Opportunity ",
          format_executive_hover_value(row$discount_opportunity),
          " | Capture ",
          format_percentage(row$capture_rate)
        )
      })
      
      # Old Executive Site and Facility chart outputs were retired when the
      # fixed four-chart Sites view was introduced. Their shared calculation
      # and plotting functions remain in the project for other dashboards.

      # ---------------------------------------------------------------
      # Executive Sites: factual single-year insights for presentations
      # ---------------------------------------------------------------
      output$site_key_insights <- renderUI({
        context <- executive_context()
        year <- suppressWarnings(as.integer(context$year))
        if (length(year) == 0L || is.na(year[[1L]])) {
          return(shiny::div(class = "text-secondary", "Select a reporting year."))
        }
        year <- year[[1L]]
        finance <- executive_site_financial_rows()
        finance <- finance[!is.na(finance$report_year) &
                             finance$report_year == year, , drop = FALSE]
        operational <- executive_site_operational_rows()
        operational <- operational[!is.na(operational$report_year) &
                                     operational$report_year == year, , drop = FALSE]
        insights <- character()
        if (nrow(finance) > 0L && any(is.finite(finance$net_actual))) {
          i <- which.max(replace(finance$net_actual,
                                 !is.finite(finance$net_actual), -Inf))
          insights <- c(insights, paste0(
            "Highest Net Result: ", finance$site[[i]], " (",
            format_compact_currency(finance$net_actual[[i]]), ")."
          ))
        }
        if (nrow(operational) > 0L) {
          complete <- is.finite(operational$direct) &
            is.finite(operational$indirect)
          if (any(complete)) {
            v <- operational[complete, , drop = FALSE]
            visits <- v$direct + v$indirect
            i <- which.max(visits)
            insights <- c(insights, paste0(
              "Most visits: ", v$site[[i]], " (",
              executive_site_format_count(visits[[i]]), ")."
            ))
          }
          members <- operational[is.finite(operational$active_members), , drop = FALSE]
          if (nrow(members) > 0L) {
            i <- which.max(members$active_members)
            insights <- c(insights, paste0(
              "Most active members: ", members$site[[i]], " (",
              executive_site_format_count(members$active_members[[i]]), ")."
            ))
          }
        }
        kpis <- executive_site_snapshot_kpis(year)
        if (!is.finite(kpis$programmes)) {
          insights <- c(insights, paste0(
            "Program enrolments were not reported for ", year,
            "; Aquatics active enrolments are shown separately."
          ))
        }
        if (length(insights) == 0L) {
          return(shiny::div(class = "text-secondary",
                            "No usable site insights are available."))
        }
        shiny::tags$ul(
          class = "executive-insights-list",
          shiny::tagList(lapply(insights, shiny::tags$li))
        )
      })

      # -------------------------------------------------------------------
      # Executive Key Insights
      # -------------------------------------------------------------------

      output$key_insights <- renderUI({

        insights <- list()

        context <- executive_context()

        selected_year <-
          suppressWarnings(
            as.integer(
              context$year
            )
          )


        # -----------------------------------------------------------------
        # 1. Overall / selected-site Net Result vs budget
        # -----------------------------------------------------------------

        finance <-
          executive_kpi_finance()

        if (
          !is.null(finance) &&
            is.data.frame(finance) &&
            nrow(finance) > 0L &&
            is.finite(
              finance$net_actual[[1L]]
            )
        ) {

          net_result <-
            finance$net_actual[[1L]]

          budget_variance <-
            finance$budget_variance[[1L]]

          context_name <-
            if (
              identical(
                context$site,
                "All Monash Sport"
              )
            ) {
              "Monash Sport"
            } else {
              context$site
            }


          if (
            is.finite(
              budget_variance
            )
          ) {

            if (budget_variance > 0) {

              sentence <- paste0(
                context_name,
                " recorded a Net Result of ",
                format_compact_currency(
                  net_result
                ),
                ", ",
                format_compact_currency(
                  abs(
                    budget_variance
                  )
                ),
                " above budget."
              )

            } else if (
              budget_variance < 0
            ) {

              sentence <- paste0(
                context_name,
                " recorded a Net Result of ",
                format_compact_currency(
                  net_result
                ),
                ", ",
                format_compact_currency(
                  abs(
                    budget_variance
                  )
                ),
                " below budget."
              )

            } else {

              sentence <- paste0(
                context_name,
                " recorded a Net Result of ",
                format_compact_currency(
                  net_result
                ),
                ", in line with budget."
              )
            }

          } else {

            sentence <- paste0(
              context_name,
              " recorded a Net Result of ",
              format_compact_currency(
                net_result
              ),
              "."
            )
          }

          insights <-
            c(
              insights,
              list(
                tags$li(
                  sentence
                )
              )
            )
        }


        # -----------------------------------------------------------------
        # 2. Strongest site by Net Result
        # -----------------------------------------------------------------

        site_finance <-
          executive_site_snapshot_finance()

        if (
          !is.null(site_finance) &&
            is.data.frame(site_finance) &&
            nrow(site_finance) > 0L &&
            "net_actual" %in%
              names(site_finance)
        ) {

          valid <-
            is.finite(
              site_finance$net_actual
            )

          if (any(valid)) {

            candidates <-
              site_finance[
                valid,
                ,
                drop = FALSE
              ]

            best_index <-
              which.max(
                candidates$net_actual
              )

            insights <-
              c(
                insights,
                list(
                  tags$li(
                    paste0(
                      candidates$site[[best_index]],
                      " has the strongest site Net Result at ",
                      format_compact_currency(
                        candidates$net_actual[[best_index]]
                      ),
                      "."
                    )
                  )
                )
              )
          }
        }


        # -----------------------------------------------------------------
        # 3. Largest theoretical discount opportunity
        # -----------------------------------------------------------------

        site_revenue <-
          executive_site_snapshot_revenue()

        if (
          !is.null(site_revenue) &&
            is.data.frame(site_revenue) &&
            nrow(site_revenue) > 0L &&
            "discount_opportunity" %in%
              names(site_revenue)
        ) {

          valid <-
            is.finite(
              site_revenue$discount_opportunity
            )

          if (any(valid)) {

            candidates <-
              site_revenue[
                valid,
                ,
                drop = FALSE
              ]

            opportunity_index <-
              which.max(
                candidates$discount_opportunity
              )

            insights <-
              c(
                insights,
                list(
                  tags$li(
                    paste0(
                      candidates$site[[opportunity_index]],
                      " has the largest theoretical discount opportunity at ",
                      format_compact_currency(
                        candidates$discount_opportunity[[opportunity_index]]
                      ),
                      "."
                    )
                  )
                )
              )
          }
        }


        # -----------------------------------------------------------------
        # No usable data
        # -----------------------------------------------------------------

        if (length(insights) == 0L) {

          return(
            div(
              class = "text-secondary",
              "No insights are available for the selected dashboard context."
            )
          )
        }


        tagList(
          tags$ul(
            class = "executive-insights-list",
            insights
          )
        )
      })

      # -------------------------------------------------------------------
      # Dynamic Finance visual grid
      # -------------------------------------------------------------------
            
      output$finance_visual_grid <- renderUI({

        # Venues: fixed four-chart executive dashboard
        if (identical(active_focus(), "Metrics")) {
          context <- executive_context()

          context_label <- executive_visual_context_label(
            view_mode = context$view_mode,
            year = context$year,
            comparison_year = context$comparison_year,
            site = "All Venues"
          )

          return(
            executive_venues_dashboard_ui(
              ns = ns,
              context_label = context_label,
              years = executive_selected_years()
            )
          )
        }

        # Sites: existing fixed dashboard, unchanged
        if (identical(active_focus(), "Site")) {
          context <- executive_context()

          context_label <- executive_visual_context_label(
            view_mode = context$view_mode, year = context$year,
            comparison_year = context$comparison_year,
            site = "All Sites"
          )
          return(executive_site_dashboard_ui(ns, context_label))
        }

        context <- executive_context()

        context_label <-
          executive_visual_context_label(
            view_mode =
              context$view_mode,

            year =
              context$year,

            site =
              context$site,

            comparison_year =
              context$comparison_year
          )


        # Cross-site comparison visuals intentionally use all sites.
        site_snapshot_mode <- if (
          identical(
            context$view_mode,
            "Comparison"
          )
        ) {
          "Comparison snapshot"
        } else {
          "Year Review"
        }


        site_snapshot_label <-
          paste(
            site_snapshot_mode,
            context$year,
            "All Sites",
            sep = " · "
          )
        
        trend_context_label <- if (
          identical(
            context$view_mode,
            "Comparison"
          )
        ) {

          paste(
            "Comparison",
            paste0(
              context$year,
              " vs ",
              context$comparison_year
            ),
            context$site,
            sep = " · "
          )

        } else {

          paste(
            "History through",
            context$year,
            context$site,
            sep = " · "
          )
        }

        facility_snapshot_label <- if (
          identical(
            context$site,
            "All Monash Sport"
          )
        ) {

          paste(
            "Select a Site",
            context$year,
            sep = " · "
          )

        } else {

          paste(
            context$site,
            context$year,
            "Facility snapshot",
            sep = " · "
          )
        }

        facility_trend_label <- if (
          identical(
            context$site,
            "All Monash Sport"
          )
        ) {

          paste(
            "Select a Site",
            context$year,
            sep = " · "
          )

        } else if (
          identical(
            context$view_mode,
            "Comparison"
          )
        ) {

          paste(
            context$site,
            paste0(
              context$year,
              " vs ",
              context$comparison_year
            ),
            "Facility trend",
            sep = " · "
          )

        } else {

          paste(
            context$site,
            "History through",
            context$year,
            sep = " · "
          )
        }

        context_label_overrides <-
          c(
            annual_performance_trend =
              trend_context_label,

            revenue_capture =
              trend_context_label,

            site_performance =
              site_snapshot_label,

            site_ranking =
              site_snapshot_label,

            site_budget_performance =
              site_snapshot_label,

            site_revenue_opportunity =
              site_snapshot_label,

            site_discount_mix =
              site_snapshot_label,

            facility_ranking =
              facility_snapshot_label,

            facility_performance =
              facility_trend_label,

            facility_discount_mix =
              facility_snapshot_label
          )


        executive_visual_grid_ui(
          visual_ids =
            if (identical(active_focus(), "Overview")) {
              c(
                "monthly_income_expenses",
                "annual_performance_trend",
                "budget_vs_actual",
                "revenue_capture"
              )
            } else {
              selected_visuals[[active_focus()]]
            },

          context_label =
            context_label,

          context_label_overrides =
            context_label_overrides,

          ns = ns
        )
      })

      # -------------------------------------------------------------------
      # Dynamic Finance table / insight components
      # -------------------------------------------------------------------

      
      output$finance_supporting_components <- renderUI({

        # Venues: Operational Summary and Key Insights
        if (identical(active_focus(), "Metrics")) {
          return(
            executive_venues_supporting_ui(ns)
          )
        }

        # Sites: existing financial rankings and insights
        if (identical(active_focus(), "Site")) {
          return(shiny::div(
            class = "executive-site-bottom-grid",
            executive_site_rankings_card_ui(ns),
            executive_site_insights_card_ui(ns)
          ))
        }


        if (identical(active_focus(), "Overview")) {

          return(
            div(
              class = paste(
                "executive-dashboard-bottom-grid",
                "executive-overview-bottom-grid"
              ),

              executive_financial_summary_card_ui(ns),

              executive_key_insights_card_ui(ns)
            )
          )
        }

        executive_supporting_components_ui(
          visual_ids = selected_visuals[[active_focus()]],
          ns = ns
        )
      })

      # -------------------------------------------------------------------
      # Visual picker contents
      # -------------------------------------------------------------------

      output$visual_picker_body <- renderUI({

        registry <- picker_registry()
        category <- active_category()

        if (
          is.null(registry) ||
            !is.data.frame(registry) ||
            nrow(registry) == 0L
        ) {

          return(
            div(
              class = "executive-visual-picker-empty",

              bs_icon(
                "bar-chart",
                size = "24px"
              ),

              div(
                tags$strong(
                  paste0(
                    "No ",
                    category,
                    " visuals are registered yet."
                  )
                ),

                tags$p(
                  paste(
                    "Visuals will appear here as the",
                    "corresponding dashboard sections are completed."
                  )
                )
              )
            )
          )
        }

        focus <- active_focus()

        source_groups <- intersect(
          names(EXECUTIVE_SOURCE_LABELS),
          unique(
            registry$source_group
          )
        )


        source_panels <- lapply(
          source_groups,
          function(source_group) {

            source_rows <- registry[
              registry$source_group ==
                source_group,
              ,
              drop = FALSE
            ]


            available_rows <- source_rows[
              source_rows$available,
              ,
              drop = FALSE
            ]


            unavailable_rows <- source_rows[
              !source_rows$available,
              ,
              drop = FALSE
            ]


            source_label <-
              executive_visual_source_label(
                source_group
              )


            available_count <-
              nrow(
                available_rows
              )

            total_count <-
              nrow(
                source_rows
              )


            current_selection <-
              selected_visuals[[focus]]

            available_choices <- if (
              available_count > 0L
            ) {

              available_labels <- paste0(
                available_rows$title,
                " · ",
                toupper(
                  available_rows$component_type
                )
              )

              stats::setNames(
                available_rows$id,
                available_labels
              )

            } else {

              stats::setNames(
                character(),
                character()
              )
            }


            available_ui <- if (
              available_count > 0L
            ) {

              checkboxGroupInput(
                ns(
                  paste0(
                    "visuals_",
                    source_group
                  )
                ),
                label = NULL,
                choices =
                  available_choices,
                selected =
                  intersect(
                    current_selection,
                    available_rows$id
                  )
              )

            } else {

              div(
                class =
                  "executive-visual-source-empty",

                bs_icon("info-circle"),

                span(
                  "No visuals from this source are currently available."
                )
              )
            }


            unavailable_ui <- if (
              nrow(
                unavailable_rows
              ) > 0L
            ) {

              conditionalPanel(
                condition = sprintf(
                  "!input['%s']",
                  ns(
                    "visuals_available_only"
                  )
                ),

                div(
                  class =
                    "executive-visual-unavailable-list",

                  div(
                    class =
                      "executive-visual-unavailable-heading",
                    "Unavailable"
                  ),

                  lapply(
                    seq_len(
                      nrow(
                        unavailable_rows
                      )
                    ),
                    function(row_index) {

                      row <-
                        unavailable_rows[
                          row_index,
                          ,
                          drop = FALSE
                        ]

                      div(
                        class =
                          "executive-visual-unavailable-row",

                        div(
                          class =
                            "executive-visual-unavailable-title",

                          bs_icon("lock"),

                          span(
                            row$title[[1L]]
                          )
                        ),

                        div(
                          class =
                            "executive-visual-unavailable-reason",

                          row$availability_reason[[1L]]
                        )
                      )
                    }
                  )
                )
              )

            } else {

              NULL
            }


            div(
              class =
                "executive-visual-source-card",

              div(
                class =
                  "executive-visual-source-header",

                div(
                  div(
                    class =
                      "executive-visual-source-title",
                    source_label
                  ),

                  div(
                    class =
                      "executive-visual-source-description",

                    switch(
                      source_group,

                      monthly =
                        paste(
                          "Organisation and",
                          "site-level performance"
                        ),

                      detailed =
                        paste(
                          "Facility and",
                          "business-unit analysis"
                        ),

                      venues =
                        paste(
                          "Venue operations",
                          "and attendance"
                        ),

                      forecasting =
                        paste(
                          "Forward-looking",
                          "financial performance"
                        ),

                      supporting = 
                        paste(
                          "Presentation tables",
                          "and insights"
                        ),

                      "Dashboard visuals"
                    )
                  )
                ),

                span(
                  class =
                    if (
                      available_count ==
                        total_count
                    ) {
                      "executive-visual-count available"
                    } else if (
                      available_count > 0L
                    ) {
                      "executive-visual-count partial"
                    } else {
                      "executive-visual-count unavailable"
                    },

                  paste0(
                    available_count,
                    " / ",
                    total_count,
                    " available"
                  )
                )
              ),

              div(
                class =
                  "executive-visual-source-body",

                available_ui,

                unavailable_ui
              )
            )
          }
        )


        tagList(
          source_panels
        )
      })


      # -------------------------------------------------------------------
      # Open visual picker
      # -------------------------------------------------------------------

      observeEvent(
        input$choose_visuals,
        {

          category <-
            active_category()

          showModal(
            modalDialog(

              title = paste(
                "Choose",
                category,
                "Visuals"
              ),

              size = "l",

              easyClose = TRUE,

              div(
                class =
                  "executive-visual-picker",

                div(
                  class =
                    "executive-visual-picker-intro",

                  div(
                    tags$strong(
                      paste0(
                        category,
                        " presentation visuals"
                      )
                    ),

                    tags$p(
                      paste(
                        "Choose graphs, tables and insights to include in the",
                        "Executive Dashboard.",
                        "Components are grouped by their data source."
                      )
                    )
                  ),

                  checkboxInput(
                    ns(
                      "visuals_available_only"
                    ),
                    "Available only",
                    value = TRUE
                  )
                ),


                div(
                  class =
                    "executive-visual-picker-toolbar",

                  actionButton(
                    ns(
                      "visuals_recommended"
                    ),
                    "Recommended",
                    icon =
                      bs_icon(
                        "stars"
                      )
                  ),

                  actionButton(
                    ns(
                      "visuals_select_all"
                    ),
                    "Select All",
                    icon =
                      bs_icon(
                        "check2-square"
                      )
                  ),

                  actionButton(
                    ns(
                      "visuals_clear"
                    ),
                    "Clear",
                    icon =
                      bs_icon(
                        "x-lg"
                      )
                  )
                ),


                uiOutput(
                  ns(
                    "visual_picker_body"
                  )
                )
              ),

              footer = tagList(

                modalButton(
                  "Cancel"
                ),

                actionButton(
                  ns(
                    "apply_visuals"
                  ),
                  "Apply Visuals",
                  icon =
                    bs_icon(
                      "check-lg"
                    ),
                  class =
                    "btn-primary"
                )
              )
            )
          )
        },
        ignoreInit = TRUE
      )


      # -------------------------------------------------------------------
      # Recommended visuals
      # -------------------------------------------------------------------

      observeEvent(
        input$visuals_recommended,
        {

          registry <- picker_registry()
          category <- active_category()

          if (
            is.null(registry) ||
              nrow(registry) == 0L
          ) {
            return()
          }


          source_groups <- unique(
            registry$source_group
          )


          for (
            source_group in
              source_groups
          ) {

            source_rows <- registry[
              registry$source_group ==
                source_group &
                registry$available,
              ,
              drop = FALSE
            ]


            recommended_ids <-
              source_rows$id[
                source_rows$default_selected
              ]


            updateCheckboxGroupInput(
              session,
              paste0(
                "visuals_",
                source_group
              ),
              selected =
                recommended_ids
            )
          }
        },
        ignoreInit = TRUE
      )


      # -------------------------------------------------------------------
      # Select every currently available visual
      # -------------------------------------------------------------------

      observeEvent(
        input$visuals_select_all,
        {

          registry <- picker_registry()

          if (
            is.null(registry) ||
              nrow(registry) == 0L
          ) {
            return()
          }


          source_groups <- unique(
            registry$source_group
          )


          for (
            source_group in
              source_groups
          ) {

            source_rows <- registry[
              registry$source_group ==
                source_group &
                registry$available,
              ,
              drop = FALSE
            ]


            updateCheckboxGroupInput(
              session,
              paste0(
                "visuals_",
                source_group
              ),
              selected =
                source_rows$id
            )
          }
        },
        ignoreInit = TRUE
      )


      # -------------------------------------------------------------------
      # Clear currently available selections
      # -------------------------------------------------------------------

      observeEvent(
        input$visuals_clear,
        {

          registry <- picker_registry()

          if (
            is.null(registry) ||
              nrow(registry) == 0L
          ) {
            return()
          }


          source_groups <- unique(
            registry$source_group
          )


          for (
            source_group in
              source_groups
          ) {

            updateCheckboxGroupInput(
              session,
              paste0(
                "visuals_",
                source_group
              ),
              selected =
                character()
            )
          }
        },
        ignoreInit = TRUE
      )


      # -------------------------------------------------------------------
      # Apply visual selection
      # -------------------------------------------------------------------

      observeEvent(
        input$apply_visuals,
        {

          registry <- picker_registry()
          category <- active_category()
          focus <- active_focus()

          if (
            is.null(registry) ||
              nrow(registry) == 0L
          ) {

            selected_visuals[[focus]] <- character()

            removeModal()

            return()
          }


          source_groups <- unique(
            registry$source_group
          )


          chosen <- unlist(
            lapply(
              source_groups,
              function(source_group) {

                input[[paste0("visuals_",source_group)]]
              }
            ),
            use.names = FALSE
          )


          chosen <- unique(
            as.character(
              chosen
            )
          )


          chosen <- chosen[
            chosen %in%
              registry$id[
                registry$available
              ]
          ]


          selected_visuals[[focus]] <- chosen
          removeModal()
        },
        ignoreInit = TRUE
      )


      # -------------------------------------------------------------------
      # Expose state for later dashboard rendering and automated tests
      # -------------------------------------------------------------------

      invisible(
        list(

          selected_visuals = reactive({
            list(
              Overview =
                selected_visuals$Overview,
              Site =
                selected_visuals$Site,
              Metrics =
                selected_visuals$Metrics
            )
          }),

          active_category =
            active_category,

          active_focus =
            active_focus,

          registry =
            picker_registry,

          context =
            executive_context
        )
      )
    }
  )
}