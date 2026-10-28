# -------------------------------------------------------------------------
# Executive Dashboard module
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

executive_visual_metric_control_ui <- function(
    visual_id,
    ns
) {

  metric_choices <- FINANCE_METRICS

  switch(
    visual_id,

    annual_performance_trend =
      div(
        class =
          "chart-header-control executive-visual-metric-control",

        selectizeInput(
          ns("metric_annual_performance_trend"),
          label = NULL,
          choices = metric_choices,
          selected = c(
            "Income",
            "Expenses",
            "Net Result"
          ),
          multiple = TRUE,
          options = list(
            maxItems = 3,
            placeholder = "Choose metrics"
          )
        )
      ),

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

    subtitle_ui <- div(
      class = "chart-header-subtitle",
      context_label[[1L]]
    )
  }

  metric_control <-
  executive_visual_metric_control_ui(
    visual_id = visual_id,
    ns = ns
  )

  card(
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
      ns(
        paste0(
          "executive_plot_",
          visual_id
        )
      ),
      height = "320px"
    )
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

  div(
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

  card(
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
              "Income",
              "Expenses",
              "Net Result",
              "Budget Variance",
              "Discount Opportunity",
              "Potential Revenue",
              "Revenue Capture Rate"
            ),
            selected = "Income"
          )
        )
      )
    ),

    placeholder_chart(
      "Site performance table",
      paste(
        "Exact site values will be connected",
        "after the dashboard shell is complete"
      )
    )
  )
}


executive_key_insights_card_ui <- function() {

  card(
    class = "executive-insights-card",
    full_screen = TRUE,

    card_header(
      "Key Insights"
    ),

    div(
      class = "executive-insights-placeholder",

      bs_icon(
        "lightbulb",
        size = "24px"
      ),

      div(
        tags$strong(
          "Presentation talking points will appear here."
        ),

        tags$p(
          paste(
            "The dashboard will automatically highlight",
            "important performance movements, risks and opportunities."
          )
        )
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
          executive_key_insights_card_ui(),

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

  div(
    class = layout_class,
    tagList(
      components
    )
  )
}

mod_executive_dashboard_ui <- function(id) {

  ns <- NS(id)

  nav_panel(
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

      # Finance / Metrics switcher ----------------------------------------
      div(
        class = "executive-dashboard-navigation",

        navset_pill(
          id = ns("dashboard_section"),
          selected = "finance",

          nav_panel(
            "Finance",
            value = "finance",

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

                  selectInput(
                    ns("view_mode"),
                    "View",
                    choices = c(
                      "Year Review",
                      "Comparison"
                    ),
                    selected = "Year Review"
                  ),

                  div(
                    class = "executive-dashboard-year-controls",

                    selectInput(
                      ns("year"),
                      "Year",
                      choices = APP_YEARS,
                      selected = LATEST_APP_YEAR
                    ),

                    conditionalPanel(
                      condition =
                        "input.view_mode == 'Comparison'",

                      selectInput(
                        ns("comparison_year"),
                        "Compare To",
                        choices = setdiff(
                          APP_YEARS,
                          LATEST_APP_YEAR
                        ),
                        selected =
                          executive_default_comparison_year(
                            year = LATEST_APP_YEAR,
                            years = APP_YEARS
                          )
                      ),

                      ns = ns
                    )
                  ),

                  selectInput(
                    ns("site"),
                    "Site",
                    choices = c(
                      "All Monash Sport",
                      SITE_NAMES
                    ),
                    selected = "All Monash Sport"
                  ),

                  div(
                    class = "executive-dashboard-actions",

                    actionButton(
                      ns("choose_visuals"),
                      "Choose Visuals",
                      icon = bs_icon("sliders")
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
                    4,
                    2,
                    4
                  )
                )
              ),
                
              # KPI strip -------------------------------------------------
              
              div(
                class = "executive-kpi-grid",

                div(
                  class = "executive-kpi-card",

                  div(
                    class = "executive-kpi-content",

                    div(
                      class = "executive-kpi-label",
                      "Total Income"
                    ),

                    div(
                      class = "executive-kpi-value",
                      textOutput(
                        ns("kpi_total_income"),
                        inline = TRUE
                      )
                    )
                  ),

                  div(
                    class = "executive-kpi-icon",
                    bs_icon("cash-stack")
                  )
                ),

                div(
                  class = "executive-kpi-card",

                  div(
                    class = "executive-kpi-content",

                    div(
                      class = "executive-kpi-label",
                      "Total Expenses"
                    ),

                    div(
                      class = "executive-kpi-value",
                      textOutput(
                        ns("kpi_total_expenses"),
                        inline = TRUE
                      )
                    )
                  ),

                  div(
                    class = "executive-kpi-icon",
                    bs_icon("receipt")
                  )
                ),

                div(
                  class = "executive-kpi-card",

                  div(
                    class = "executive-kpi-content",

                    div(
                      class = "executive-kpi-label",
                      "Net Result"
                    ),

                    div(
                      class = "executive-kpi-value",
                      textOutput(
                        ns("kpi_net_result"),
                        inline = TRUE
                      )
                    )
                  ),

                  div(
                    class = "executive-kpi-icon",
                    bs_icon("wallet2")
                  )
                ),

                div(
                  class = "executive-kpi-card",

                  div(
                    class = "executive-kpi-content",

                    div(
                      class = "executive-kpi-label",
                      "Budget Variance"
                    ),

                    div(
                      class = "executive-kpi-value",
                      textOutput(
                        ns("kpi_budget_variance"),
                        inline = TRUE
                      )
                    )
                  ),

                  div(
                    class = "executive-kpi-icon",
                    bs_icon("bullseye")
                  )
                )
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
          ),

          nav_panel(
            "Metrics",
              value = "metrics",

              div(
                class = "pt-3",

                card(
                  full_screen = TRUE,

                  card_header(
                    "Metrics Dashboard"
                  ),

                  placeholder_chart(
                    "Operational and participation metrics",
                    paste(
                      "Direct Visits, Indirect Visits, memberships,",
                      "enrolments and venue metrics will be added",
                      "as those dashboard sections are completed."
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

mod_executive_dashboard_server <- function(
    id,
    data = NULL
) {

  moduleServer(
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


      # -------------------------------------------------------------------
      # Selected visuals
      # -------------------------------------------------------------------
      # Finance and Metrics keep their own selections so future operational
      # visuals do not interfere with the Finance presentation layout.

      selected_visuals <- reactiveValues(

        Finance =
          executive_default_visual_ids(
            "Finance"
          ),

        Metrics =
          executive_default_visual_ids(
            "Metrics"
          )
      )


      # -------------------------------------------------------------------
      # Registry for the currently active category
      # -------------------------------------------------------------------

      picker_registry <- reactive({

        executive_registry_availability_table(
          dashboard_data =
            dashboard_data_value(),
          category =
            active_category()
        )
      })

      # -------------------------------------------------------------------
      # Executive Dashboard context
      # -------------------------------------------------------------------

      executive_context <- reactive({

        comparison_year <- NULL

        if (
          identical(
            input$view_mode,
            "Comparison"
          )
        ) {

          comparison_year <-
            input$comparison_year
        }

        list(
          view_mode =
            input$view_mode,

          year =
            input$year,

          comparison_year =
            comparison_year,

          site =
            input$site
        )
      })

      observeEvent(
        input$year,
        {

          choices <- setdiff(
            as.character(APP_YEARS),
            as.character(input$year)
          )

          current_value <- input$comparison_year

          if (
            is.null(current_value) ||
              !current_value %in% choices
          ) {

            current_value <-
              executive_default_comparison_year(
                year = input$year,
                years = APP_YEARS
              )
          }

          updateSelectInput(
            session,
            "comparison_year",
            choices = choices,
            selected = current_value
          )
        },
        ignoreInit = FALSE
      )

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


      executive_annual_revenue <- reactive({

        x <-
          calculate_overview_annual_revenue(
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

      # -------------------------------------------------------------------
      # Executive all-site snapshot data
      # -------------------------------------------------------------------

      executive_site_trend_finance <- reactive({

        x <-
          calculate_site_annual_finance(
            executive_monthly_reports()
          )

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
          return(
            x[0, , drop = FALSE]
          )
        }

        if (
          identical(
            context$view_mode,
            "Comparison"
          ) &&
            !is.null(
              context$comparison_year
            )
        ) {

          comparison_year <-
            suppressWarnings(
              as.integer(
                context$comparison_year
              )
            )

          years <- c(
            selected_year,
            comparison_year
          )

          return(
            x[
              x$report_year %in% years,
              ,
              drop = FALSE
            ]
          )
        }

        x[
          x$report_year <= selected_year,
          ,
          drop = FALSE
        ]
      })

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


      executive_site_snapshot_discounts <- reactive({

        x <-
          calculate_site_annual_discounts(
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

      validate_executive_site_snapshot_data <- function(
          x,
          empty_message
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
            empty_message
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

            plot_monthly_income_expenses(
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

            plot_annual_performance_trend(
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

            plot_monthly_budget_vs_actual_net_result(
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

            plot_revenue_capture_over_time(
              x
            )
          },
          res = 96,
          bg = "white"
        )
      
      # -------------------------------------------------------------------
      # Executive Site Comparison plots
      # -------------------------------------------------------------------

      output$executive_plot_site_performance <-
        renderPlot(
          {

            x <-
              executive_site_trend_finance()

            validate_executive_site_snapshot_data(
              x,
              paste(
                "No site finance data is available",
                "for the selected period."
              )
            )

            metric <-
              input$metric_site_performance

            if (
              is.null(metric) ||
                !metric %in% FINANCE_METRICS
            ) {
              metric <- "Net Result"
            }

            values <-
              site_finance_values(
                x,
                metric,
                "Actual"
              )

            validate(
              need(
                any(
                  is.finite(values)
                ),
                paste(
                  "No Actual",
                  metric,
                  "values are available for the selected period."
                )
              )
            )

            plot_site_performance_over_time(
              x,
              metric
            )
          },
          res = 96,
          bg = "white"
        )

      output$executive_plot_site_ranking <-
        renderPlot(
          {

            x <-
              executive_site_snapshot_finance()

            validate_executive_site_snapshot_data(
              x,
              paste(
                "No site finance data is available",
                "for the selected year."
              )
            )

            metric <-
              input$metric_site_ranking

            if (
              is.null(metric) ||
                !metric %in% FINANCE_METRICS
            ) {
              metric <- "Net Result"
            }

            values <-
              site_finance_values(
                x,
                metric,
                "Actual"
              )

            validate(
              need(
                any(
                  is.finite(values)
                ),
                paste(
                  "No Actual",
                  metric,
                  "values are available for the selected year."
                )
              )
            )

            plot_site_ranking(
              x,
              metric
            )
          },
          res = 96,
          bg = "white"
        )
      
      output$executive_plot_site_budget_performance <-
        renderPlot(
          {

            x <-
              executive_site_snapshot_finance()

            validate_executive_site_snapshot_data(
              x,
              paste(
                "No site finance data is available",
                "for the selected year."
              )
            )

            metric <-
              input$metric_site_budget_performance

            if (
              is.null(metric) ||
                !metric %in% FINANCE_METRICS
            ) {
              metric <- "Net Result"
            }

            actual <-
              site_finance_values(
                x,
                metric,
                "Actual"
              )

            budget <-
              site_finance_values(
                x,
                metric,
                "Budget"
              )

            complete <-
              is.finite(actual) &
              is.finite(budget)

            validate(
              need(
                any(complete),
                paste(
                  "Complete Budget and Actual",
                  metric,
                  "values are not available for the selected year."
                )
              )
            )

            plot_site_budget_performance(
              x,
              metric
            )
          },
          res = 96,
          bg = "white"
        )
      
      output$executive_plot_site_revenue_opportunity <-
        renderPlot(
          {

            x <-
              executive_site_snapshot_revenue()

            validate_executive_site_snapshot_data(
              x,
              paste(
                "No site revenue opportunity data is available",
                "for the selected year."
              )
            )

            complete <-
              is.finite(
                x$actual_income
              ) &
              is.finite(
                x$discount_opportunity
              ) &
              is.finite(
                x$potential_revenue
              )

            validate(
              need(
                any(complete),
                paste(
                  "No complete revenue opportunity values",
                  "are available for the selected year."
                )
              )
            )

            plot_site_revenue_opportunity(
              x
            )
          },
          res = 96,
          bg = "white"
        )
      
      output$executive_plot_site_discount_mix <-
        renderPlot(
          {

            x <-
              executive_site_snapshot_discounts()

            validate_executive_site_snapshot_data(
              x,
              paste(
                "No site discount data is available",
                "for the selected year."
              )
            )

            complete <-
              is.finite(
                x$student_discount
              ) &
              is.finite(
                x$staff_discount
              ) &
              is.finite(
                x$other_discount
              ) &
              is.finite(
                x$total_discount
              ) &
              x$total_discount > 0

            validate(
              need(
                any(complete),
                paste(
                  "No complete discount mix values",
                  "are available for the selected year."
                )
              )
            )

            plot_site_discount_mix(
              x
            )
          },
          res = 96,
          bg = "white"
        )

      # -------------------------------------------------------------------
      # Dynamic Finance visual grid
      # -------------------------------------------------------------------

      output$finance_visual_grid <- renderUI({

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


        context_label_overrides <-
          c(
            site_performance =
              site_snapshot_label,
        
            site_ranking =
              site_snapshot_label,

            site_revenue_opportunity =
              site_snapshot_label,

            site_discount_mix =
              site_snapshot_label
          )


        executive_visual_grid_ui(
          visual_ids =
            selected_visuals$Finance,

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

        executive_supporting_components_ui(
          visual_ids =
            selected_visuals$Finance,
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
              selected_visuals[[category]]

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

          if (
            is.null(registry) ||
              nrow(registry) == 0L
          ) {

            selected_visuals[[category]] <- character()

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


          selected_visuals[[category]] <- chosen
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
              Finance =
                selected_visuals$Finance,
              Metrics =
                selected_visuals$Metrics
            )
          }),

          active_category =
            active_category,

          registry =
            picker_registry,

          context =
            executive_context
        )
      )
    }
  )
}