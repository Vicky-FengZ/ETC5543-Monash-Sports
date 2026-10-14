# -------------------------------------------------------------------------
# Site Detail module
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Site Detail data helpers
# -------------------------------------------------------------------------

site_detail_filter_rows <- function(
    data,
    year = NULL,
    site = NULL
) {

  if (
    is.null(data) ||
      !is.data.frame(data) ||
      nrow(data) == 0L
  ) {
    return(data)
  }

  keep <- rep(TRUE, nrow(data))

  if (
    !is.null(year) &&
      length(year) > 0L
  ) {

    selected_year <- suppressWarnings(
      as.integer(year[[1L]])
    )

    if (
      !is.na(selected_year) &&
        "report_year" %in% names(data)
    ) {
      keep <- keep &
        data$report_year == selected_year
    }
  }

  if (
    !is.null(site) &&
      length(site) > 0L &&
      nzchar(as.character(site[[1L]]))
  ) {

    key <- facility_site_key(
      as.character(site[[1L]])
    )

    if (
      is.null(key) ||
        !all(
          c(
            "report_unit",
            "campus"
          ) %in% names(data)
        )
    ) {
      return(
        data[0, , drop = FALSE]
      )
    }

    keep <- keep &
      data$report_unit == key$report_unit &
      data$campus == key$campus
  }

  data[
    !is.na(keep) & keep,
    ,
    drop = FALSE
  ]
}


site_detail_available_years <- function(
    site_reports
) {

  if (
    is.null(site_reports) ||
      !is.list(site_reports)
  ) {
    return(integer())
  }

  tables <- lapply(
    c(
      "visits",
      "memberships",
      "enrollments"
    ),
    function(name) {
      site_reports[[name]]
    }
  )

  years <- unlist(
    lapply(
      tables,
      function(data) {

        if (
          is.null(data) ||
            !is.data.frame(data) ||
            !"report_year" %in% names(data)
        ) {
          return(integer())
        }

        as.integer(data$report_year)
      }
    ),
    use.names = FALSE
  )

  sort(
    unique(
      years[
        !is.na(years)
      ]
    )
  )
}


site_detail_available_sites <- function(
    site_reports,
    year = NULL
) {

  if (
    is.null(site_reports) ||
      !is.list(site_reports)
  ) {
    return(character())
  }

  SITE_NAMES[
    vapply(
      SITE_NAMES,
      function(site) {

        any(
          vapply(
            c(
              "visits",
              "memberships",
              "enrollments"
            ),
            function(name) {

              data <- site_detail_filter_rows(
                site_reports[[name]],
                year = year,
                site = site
              )

              is.data.frame(data) &&
                nrow(data) > 0L
            },
            logical(1)
          )
        )
      },
      logical(1)
    )
  ]
}


site_detail_available_areas <- function(
    site_reports,
    year = NULL,
    site = NULL
) {

  if (
    is.null(site_reports) ||
      !is.list(site_reports)
  ) {
    return(character())
  }

  datasets <- c(
    Visits = "visits",
    Memberships = "memberships",
    Enrolments = "enrollments"
  )

  available <- vapply(
    datasets,
    function(name) {

      data <- site_detail_filter_rows(
        site_reports[[name]],
        year = year,
        site = site
      )

      is.data.frame(data) &&
        nrow(data) > 0L
    },
    logical(1)
  )

  names(datasets)[available]
}

# -------------------------------------------------------------------------
# Site Detail visit calculations
# -------------------------------------------------------------------------

site_detail_sum_available <- function(values) {

  values <- suppressWarnings(
    as.numeric(values)
  )

  values <- values[
    is.finite(values)
  ]

  if (length(values) == 0L) {
    return(NA_real_)
  }

  sum(values)
}


site_detail_visit_value <- function(
    visits,
    visit_type,
    audience
) {

  if (
    is.null(visits) ||
      !is.data.frame(visits) ||
      nrow(visits) == 0L
  ) {
    return(NA_real_)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "visit_type",
    "audience",
    "value"
  )

  if (!all(required_columns %in% names(visits))) {
    return(NA_real_)
  }

  base_keep <-
    visits$row_type == "Total" &
    visits$visit_type == visit_type &
    visits$audience == audience

  ytd_values <- visits$value[
    base_keep &
      visits$period_type == "YTD"
  ]

  ytd_total <- site_detail_sum_available(
    ytd_values
  )

  if (is.finite(ytd_total)) {
    return(ytd_total)
  }

  monthly_values <- visits$value[
    base_keep &
      visits$period_type == "Month"
  ]

  site_detail_sum_available(
    monthly_values
  )
}


site_detail_visit_kpis <- function(visits) {

  direct_total <- site_detail_visit_value(
    visits,
    visit_type = "Direct",
    audience = "Total"
  )

  indirect_total <- site_detail_visit_value(
    visits,
    visit_type = "Indirect",
    audience = "Total"
  )

  combined_value <- function(audience) {

    direct <- site_detail_visit_value(
      visits,
      visit_type = "Direct",
      audience = audience
    )

    indirect <- site_detail_visit_value(
      visits,
      visit_type = "Indirect",
      audience = audience
    )

    if (
      !is.finite(direct) ||
        !is.finite(indirect)
    ) {
      return(NA_real_)
    }

    direct + indirect
  }

  total_visits <- if (
    is.finite(direct_total) &&
      is.finite(indirect_total)
  ) {
    direct_total + indirect_total
  } else {
    NA_real_
  }

  c(
    total = total_visits,
    direct = direct_total,
    indirect = indirect_total,
    student = combined_value("Student"),
    staff = combined_value("Staff"),
    community = combined_value("Community")
  )
}


site_detail_format_visit_count <- function(value) {

  if (
    length(value) == 0L ||
      !is.finite(value[[1L]])
  ) {
    return("—")
  }

  format(
    round(value[[1L]]),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

# -------------------------------------------------------------------------
# Monthly visit trend data
# -------------------------------------------------------------------------

site_detail_monthly_visit_totals <- function(visits) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    direct = double(),
    indirect = double(),
    total = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(visits) ||
      !is.data.frame(visits) ||
      nrow(visits) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "period_type",
    "row_type",
    "visit_type",
    "audience",
    "value"
  )

  if (!all(required_columns %in% names(visits))) {
    return(empty_result)
  }

  keep <-
    visits$period_type == "Month" &
    visits$row_type == "Total" &
    visits$audience == "Total" &
    visits$visit_type %in% c(
      "Direct",
      "Indirect"
    ) &
    !is.na(visits$date)

  monthly <- visits[
    !is.na(keep) & keep,
    c(
      "date",
      "visit_type",
      "value"
    ),
    drop = FALSE
  ]

  if (nrow(monthly) == 0L) {
    return(empty_result)
  }

  monthly$value <- suppressWarnings(
    as.numeric(monthly$value)
  )

  totals <- aggregate(
    value ~ date + visit_type,
    data = monthly,
    FUN = site_detail_sum_available
  )

  totals <- reshape(
    totals,
    idvar = "date",
    timevar = "visit_type",
    direction = "wide"
  )

  if (!"value.Direct" %in% names(totals)) {
    totals$value.Direct <- NA_real_
  }

  if (!"value.Indirect" %in% names(totals)) {
    totals$value.Indirect <- NA_real_
  }

  dates <- as.Date(totals$date)
  order_index <- order(dates)

  direct <- totals$value.Direct[order_index]
  indirect <- totals$value.Indirect[order_index]
  dates <- dates[order_index]

  data.frame(
    date = dates,
    month = as.integer(
      format(dates, "%m")
    ),
    month_label = format(
      dates,
      "%b"
    ),
    direct = direct,
    indirect = indirect,
    total = ifelse(
      is.finite(direct) &
        is.finite(indirect),
      direct + indirect,
      NA_real_
    ),
    stringsAsFactors = FALSE
  )
}

# UI ----------------------------------------------------------------------
# Detailed Analysis -> Site Detail.
# Uses Site Report files to examine one site within one year, month by month.

mod_site_detail_ui <- function(id) {

  ns <- NS(id)

  nav_panel(
    "Site Detail",

    div(
      class = "p-4",

      div(
        class = "page-heading",
        h2("Site Detail"),
        p(
          "Explore month-by-month performance for a selected site using the detailed Site Reports."
        )
      ),

      # Site Detail controls are populated from uploaded Detailed Site Reports.
      div(
        class = "filter-bar",

        div(
          class = "filter-section-title",
          "Participation detail"
        ),

        layout_columns(
          
          selectInput(
            ns("year"),
            "Year",
            choices = character()
          ),

          selectInput(
            ns("site"),
            "Site",
            choices = character()
          ),

          col_widths = c(
            4,
            8
          )
        ),

        uiOutput(
          ns("availability_note")
        )
      ),

      div(
        class = "section-heading-custom",

        h4(
          class = "mb-1",
          "Participation Analysis"
        ),

        p(
          class = "text-secondary mb-2",
          "Choose the participation dataset to explore."
        )
      ),

      uiOutput(
        ns("area_tabs")
      ),

      # Dynamic focus heading confirms the current Year / Site / Area.
      uiOutput(
        ns("focus_heading")
      ),

      # KPI cards change according to the selected analysis area.
      uiOutput(
        ns("kpis")
      ),

      br(),

      # Main chart: the month-by-month trend for the selected analysis area.
      card(
        full_screen = TRUE,

        card_header(
          div(
            div(
              class = "chart-header-title",
              uiOutput(
                ns("main_title"),
                inline = TRUE
              )
            ),

            uiOutput(
              ns("main_subtitle")
            )
          )
        ),

        uiOutput(
          ns("main_content")
        )
      ),

      # Supporting row:
      #   left  = category/budget/membership breakdown
      #   right = qualitative context and monthly notes
      layout_columns(

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                uiOutput(
                  ns("breakdown_title"),
                  inline = TRUE
                )
              ),

              uiOutput(
                ns("breakdown_subtitle")
              )
            )
          ),

          uiOutput(
            ns("breakdown_content")
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Audience Mix"
              ),

              uiOutput(
                ns("audience_subtitle")
              )
            )
          ),

          uiOutput(
            ns("audience_content")
          )
        ),

          div(
            class = "p-3",

            p(
              class = "text-secondary",
              paste(
                "This area can provide context for unusual monthly",
                "movements, events, operational changes and reporting notes."
              )
            ),

            tags$ul(
              tags$li(
                "Monthly highlights from the Site Report"
              ),
              tags$li(
                "Operational or program changes"
              ),
              tags$li(
                "Potential explanations for unusual peaks or drops"
              )
            )
          )
        ),

        col_widths = c(8, 4)
      )
    )
}


# Server ------------------------------------------------------------------
# Updates available analysis areas and switches KPI/chart labels according
# to the selected Site Report content.

mod_site_detail_server <- function(
  id,
  data = NULL
) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # -------------------------------------------------------------------
      # Detailed Site Report data
      # -------------------------------------------------------------------

      site_reports <- reactive({

        if (is.null(data)) {
          return(NULL)
        }

        dashboard_data <- if (
          shiny::is.reactive(data)
        ) {
          data()
        } else {
          data
        }

        if (is.null(dashboard_data)) {
          return(NULL)
        }

        if (
          inherits(
            dashboard_data,
            "business_unit_template_report_data"
          )
        ) {
          return(dashboard_data)
        }

        if (!is.list(dashboard_data)) {
          return(NULL)
        }

        if (
          inherits(
            dashboard_data$site_reports,
            "business_unit_template_report_data"
          )
        ) {
          return(
            dashboard_data$site_reports
          )
        }

        if (
          inherits(
            dashboard_data$sites,
            "business_unit_template_report_data"
          )
        ) {
          return(
            dashboard_data$sites
          )
        }

        NULL
      })

      selected_visits <- reactive({

        reports <- site_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        site_detail_filter_rows(
          reports$visits,
          year = input$year,
          site = input$site
        )
      })


      visit_kpis <- reactive({

        site_detail_visit_kpis(
          selected_visits()
        )
      })

            monthly_visits <- reactive({

        site_detail_monthly_visit_totals(
          selected_visits()
        )
      })

        visit_type_breakdown <- reactive({

        values <- visit_kpis()

        visit_values <- c(
          Direct = values[["direct"]],
          Indirect = values[["indirect"]]
        )

        total <- sum(
          visit_values[
            is.finite(visit_values)
          ]
        )

        shares <- if (
          is.finite(total) &&
            total > 0
        ) {
          visit_values / total
        } else {
          rep(
            NA_real_,
            length(visit_values)
          )
        }

        data.frame(
          visit_type = names(visit_values),
          value = as.numeric(visit_values),
          share = as.numeric(shares),
          stringsAsFactors = FALSE
        )
      })

      audience_breakdown <- reactive({

        values <- visit_kpis()

        audience_values <- c(
          Student = values[["student"]],
          Staff = values[["staff"]],
          Community = values[["community"]]
        )

        complete <- all(
          is.finite(audience_values)
        )

        total <- if (complete) {
          sum(audience_values)
        } else {
          NA_real_
        }

        shares <- if (
          is.finite(total) &&
            total > 0
        ) {
          audience_values / total
        } else {
          rep(
            NA_real_,
            length(audience_values)
          )
        }

        data.frame(
          audience = names(audience_values),
          value = as.numeric(audience_values),
          share = as.numeric(shares),
          stringsAsFactors = FALSE
        )
      })


      # -------------------------------------------------------------------
      # Available years
      # -------------------------------------------------------------------

      observeEvent(
        site_reports(),
        {

          years <- site_detail_available_years(
            site_reports()
          )

          selected_year <- if (
            length(years) > 0L
          ) {
            as.character(max(years))
          } else {
            character()
          }

          updateSelectInput(
            session,
            "year",
            choices = years,
            selected = selected_year
          )
        },
        ignoreInit = FALSE
      )


      # -------------------------------------------------------------------
      # Available sites for selected year
      # -------------------------------------------------------------------

      observeEvent(
        list(
          site_reports(),
          input$year
        ),
        {

          sites <- site_detail_available_sites(
            site_reports(),
            year = input$year
          )

          selected_site <- if (
            !is.null(input$site) &&
              input$site %in% sites
          ) {
            input$site
          } else if (length(sites) > 0L) {
            sites[[1L]]
          } else {
            character()
          }

          updateSelectInput(
            session,
            "site",
            choices = sites,
            selected = selected_site
          )
        },
        ignoreInit = FALSE
      )

      # -------------------------------------------------------------------
      # Available participation views
      # -------------------------------------------------------------------

      output$area_tabs <- renderUI({

        reports <- site_reports()

        if (is.null(reports)) {
          return(NULL)
        }

        areas <- site_detail_available_areas(
          reports,
          year = input$year,
          site = input$site
        )

        if (length(areas) == 0L) {
          return(NULL)
        }

        current_area <- isolate(
          input$area
        )

        selected_area <- if (
          !is.null(current_area) &&
            current_area %in% areas
        ) {
          current_area
        } else if ("Visits" %in% areas) {
          "Visits"
        } else {
          areas[[1L]]
        }

        tabs <- lapply(
          areas,
          function(area) {

            nav_panel(
              area,
              value = area,
              div()
            )
          }
        )

        do.call(
          navset_pill,
          c(
            list(
              id = ns("area"),
              selected = selected_area
            ),
            tabs
          )
        )
      })

      output$availability_note <- renderUI({

        reports <- site_reports()

        if (is.null(reports)) {
          return(
            tags$small(
              class = "text-secondary",
              "Upload Detailed Site Reports on the Data page to activate this analysis."
            )
          )
        }

        areas <- site_detail_available_areas(
          reports,
          year = input$year,
          site = input$site
        )

        if (length(areas) == 0L) {
          return(
            tags$small(
              class = "text-secondary",
              "No participation data is available for this selection."
            )
          )
        }

        tags$small(
          class = "text-secondary",
          paste0(
            "Available from uploaded reports: ",
            paste(
              areas,
              collapse = ", "
            )
          )
        )
      })

      output$focus_heading <- renderUI({

        req(
          input$year,
          input$site,
          input$area
        )

        div(
          class = "section-heading-custom",

          h4(
            class = "mb-1",
            paste0(
              input$site,
              " · ",
              input$area
            )
          ),

          p(
            class = "mb-0 text-secondary",
            paste(
              input$year,
              "· Participation detail from the selected Detailed Site Report."
            )
          )
        )
      })

      # Defines the KPI layout for each analysis area.
      # Values are placeholders; only the structure changes at this stage.

      kpi_specs <- reactive({

        req(input$area)

        switch(
          input$area,

          "Visits" = {

            values <- visit_kpis()

            list(
              list(
                title = "Total Visits",
                value = site_detail_format_visit_count(
                  values[["total"]]
                ),
                subtitle = "Direct + indirect",
                icon = "people"
              ),
              list(
                title = "Direct Visits",
                value = site_detail_format_visit_count(
                  values[["direct"]]
                ),
                subtitle = "Selected year",
                icon = "arrow-right-circle"
              ),
              list(
                title = "Indirect Visits",
                value = site_detail_format_visit_count(
                  values[["indirect"]]
                ),
                subtitle = "Selected year",
                icon = "diagram-3"
              ),
              list(
                title = "Student Visits",
                value = site_detail_format_visit_count(
                  values[["student"]]
                ),
                subtitle = "Direct + indirect",
                icon = "mortarboard"
              ),
              list(
                title = "Staff Visits",
                value = site_detail_format_visit_count(
                  values[["staff"]]
                ),
                subtitle = "Direct + indirect",
                icon = "person-badge"
              ),
              list(
                title = "Community Visits",
                value = site_detail_format_visit_count(
                  values[["community"]]
                ),
                subtitle = "Direct + indirect",
                icon = "people-fill"
              )
            )
          },

          "Memberships" = list(
            list(
              title = "Memberships",
              value = "—",
              subtitle = "Latest available period",
              icon = "people"
            ),
            list(
              title = "Count Measures",
              value = "—",
              subtitle = "Available membership measures",
              icon = "bar-chart"
            ),
            list(
              title = "Reporting Months",
              value = "—",
              subtitle = "Available periods",
              icon = "calendar3"
            )
          ),

          "Enrolments" = list(
            list(
              title = "Enrolments",
              value = "—",
              subtitle = "Selected year",
              icon = "clipboard-data"
            ),
            list(
              title = "Available Measures",
              value = "—",
              subtitle = "Enrolment measures",
              icon = "bar-chart"
            ),
            list(
              title = "Reporting Months",
              value = "—",
              subtitle = "Available periods",
              icon = "calendar3"
            )
          ),

          NULL
        )
      })

      output$kpis <- renderUI({

        boxes <- kpi_specs()

        if (is.null(boxes)) {
          return(NULL)
        }

        make_value_box_row(
          boxes,
          width = 1 / 3
        )
      })

      # Centralises chart titles/placeholders so UI text stays consistent
      # when the user switches analysis area.
      titles <- reactive({

        req(input$area)

        switch(
          input$area,

          "Visits" = list(
            main = "Visits Over Time",
            breakdown = "Direct vs Indirect Visits",
            main_placeholder =
              "Monthly Direct and Indirect Visits",
            breakdown_placeholder =
              "Annual visit-type breakdown"
          ),

          "Memberships" = list(
            main = "Memberships Over Time",
            breakdown = "Membership Breakdown",
            main_placeholder =
              "Monthly membership measures",
            breakdown_placeholder =
              "Membership counts and shares"
          ),

          "Enrolments" = list(
            main = "Enrolments Over Time",
            breakdown = "Enrolment Breakdown",
            main_placeholder =
              "Monthly enrolment measures",
            breakdown_placeholder =
              "Enrolment measure breakdown"
          )
        )
      })

      output$main_title <- renderUI({
        titles()$main
      })

      output$main_subtitle <- renderUI({

        req(
          input$year,
          input$site
        )

        chart_subtitle(
          paste0(
            input$site,
            " · ",
            input$year
          )
        )
      })

      output$main_content <- renderUI({

        if (identical(input$area, "Visits")) {
          return(
            plotOutput(
              ns("visits_over_time"),
              height = "440px"
            )
          )
        }

        placeholder_chart(
          titles()$main_placeholder
        )
      })


      output$visits_over_time <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Visits"
            )
          )

          monthly <- monthly_visits()

          validate(
            need(
              is.data.frame(monthly) &&
                nrow(monthly) > 0L,
              "No monthly visit values are available for this selection."
            )
          )

          plot_site_detail_visits_over_time(
            monthly
          )
        },
        res = 96,
        bg = "white"
      )

      output$breakdown_title <- renderUI({
        titles()$breakdown
      })

      output$breakdown_subtitle <- renderUI({

        req(input$area)

        chart_subtitle(
          paste0(
            "Detailed view · ",
            input$area
          )
        )
      })

      output$breakdown_content <- renderUI({

        if (identical(input$area, "Visits")) {
          return(
            plotOutput(
              ns("visit_type_breakdown"),
              height = "360px"
            )
          )
        }

        placeholder_chart(
          titles()$breakdown_placeholder
        )
      })


      output$visit_type_breakdown <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Visits"
            )
          )

          breakdown <- visit_type_breakdown()

          validate(
            need(
              is.data.frame(breakdown) &&
                nrow(breakdown) > 0L &&
                any(is.finite(breakdown$value)),
              "No visit breakdown values are available for this selection."
            )
          )

          plot_site_detail_visit_type_breakdown(
            breakdown
          )
        },
        res = 96,
        bg = "white"
      )

      output$audience_subtitle <- renderUI({

        req(
          input$year,
          input$site
        )

        chart_subtitle(
          paste0(
            "Student, staff and community · ",
            input$year
          )
        )
      })


      output$audience_content <- renderUI({

        if (identical(input$area, "Visits")) {
          return(
            plotOutput(
              ns("audience_mix"),
              height = "360px"
            )
          )
        }

        placeholder_chart(
          "Audience mix is available for Visits"
        )
      })


      output$audience_mix <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Visits"
            )
          )

          audience <- audience_breakdown()

          validate(
            need(
              is.data.frame(audience) &&
                nrow(audience) == 3L &&
                all(is.finite(audience$value)) &&
                all(is.finite(audience$share)),
              "Complete audience values are not available for this selection."
            )
          )

          plot_site_detail_audience_mix(
            audience
          )
        },
        res = 96,
        bg = "white"
      )

      invisible(
        list(
          year = reactive(input$year),
          site = reactive(input$site),
          area = reactive(input$area),
          selected_visits = selected_visits,
          visit_kpis = visit_kpis,
          monthly_visits = monthly_visits,
          visit_type_breakdown = visit_type_breakdown,
          audience_breakdown = audience_breakdown
        )
      )
    }
  )
}
