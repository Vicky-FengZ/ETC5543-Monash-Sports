# -------------------------------------------------------------------------
# Venues Operations module
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Venues Operations data helpers
# -------------------------------------------------------------------------

venues_operations_filter_year <- function(
    data,
    year = NULL
) {

  if (
    is.null(data) ||
      !is.data.frame(data) ||
      nrow(data) == 0L
  ) {
    return(data)
  }

  if (
    is.null(year) ||
      length(year) == 0L ||
      !"report_year" %in% names(data)
  ) {
    return(data)
  }

  selected_year <- suppressWarnings(
    as.integer(year[[1L]])
  )

  if (is.na(selected_year)) {
    return(data)
  }

  data[
    !is.na(data$report_year) &
      data$report_year == selected_year,
    ,
    drop = FALSE
  ]
}


venues_operations_available_years <- function(
    venues_reports
) {

  if (
    is.null(venues_reports) ||
      !is.list(venues_reports) ||
      is.null(venues_reports$attendance) ||
      !is.data.frame(venues_reports$attendance)
  ) {
    return(integer())
  }

  years <- suppressWarnings(
    as.integer(
      venues_reports$attendance$report_year
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

# -------------------------------------------------------------------------
# Attendance calculations
# -------------------------------------------------------------------------

venues_operations_monthly_attendance <- function(attendance) {

  empty_result <- data.frame(
    date = as.Date(character()),
    month = integer(),
    month_label = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(attendance) ||
      !is.data.frame(attendance) ||
      nrow(attendance) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "date",
    "month",
    "month_label",
    "period_type",
    "row_type",
    "value"
  )

  if (!all(required_columns %in% names(attendance))) {
    return(empty_result)
  }

  rows <- attendance[
    attendance$period_type == "Month" &
      attendance$row_type == "Overall Total" &
      !is.na(attendance$date),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows$value <- suppressWarnings(
    as.numeric(rows$value)
  )

  rows <- rows[
    is.finite(rows$value),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  rows <- rows[
    order(rows$date),
    ,
    drop = FALSE
  ]

  data.frame(
    date = as.Date(rows$date),
    month = as.integer(rows$month),
    month_label = as.character(rows$month_label),
    value = rows$value,
    stringsAsFactors = FALSE
  )
}


venues_operations_attendance_kpis <- function(attendance) {

  monthly <- venues_operations_monthly_attendance(
    attendance
  )

  ytd_value <- NA_real_

  if (
    is.data.frame(attendance) &&
      nrow(attendance) > 0L &&
      all(
        c(
          "period_type",
          "row_type",
          "value"
        ) %in% names(attendance)
      )
  ) {

    ytd_rows <- attendance[
      attendance$period_type == "YTD" &
        attendance$row_type == "Overall Total",
      ,
      drop = FALSE
    ]

    ytd_values <- suppressWarnings(
      as.numeric(ytd_rows$value)
    )

    ytd_values <- ytd_values[
      is.finite(ytd_values)
    ]

    if (length(ytd_values) == 1L) {
      ytd_value <- ytd_values[[1L]]
    }
  }

  total_attendance <- if (
    is.finite(ytd_value)
  ) {
    ytd_value
  } else if (nrow(monthly) > 0L) {
    sum(monthly$value)
  } else {
    NA_real_
  }

  average_monthly <- if (
    nrow(monthly) > 0L
  ) {
    mean(monthly$value)
  } else {
    NA_real_
  }

  if (nrow(monthly) > 0L) {

    peak_index <- which.max(
      monthly$value
    )

    peak_value <- monthly$value[
      peak_index
    ]

    peak_month <- monthly$month_label[
      peak_index
    ]

  } else {

    peak_value <- NA_real_
    peak_month <- NA_character_
  }

  list(
    total_attendance = total_attendance,
    average_monthly = average_monthly,
    peak_value = peak_value,
    peak_month = peak_month,
    reporting_months = nrow(monthly)
  )
}


venues_operations_format_count <- function(value) {

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

# UI ----------------------------------------------------------------------
# Detailed Analysis -> Venues Operations.
# This page is separated from Site Detail because Tracking Reports contain
# much more operational and category-specific information.

mod_venues_operations_ui <- function(id) {

  ns <- NS(id)

  nav_panel(
    "Venues Operations",

    div(
      class = "p-4",

      div(
        class = "page-heading",
        h2("Venues Operations"),
        p(
          "Explore detailed operational patterns from the Venues Tracking Reports."
        )
      ),

      # Choose the Tracking Report year and the operational topic to inspect.
      div(
        class = "filter-bar",

        div(
          class = "filter-section-title",
          "Operational detail"
        ),

        layout_columns(

          selectInput(
            ns("year"),
            "Year",
            choices = character()
          ),

          selectInput(
            ns("area"),
            "Operational area",
            choices = character()
          ),

          col_widths = c(3, 5)
        ),

        uiOutput(
          ns("availability_note")
        )
      ),

      # Current operational focus and context-specific KPI summary.
      uiOutput(
        ns("focus_heading")
      ),

      uiOutput(
        ns("kpis")
      ),

      br(),

      # Main view: month-by-month trend for the selected operational area.
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

        plotOutput(
          ns("attendance_over_time"),
          height = "440px"
        )
      ),

      # Supporting views: one category breakdown plus one secondary analysis.
      layout_columns(

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Operational Breakdown"
              ),

              uiOutput(
                ns("breakdown_subtitle")
              )
            )
          ),

          placeholder_chart(
            "Break down the selected operational area into its main categories"
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Secondary Analysis"
              ),

              chart_subtitle(
                "Context-specific analysis will appear here"
              )
            )
          ),

          placeholder_chart(
            "Additional operational detail"
          )
        ),

        col_widths = c(7, 5)
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Changes available operational areas by year and prepares the labels/KPIs
# for the selected Venues Tracking Report topic.

mod_venues_operations_server <- function(
  id,
  data = NULL
) {

  moduleServer(
    id,
    function(input, output, session) {

      # -------------------------------------------------------------------
      # Venues Tracking Report data
      # -------------------------------------------------------------------

      venues_reports <- reactive({

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
            "venues_tracking_report_data"
          )
        ) {
          return(dashboard_data)
        }

        if (!is.list(dashboard_data)) {
          return(NULL)
        }

        if (
          inherits(
            dashboard_data$venues_reports,
            "venues_tracking_report_data"
          )
        ) {
          return(
            dashboard_data$venues_reports
          )
        }

        if (
          inherits(
            dashboard_data$venues,
            "venues_tracking_report_data"
          )
        ) {
          return(
            dashboard_data$venues
          )
        }

        NULL
      })


      selected_attendance <- reactive({

        reports <- venues_reports()

        if (
          is.null(reports) ||
            !is.list(reports)
        ) {
          return(NULL)
        }

        venues_operations_filter_year(
          reports$attendance,
          year = input$year
        )
      })

      attendance_kpis <- reactive({

        venues_operations_attendance_kpis(
          selected_attendance()
        )
      })

      monthly_attendance <- reactive({

        venues_operations_monthly_attendance(
          selected_attendance()
        )
      })

      # -------------------------------------------------------------------
      # Available years
      # -------------------------------------------------------------------

      observeEvent(
        venues_reports(),
        {

          years <- venues_operations_available_years(
            venues_reports()
          )

          selected_year <- if (
            length(years) > 0L
          ) {
            as.character(
              max(years)
            )
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
      # Available operational areas
      # -------------------------------------------------------------------

      observeEvent(
        list(
          venues_reports(),
          input$year
        ),
        {

          attendance <- selected_attendance()

          areas <- if (
            is.data.frame(attendance) &&
              nrow(attendance) > 0L
          ) {
            "Attendance"
          } else {
            character()
          }

          updateSelectInput(
            session,
            "area",
            choices = areas,
            selected = if (
              length(areas) > 0L
            ) {
              areas[[1L]]
            } else {
              character()
            }
          )
        },
        ignoreInit = FALSE
      )

      output$availability_note <- renderUI({

        reports <- venues_reports()

        if (is.null(reports)) {
          return(
            tags$small(
              class = "text-secondary",
              "Upload Venues Tracking Reports on the Data page to activate this analysis."
            )
          )
        }

        attendance <- selected_attendance()

        if (
          !is.data.frame(attendance) ||
            nrow(attendance) == 0L
        ) {
          return(
            tags$small(
              class = "text-secondary",
              "No Attendance data is available for this year."
            )
          )
        }

        tags$small(
          class = "text-secondary",
          "Available from uploaded reports: Attendance"
        )
      })

      output$focus_heading <- renderUI({

        req(
          input$year,
          input$area
        )

        div(
          class = "snapshot-heading",

          h4(
            paste0(
              input$year,
              " · ",
              input$area
            )
          ),

          p(
            "Operational detail from the selected Venues Tracking Report."
          )
          
        )
      })

      # KPI definitions differ by operational area because each topic has
      # different business measures.
      kpi_specs <- reactive({

        req(input$area)

        if (!identical(input$area, "Attendance")) {
          return(NULL)
        }

        values <- attendance_kpis()

        peak_month <- if (
          is.character(values$peak_month) &&
            length(values$peak_month) > 0L &&
            !is.na(values$peak_month[[1L]])
        ) {
          values$peak_month[[1L]]
        } else {
          "No reported month"
        }

        list(
          list(
            title = "Total Attendance",
            value = venues_operations_format_count(
              values$total_attendance
            ),
            subtitle = "Selected year",
            icon = "people"
          ),
          list(
            title = "Average / Month",
            value = venues_operations_format_count(
              values$average_monthly
            ),
            subtitle = paste0(
              values$reporting_months,
              " reporting months"
            ),
            icon = "calendar3"
          ),
          list(
            title = "Peak Month",
            value = venues_operations_format_count(
              values$peak_value
            ),
            subtitle = peak_month,
            icon = "bar-chart-line"
          )
        )
      })

      output$kpis <- renderUI({
        make_value_box_row(
          kpi_specs(),
          width = 1 / 3
        )
      })

      output$main_title <- renderUI({

        req(input$area)

        if (identical(input$area, "Attendance")) {
          return("Attendance Over Time")
        }

        "Operational Trend"
      })

      output$main_subtitle <- renderUI({

        req(
          input$year,
          input$area
        )

        chart_subtitle(
          paste0(
            input$area,
            " · January–December ",
            input$year
          )
        )
      })

      output$attendance_over_time <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Attendance"
            )
          )

          monthly <- monthly_attendance()

          validate(
            need(
              is.data.frame(monthly) &&
                nrow(monthly) > 0L &&
                any(is.finite(monthly$value)),
              "No monthly attendance values are available for this selection."
            )
          )

          plot_venues_attendance_over_time(
            monthly
          )
        },
        res = 96,
        bg = "white"
      )

      output$breakdown_subtitle <- renderUI({

        req(input$area)

        chart_subtitle(
          paste0(
            "Category-level view · ",
            input$area
          )
        )
      })

      invisible(
        list(
          year = reactive(input$year),
          area = reactive(input$area)
        )
      )
    }
  )
}
