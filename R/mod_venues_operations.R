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

# -------------------------------------------------------------------------
# Attendance by activity
# -------------------------------------------------------------------------

venues_operations_attendance_section_totals <- function(attendance) {

  empty_result <- data.frame(
    activity = character(),
    area = character(),
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
    "period_type",
    "row_type",
    "activity",
    "area",
    "value"
  )

  if (!all(required_columns %in% names(attendance))) {
    return(empty_result)
  }

  valid_activity <-
    !is.na(attendance$activity) &
    nzchar(trimws(attendance$activity)) &
    tolower(trimws(attendance$activity)) != "overall"

  select_rows <- function(period_type) {

    rows <- attendance[
      attendance$period_type == period_type &
        attendance$row_type == "Section Total" &
        valid_activity,
      ,
      drop = FALSE
    ]

    rows$value <- suppressWarnings(
      as.numeric(rows$value)
    )

    rows[
      is.finite(rows$value),
      ,
      drop = FALSE
    ]
  }

  # Prefer reported YTD totals for each individual activity.
  rows <- select_rows("YTD")

  # If no YTD section totals exist, sum the monthly section totals.
  if (nrow(rows) == 0L) {
    rows <- select_rows("Month")
  }

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  totals <- aggregate(
    value ~ activity + area,
    data = rows,
    FUN = sum
  )

  totals <- totals[
    order(
      totals$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(totals) <- NULL

  totals
}

venues_operations_attendance_detail_totals <- function(
    attendance,
    activity = NULL
) {

  empty_result <- data.frame(
    category = character(),
    value = double(),
    stringsAsFactors = FALSE
  )

  if (
    is.null(attendance) ||
      !is.data.frame(attendance) ||
      nrow(attendance) == 0L ||
      is.null(activity) ||
      length(activity) == 0L
  ) {
    return(empty_result)
  }

  required_columns <- c(
    "period_type",
    "row_type",
    "activity",
    "category",
    "value"
  )

  if (!all(required_columns %in% names(attendance))) {
    return(empty_result)
  }

  selected_activity <- trimws(
    as.character(activity[[1L]])
  )

  if (
    is.na(selected_activity) ||
      !nzchar(selected_activity)
  ) {
    return(empty_result)
  }

  select_rows <- function(period_type) {

    rows <- attendance[
      attendance$period_type == period_type &
        attendance$row_type == "Detail" &
        !is.na(attendance$activity) &
        trimws(as.character(attendance$activity)) ==
          selected_activity &
        !is.na(attendance$category) &
        nzchar(trimws(as.character(attendance$category))) &
        tolower(trimws(as.character(attendance$category))) !=
          "total",
      ,
      drop = FALSE
    ]

    rows$value <- suppressWarnings(
      as.numeric(rows$value)
    )

    rows[
      is.finite(rows$value),
      ,
      drop = FALSE
    ]
  }

  # Prefer reported YTD detail values.
  rows <- select_rows("YTD")

  # Fall back to monthly detail values if YTD rows are unavailable.
  if (nrow(rows) == 0L) {
    rows <- select_rows("Month")
  }

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  totals <- aggregate(
    value ~ category,
    data = rows,
    FUN = sum
  )

  totals <- totals[
    order(
      totals$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(totals) <- NULL

  totals
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

# -------------------------------------------------------------------------
# Attendance by area
# -------------------------------------------------------------------------

venues_operations_attendance_area_totals <- function(attendance) {

  empty_result <- data.frame(
    area = character(),
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
    "period_type",
    "row_type",
    "area",
    "value"
  )

  if (!all(required_columns %in% names(attendance))) {
    return(empty_result)
  }

  valid_area <-
    !is.na(attendance$area) &
    nzchar(trimws(attendance$area)) &
    attendance$area != "All"

  select_rows <- function(period_type, row_type) {

    rows <- attendance[
      attendance$period_type == period_type &
        attendance$row_type == row_type &
        valid_area,
      ,
      drop = FALSE
    ]

    rows$value <- suppressWarnings(
      as.numeric(rows$value)
    )

    rows[
      is.finite(rows$value),
      ,
      drop = FALSE
    ]
  }

  # Prefer reported annual/YTD activity totals.
  rows <- select_rows(
    "YTD",
    "Summary Total"
  )

  if (nrow(rows) == 0L) {
    rows <- select_rows(
      "YTD",
      "Section Total"
    )
  }

  if (nrow(rows) == 0L) {
    rows <- select_rows(
      "Month",
      "Summary Total"
    )
  }

  if (nrow(rows) == 0L) {
    rows <- select_rows(
      "Month",
      "Section Total"
    )
  }

  if (nrow(rows) == 0L) {
    return(empty_result)
  }

  totals <- aggregate(
    value ~ area,
    data = rows,
    FUN = sum
  )

  totals <- totals[
    order(
      totals$value,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  rownames(totals) <- NULL

  totals
}

# UI ----------------------------------------------------------------------
# Detailed Analysis -> Venues Operations.
# This page is separated from Site Detail because Tracking Reports contain
# much more operational and category-specific information.

mod_venues_operations_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::nav_panel(
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

          layout_columns(

            selectInput(
              ns("attendance_breakdown_area"),
              "Area",
              choices = "All Areas",
              selected = "All Areas",
              width = "100%"
            ),

            selectInput(
              ns("attendance_breakdown_activity"),
              "Activity",
              choices = "All Activities",
              selected = "All Activities",
              width = "100%"
            ),

            col_widths = c(4, 8)
          ),

          plotOutput(
            ns("attendance_by_activity"),
            height = "360px"
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                uiOutput(
                  ns("secondary_title"),
                  inline = TRUE
                )
              ),

              uiOutput(
                ns("secondary_subtitle")
              )
            )
          ),

          plotOutput(
            ns("attendance_by_area"),
            height = "360px"
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

  shiny::moduleServer(
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

      attendance_activity <- reactive({

        venues_operations_attendance_section_totals(
          selected_attendance()
        )
      })

      attendance_activity_filtered <- reactive({

        activity_data <- attendance_activity()

        if (
          !is.data.frame(activity_data) ||
            nrow(activity_data) == 0L
        ) {
          return(activity_data)
        }

        selected_area <- input$attendance_breakdown_area

        if (
          is.null(selected_area) ||
            length(selected_area) == 0L ||
            identical(selected_area, "All Areas")
        ) {
          return(activity_data)
        }

        activity_data[
          activity_data$area == selected_area,
          ,
          drop = FALSE
        ]
      })

      observeEvent(
        attendance_activity_filtered(),
        {

          activity_data <- attendance_activity_filtered()

          activities <- if (
            is.data.frame(activity_data) &&
              nrow(activity_data) > 0L
          ) {
            as.character(activity_data$activity)
          } else {
            character()
          }

          activities <- activities[
            !is.na(activities) &
              nzchar(trimws(activities))
          ]

          choices <- c(
            "All Activities",
            activities
          )

          current_selection <-
            input$attendance_breakdown_activity

          selected <- if (
            !is.null(current_selection) &&
              current_selection %in% choices
          ) {
            current_selection
          } else {
            "All Activities"
          }

          updateSelectInput(
            session,
            "attendance_breakdown_activity",
            choices = choices,
            selected = selected
          )
        },
        ignoreInit = FALSE
      )

      attendance_breakdown_data <- reactive({

        selected_activity <-
          input$attendance_breakdown_activity

        if (
          is.null(selected_activity) ||
            length(selected_activity) == 0L ||
            identical(
              selected_activity,
              "All Activities"
            )
        ) {
          return(
            attendance_activity_filtered()
          )
        }

        detail_data <-
          venues_operations_attendance_detail_totals(
            selected_attendance(),
            activity = selected_activity
          )

        if (
          !is.data.frame(detail_data) ||
            nrow(detail_data) == 0L
        ) {
          return(detail_data)
        }

        data.frame(
          activity = detail_data$category,
          value = detail_data$value,
          stringsAsFactors = FALSE
        )
      })

      observeEvent(
        attendance_activity(),
        {

          activity_data <- attendance_activity()

          areas <- if (
            is.data.frame(activity_data) &&
              nrow(activity_data) > 0L
          ) {
            sort(
              unique(
                as.character(activity_data$area)
              )
            )
          } else {
            character()
          }

          areas <- areas[
            !is.na(areas) &
              nzchar(trimws(areas))
          ]

          choices <- c(
            "All Areas",
            areas
          )

          current_selection <-
            input$attendance_breakdown_area

          selected <- if (
            !is.null(current_selection) &&
              current_selection %in% choices
          ) {
            current_selection
          } else {
            "All Areas"
          }

          updateSelectInput(
            session,
            "attendance_breakdown_area",
            choices = choices,
            selected = selected
          )
        },
        ignoreInit = FALSE
      )

      attendance_area <- reactive({

        venues_operations_attendance_area_totals(
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

      output$breakdown_title <- renderUI({

        req(input$area)

        if (!identical(input$area, "Attendance")) {
          return("Operational Breakdown")
        }

        selected_activity <-
          input$attendance_breakdown_activity

        if (
          !is.null(selected_activity) &&
            length(selected_activity) > 0L &&
            !identical(
              selected_activity,
              "All Activities"
            )
        ) {
          return("Attendance Breakdown")
        }

        "Attendance by Activity"
      })

      output$secondary_title <- renderUI({

        req(input$area)

        if (identical(input$area, "Attendance")) {
          return("Attendance by Area")
        }

        "Secondary Analysis"
      })

      output$breakdown_subtitle <- renderUI({

        req(
          input$year,
          input$area
        )

        if (!identical(input$area, "Attendance")) {
          return(
            chart_subtitle(
              paste0(
                "Category-level view · ",
                input$area
              )
            )
          )
        }

        selected_area <-
          input$attendance_breakdown_area

        selected_activity <-
          input$attendance_breakdown_activity

        if (
          !is.null(selected_activity) &&
            length(selected_activity) > 0L &&
            !identical(
              selected_activity,
              "All Activities"
            )
        ) {

          activity_data <- attendance_activity()

          activity_row <- activity_data[
            activity_data$activity == selected_activity,
            ,
            drop = FALSE
          ]

          activity_total <- if (
            nrow(activity_row) > 0L
          ) {
            sum(
              activity_row$value,
              na.rm = TRUE
            )
          } else {
            NA_real_
          }

          activity_area <- if (
            nrow(activity_row) > 0L
          ) {
            as.character(
              activity_row$area[[1L]]
            )
          } else {
            selected_area
          }

          return(
            chart_subtitle(
              paste0(
                selected_activity,
                " · ",
                activity_area,
                " · Total Attendance ",
                venues_operations_format_count(
                  activity_total
                ),
                " · ",
                input$year
              )
            )
          )
        }

        area_label <- if (
          is.null(selected_area) ||
            length(selected_area) == 0L ||
            identical(
              selected_area,
              "All Areas"
            )
        ) {
          "All Areas"
        } else {
          selected_area
        }

        chart_subtitle(
          paste0(
            "Annual attendance by activity · ",
            area_label,
            " · ",
            input$year
          )
        )
      })

      output$secondary_subtitle <- renderUI({

        req(
          input$year,
          input$area
        )

        if (identical(input$area, "Attendance")) {
          return(
            chart_subtitle(
              paste0(
                "Indoor, Outdoor and Sport Club · ",
                input$year
              )
            )
          )
        }

        chart_subtitle(
          paste0(
            "Additional analysis · ",
            input$area
          )
        )
      })

      output$attendance_by_activity <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Attendance"
            )
          )

          activity_data <- attendance_breakdown_data()

          validate(
            need(
              is.data.frame(activity_data) &&
                nrow(activity_data) > 0L &&
                any(is.finite(activity_data$value)),
              "No activity attendance values are available for this selection."
            )
          )

          plot_venues_attendance_by_activity(
            activity_data
          )
        },
        res = 96,
        bg = "white"
      )

      output$attendance_by_area <- renderPlot(
        {

          req(
            identical(
              input$area,
              "Attendance"
            )
          )

          area_data <- attendance_area()

          validate(
            need(
              is.data.frame(area_data) &&
                nrow(area_data) > 0L &&
                any(is.finite(area_data$value)),
              "No area attendance values are available for this selection."
            )
          )

          plot_venues_attendance_by_area(
            area_data
          )
        },
        res = 96,
        bg = "white"
      )

      invisible(
        list(
          year = reactive(input$year),
          area = reactive(input$area)
        )
      )
    }
  )
}
