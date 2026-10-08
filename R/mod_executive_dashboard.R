# -------------------------------------------------------------------------
# Executive Dashboard module
# -------------------------------------------------------------------------

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

                  selectInput(
                    ns("year"),
                    "Year",
                    choices = APP_YEARS,
                    selected = LATEST_APP_YEAR
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
                      class = "btn-primary"
                    )
                  ),

                  col_widths = c(2, 2, 3, 5)
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
                      "—"
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
                      "—"
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
                      "—"
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
                      "—"
                    )
                  ),

                  div(
                    class = "executive-kpi-icon",
                    bs_icon("bullseye")
                  )
                )
              ),

              # Responsive visual board -----------------------------------
              div(
                class = "executive-dashboard-grid",

                card(
                  full_screen = TRUE,
                  card_header("Income vs Expenses"),
                  placeholder_chart(
                    "Monthly Income and Expenses"
                  )
                ),

                card(
                  full_screen = TRUE,
                  card_header("Annual Performance Trend"),
                  placeholder_chart(
                    "Performance across reporting years"
                  )
                ),

                card(
                  full_screen = TRUE,
                  card_header("Budget vs Actual"),
                  placeholder_chart(
                    "Budget and actual performance"
                  )
                ),

                card(
                  full_screen = TRUE,
                  card_header("Site Ranking"),
                  placeholder_chart(
                    "Performance across sites"
                  )
                ),

                card(
                  full_screen = TRUE,
                  card_header("Revenue Opportunity"),
                  placeholder_chart(
                    "Actual and potential revenue"
                  )
                ),

                card(
                  full_screen = TRUE,
                  card_header("Discount Mix"),
                  placeholder_chart(
                    "Student, staff and other discounts"
                  )
                ),

                card(
                  full_screen = TRUE,
                  card_header("Revenue Capture"),
                  placeholder_chart(
                    "Share of potential revenue captured"
                  )
                )
              ),

              # Bottom analysis area --------------------------------------
              div(
                class = "executive-dashboard-bottom-grid",

                # Site values table ---------------------------------------
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
                    "Exact site values will be connected after the dashboard shell is complete"
                  )
                ),
                
                # Key insights --------------------------------------------
                card(
                  class = "executive-insights-card",

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

            available_choices <-
              stats::setNames(
                available_rows$id,
                available_rows$title
              )


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
                        "Choose the charts to include in the",
                        "Executive Dashboard.",
                        "Visuals are grouped by their data source."
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
            picker_registry
        )
      )
    }
  )
}