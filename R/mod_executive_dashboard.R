# -------------------------------------------------------------------------
# Executive Dashboard module
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Executive Dashboard visual UI helpers
# -------------------------------------------------------------------------

executive_visual_context_label <- function(
    view_mode,
    year,
    site
) {

  parts <- character()

  if (
    !is.null(view_mode) &&
      length(view_mode) > 0L &&
      !is.na(view_mode[[1L]]) &&
      nzchar(as.character(view_mode[[1L]]))
  ) {
    parts <- c(
      parts,
      as.character(view_mode[[1L]])
    )
  }

  if (
    !is.null(year) &&
      length(year) > 0L &&
      !is.na(year[[1L]]) &&
      nzchar(as.character(year[[1L]]))
  ) {
    parts <- c(
      parts,
      as.character(year[[1L]])
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
      as.character(site[[1L]])
    )
  }

  paste(
    parts,
    collapse = " · "
  )
}


executive_visual_card_ui <- function(
    visual_id,
    context_label = NULL
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

  card(
    class = paste(
      "executive-visual-card",
      paste0(
        "executive-visual-size-",
        visual$preferred_size
      )
    ),

    full_screen = TRUE,

    card_header(
      div(
        div(
          class = "chart-header-title",
          visual$title
        ),

        subtitle_ui
      )
    ),

    placeholder_chart(
      visual$title,
      paste0(
        source_label,
        " · Chart data will be connected in the next stage"
      )
    )
  )
}

executive_visual_grid_ui <- function(
    visual_ids,
    context_label = NULL
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

          executive_visual_card_ui(
            visual_id = visual_id,
            context_label = context_label
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
      # Dynamic Finance visual grid
      # -------------------------------------------------------------------

      output$finance_visual_grid <- renderUI({

        context_label <-
          executive_visual_context_label(
            view_mode = input$view_mode,
            year = input$year,
            site = input$site
          )

        executive_visual_grid_ui(
          visual_ids =
            selected_visuals$Finance,
          context_label =
            context_label
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
            picker_registry
        )
      )
    }
  )
}