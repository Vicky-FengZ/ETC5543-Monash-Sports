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

      # The first implementation establishes the presentation workspace.
      # Dashboard calculations and interactive visual selection are added
      # in the following stages.

    }
  )
}