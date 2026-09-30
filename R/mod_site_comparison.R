# -------------------------------------------------------------------------
# Site Comparison module
# -------------------------------------------------------------------------

# UI ----------------------------------------------------------------------
# Organises the page from broad comparison to drill-down:
# Site trend -> latest-year site snapshot -> facility-level analysis.

mod_site_comparison_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    "Site Comparison",
    icon = bs_icon("buildings"),
    
    div(
      class = "p-4",
      
      div(
        class = "page-heading",
        h2("Site & Facility Comparison"),
        p(
          "Compare how sites perform across years and explore the drivers behind site-level differences."
        )
      ),
      
      # Page-level controls shared by all site and facility views.
      div(
        class = "filter-bar",
        
        div(
          class = "filter-section-title",
          "Comparison period"
        ),
        
        layout_columns(
          
          selectInput(
            ns("from_year"),
            "From Year",
            choices = APP_YEARS,
            selected = min(APP_YEARS)
          ),
          
          selectInput(
            ns("to_year"),
            "To Year",
            choices = APP_YEARS,
            selected = max(APP_YEARS)
          ),
          
          selectInput(
            ns("metric"),
            "Metric",
            choices = FINANCE_METRICS,
            selected = "Income"
          ),
          
          col_widths = c(3, 3, 4)
        )
      ),
      
      # Section 1: compare site performance across the selected year range.
      div(
        class = "section-heading-custom",
        
        h4(
          class = "mb-1",
          "Site Performance Across Years"
        ),
        
        uiOutput(
          ns("period_subtitle")
        )
      ),
      
      card(
        full_screen = TRUE,
        
        card_header(
          div(
            div(
              class = "chart-header-title",
              "Site Performance Over Time"
            ),
            
            uiOutput(
              ns("trend_subtitle")
            )
          )
        ),
        
        plotOutput(
          ns("site_performance_trend"),
          height = "520px"
        )
      ),
      
      # Section 2: focus on the latest year (To Year) for ranking and budget.
      div(
        class = "section-heading-custom mt-4",
        
        h4(
          class = "mb-1",
          uiOutput(
            ns("snapshot_title"),
            inline = TRUE
          )
        ),
        
        tags$small(
          class = "text-secondary",
          "A closer look at the latest year in the selected period"
        )
      ),
      
      layout_columns(
        
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              div(
                class = "chart-header-title",
                "Site Ranking"
              ),
              
              uiOutput(
                ns("ranking_subtitle")
              )
            )
          ),
          
          plotOutput(
            ns("site_ranking"),
            height = "480px"
          )
        ),
        
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              div(
                class = "chart-header-title",
                "Budget Performance"
              ),
              
              uiOutput(
                ns("budget_subtitle")
              )
            )
          ),
          
          plotOutput(
            ns("site_budget_performance"),
            height = "480px"
          )
        ),
        
        col_widths = c(5, 7)
      ),

      # Section 3: explain where revenue opportunity sits across sites. These
      # views are intentionally separate from the standard finance metric
      # selector because discounts are drivers rather than accounting results.
      div(
        class = "section-heading-custom mt-4",

        h4(
          class = "mb-1",
          "Site Revenue Opportunity"
        ),

        tags$small(
          class = "text-secondary",
          paste(
            "Compare potential revenue and identify whether student,",
            "staff or other discounts drive the opportunity gap."
          )
        )
      ),

      layout_columns(

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Revenue Opportunity by Site"
              ),

              uiOutput(
                ns("site_opportunity_subtitle")
              )
            )
          ),

          plotOutput(
            ns("site_revenue_opportunity"),
            height = "480px"
          )
        ),

        card(
          full_screen = TRUE,

          card_header(
            div(
              div(
                class = "chart-header-title",
                "Discount Mix by Site"
              ),

              uiOutput(
                ns("site_discount_mix_subtitle")
              )
            )
          ),

          plotOutput(
            ns("site_discount_mix"),
            height = "480px"
          )
        ),

        col_widths = c(7, 5)
      ),
      
      # Section 4: drill down from the selected site to its facilities and
      # operational business units.
      # The site selector sits outside the tab card so the dropdown is never
      # clipped by the card's internal scrolling area.
      div(
        class = "section-heading-custom mt-4",
        
        h4(
          class = "mb-1",
          "Facility Breakdown"
        ),
        
        tags$small(
          class = "text-secondary",
          paste(
            "Choose a site first, then explore financial performance,",
            "discounts and revenue opportunity by facility or business unit."
          )
        )
      ),
      
      # Facility-level control bar -----------------------------------------
      # Keeping this control outside navset_card_tab makes the interaction
      # clearer and avoids the dropdown opening inside a scrollable card.
      div(
        class = "filter-bar",
        
        div(
          class = "filter-section-title",
          "Facility focus"
        ),
        
        layout_columns(
          
          selectInput(
            ns("selected_site"),
            label = "Site",
            choices = SITE_NAMES,
            selected = SITE_NAMES[[1]],
            selectize = FALSE
          ),
          
          div(
            tags$label(
              class = "form-label",
              "Current focus"
            ),
            
            uiOutput(
              ns("facility_focus")
            )
          ),
          
          col_widths = c(4, 8)
        )
      ),
      
      # Facility views -----------------------------------------------------
      # The tabs now contain only analysis content, not input controls.
      navset_card_tab(
        id = ns("facility_view"),
        selected = "Financial Performance",
        title = "Facility Analysis",
        full_screen = TRUE,
        
        nav_panel(
          "Financial Performance",

          layout_columns(

            card(
              card_header(
                div(
                  div(
                    class = "chart-header-title",
                    "Latest-year Facility Ranking"
                  ),

                  uiOutput(
                    ns("facility_ranking_subtitle")
                  )
                )
              ),

              plotOutput(
                ns("facility_ranking"),
                height = "480px"
              )
            ),

            card(
              card_header(
                div(
                  div(
                    class = "chart-header-title",
                    "Facility Performance Over Time"
                  ),

                  uiOutput(
                    ns("facility_trend_subtitle")
                  )
                )
              ),

              placeholder_chart(
                paste(
                  "Compare facility and business-unit performance",
                  "across the selected year range"
                )
              )
            ),

            col_widths = c(6, 6)
          )
        ),
        
        nav_panel(
          "Discounts",

          layout_columns(

            card(
              card_header(
                div(
                  div(
                    class = "chart-header-title",
                    "Latest-year Discount Mix"
                  ),

                  uiOutput(
                    ns("facility_discount_mix_subtitle")
                  )
                )
              ),

              plotOutput(
                ns("facility_discount_mix"),
                height = "480px"
              )
            ),

            card(
              card_header(
                div(
                  div(
                    class = "chart-header-title",
                    "Discount Opportunity Trend"
                  ),

                  uiOutput(
                    ns("facility_discount_trend_subtitle")
                  )
                )
              ),

              plotOutput(
                ns("facility_discount_trend"),
                height = "480px"
              )
            ),

            col_widths = c(6, 6)
          )
        ),

        nav_panel(
          "Revenue Opportunity",

          layout_columns(

            card(
              card_header(
                div(
                  div(
                    class = "chart-header-title",
                    "Actual vs Potential Revenue"
                  ),

                  uiOutput(
                    ns("facility_revenue_gap_subtitle")
                  )
                )
              ),

              placeholder_chart(
                paste(
                  "Compare actual income and discount opportunity",
                  "within the selected site"
                )
              )
            ),

            card(
              card_header(
                div(
                  div(
                    class = "chart-header-title",
                    "Revenue Capture Rate"
                  ),

                  uiOutput(
                    ns("facility_capture_trend_subtitle")
                  )
                )
              ),

              placeholder_chart(
                "Track the share of potential revenue captured over time"
              )
            ),

            col_widths = c(6, 6)
          )
        )
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Maintains the shared comparison period and generates descriptive labels
# for the site-level and facility-level views.

mod_site_comparison_server <- function(
    id,
    data = NULL
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # -------------------------------------------------------------------
      # Monthly Site Report data
      # -------------------------------------------------------------------

      monthly_reports <- reactive({

        if (is.null(data)) {
          return(NULL)
        }

        dashboard_data <- if (shiny::is.reactive(data)) {
          data()
        } else {
          data
        }

        if (inherits(
          dashboard_data,
          "monthly_site_report_data"
        )) {
          return(dashboard_data)
        }

        if (!is.list(dashboard_data)) {
          return(NULL)
        }

        if (!is.null(
          dashboard_data$monthly_reports
        )) {
          return(
            dashboard_data$monthly_reports
          )
        }

        dashboard_data$monthly
      })

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

        if (inherits(
          dashboard_data,
          "business_unit_template_report_data"
        )) {
          return(
            dashboard_data
          )
        }

        if (!is.list(
          dashboard_data
        )) {
          return(NULL)
        }

        if (!is.null(
          dashboard_data$site_reports
        )) {
          return(
            dashboard_data$site_reports
          )
        }

        dashboard_data$sites
      })

      site_annual_finance <- reactive({
        calculate_site_annual_finance(
          monthly_reports()
        )
      })

      site_annual_revenue <- reactive({
        calculate_site_annual_revenue(
          monthly_reports()
        )
      })

      site_annual_discounts <- reactive({
        calculate_site_annual_discounts(
         monthly_reports()
        )
      })

      facility_annual_finance <- reactive({
        calculate_facility_annual_finance(
          site_reports()
        )
      })

      facility_annual_discounts <- reactive({
        calculate_facility_annual_discounts(
          site_reports()
        )

      })

      facility_monthly_discounts <- reactive({
        calculate_facility_monthly_discounts(
          site_reports()
        )
      })

      snapshot_site_discounts <- reactive({
        
        x <- site_annual_discounts()
        selected_period <- period()
        
        x[
          x$report_year == selected_period$to,
          ,
          drop = FALSE
        ]
      })

      available_years <- reactive({

        x <- site_annual_finance()

        if (
          is.null(x) ||
            !is.data.frame(x) ||
            nrow(x) == 0L
        ) {
          return(integer())
        }

        years <- sort(
          unique(
            as.integer(
              x$report_year
            )
          )
        )

        years[
          !is.na(years)
        ]
      })


      duplicate_years <- reactive({
        overview_duplicate_report_years(
          monthly_reports()
        )
      })


      # When files are loaded, set the selectors to the available data range.
      observeEvent(
        available_years(),
        {

          years <- available_years()

          if (length(years) == 0L) {
            return()
          }

          updateSelectInput(
            session,
            "from_year",
            choices = years,
            selected = min(years)
          )

          updateSelectInput(
            session,
            "to_year",
            choices = years,
            selected = max(years)
          )
        },
        ignoreInit = FALSE
      )
      
      # Synchronise the two year selectors so From Year never exceeds To Year.
      observeEvent(
        input$from_year,
        {
          req(input$from_year)
          
          from_year <- as.integer(
            input$from_year
          )
          
                    years <- available_years()

          if (length(years) == 0L) {
            years <- APP_YEARS
          }

          current_to <- if (
            is.null(input$to_year)
          ) {
            max(years)
          } else {
            as.integer(input$to_year)
          }
          
          updateSelectInput(
            session,
            "to_year",
            choices = years[
              years >= from_year
            ],
            selected = max(
              from_year,
              current_to
            )
          )
        },
        ignoreInit = TRUE
      )
      
      observeEvent(
        input$to_year,
        {
          req(input$to_year)
          
          to_year <- as.integer(
            input$to_year
          )
          
                    years <- available_years()

          if (length(years) == 0L) {
            years <- APP_YEARS
          }

          current_from <- if (
            is.null(input$from_year)
          ) {
            min(years)
          } else {
            as.integer(input$from_year)
          }
          
          updateSelectInput(
            session,
            "from_year",
            choices = years[
              years <= to_year
            ],
            selected = min(
              to_year,
              current_from
            )
          )
        },
        ignoreInit = TRUE
      )
      
      # Shared state for every chart on this page.
      period <- reactive({
        
        req(
          input$from_year,
          input$to_year,
          input$metric
        )
        
        from_year <- as.integer(
          input$from_year
        )
        
        to_year <- as.integer(
          input$to_year
        )
        
        list(
          from = from_year,
          to = to_year,
          label = paste0(
            from_year,
            "–",
            to_year
          ),
          metric = input$metric
        )
      })

            filtered_site_finance <- reactive({

        x <- site_annual_finance()
        selected_period <- period()

        x[
          x$report_year >= selected_period$from &
            x$report_year <= selected_period$to,
          ,
          drop = FALSE
        ]
      })

            snapshot_site_finance <- reactive({

        x <- site_annual_finance()
        selected_period <- period()

        x[
          x$report_year == selected_period$to,
          ,
          drop = FALSE
        ]
      })

      snapshot_site_revenue <- reactive({
        x <- site_annual_revenue()
        selected_period <- period()
        
        x[
          x$report_year == selected_period$to,
          ,
          drop = FALSE
        ]
      })

      snapshot_facility_finance <- reactive({

        req(
          input$selected_site
        )

        x <- facility_annual_finance()

        x <- filter_facility_finance_by_site(
          x,
          input$selected_site
        )

        selected_period <- period()

        x[
          x$report_year ==
            selected_period$to,
          ,
          drop = FALSE
        ]
      })

      snapshot_facility_discounts <- reactive({

        req(
          input$selected_site
        )

        x <- facility_annual_discounts()

        x <- filter_facility_discounts_by_site(
          x,
          input$selected_site
        )

        selected_period <- period()

        x[
          x$report_year ==
            selected_period$to,
          ,
          drop = FALSE
        ]
      })

      snapshot_facility_monthly_discounts <- reactive({

        req(
          input$selected_site
        )

        x <- facility_monthly_discounts()

        x <- filter_facility_monthly_discounts_by_site(
          x,
          input$selected_site
        )

        selected_period <- period()

        x[
          x$report_year ==
            selected_period$to,
          ,
          drop = FALSE
        ]
      })
      
      # Site-level labels: trend uses the full range; ranking/budget use To Year.
      output$period_subtitle <- renderUI({
        
        x <- period()
        
        tags$small(
          class = "text-secondary",
          paste0(
            "Compare ",
            tolower(x$metric),
            " across sites over ",
            x$label,
            "."
          )
        )
      })
      
      output$trend_subtitle <- renderUI({
        
        x <- period()
        
        chart_subtitle(
          paste0(
            "Actual ",
            x$metric,
            " by site · ",
            x$label
          )
        )
      })

            output$site_performance_trend <- renderPlot(
        {

          x <- filtered_site_finance()
          reports <- monthly_reports()

          validate(
            need(
              !is.null(reports),
              paste(
                "Upload at least one Monthly Site Report",
                "on the Data page."
              )
            ),
            need(
              length(duplicate_years()) == 0L,
              paste0(
                "More than one Monthly Site Report is active for: ",
                paste(
                  duplicate_years(),
                  collapse = ", "
                ),
                ". Delete the superseded file on the Data page."
              )
            ),
            need(
              nrow(x) > 0L,
              paste(
                "No site finance data is available",
                "for this period."
              )
            )
          )

          metric <- input$metric

          values <- site_finance_values(
            x,
            metric,
            "Actual"
          )

          validate(
            need(
              any(is.finite(values)),
              paste(
                "No Actual",
                metric,
                "values are available for this period."
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
      
      output$snapshot_title <- renderUI({
        
        x <- period()
        
        paste0(
          x$to,
          " Site Snapshot"
        )
      })
      
      output$ranking_subtitle <- renderUI({
        
        x <- period()
        
        chart_subtitle(
          paste0(
            "Actual ",
            x$metric,
            " ranking · ",
            x$to
          )
        )
      })

            output$site_ranking <- renderPlot(
        {

          x <- snapshot_site_finance()
          reports <- monthly_reports()

          validate(
            need(
              !is.null(reports),
              paste(
                "Upload at least one Monthly Site Report",
                "on the Data page."
              )
            ),
            need(
              length(duplicate_years()) == 0L,
              paste0(
                "More than one Monthly Site Report is active for: ",
                paste(
                  duplicate_years(),
                  collapse = ", "
                ),
                ". Delete the superseded file on the Data page."
              )
            ),
            need(
              nrow(x) > 0L,
              paste(
                "No site finance data is available",
                "for the selected year."
              )
            )
          )

          metric <- input$metric

          values <- site_finance_values(
            x,
            metric,
            "Actual"
          )

          validate(
            need(
              any(is.finite(values)),
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
      
      output$budget_subtitle <- renderUI({
        
        x <- period()
        
        chart_subtitle(
          paste0(
            "Budget vs Actual · ",
            x$metric,
            " · ",
            x$to
          )
        )
      })

            output$site_budget_performance <- renderPlot(
        {

          x <- snapshot_site_finance()
          reports <- monthly_reports()

          validate(
            need(
              !is.null(reports),
              paste(
                "Upload at least one Monthly Site Report",
                "on the Data page."
              )
            ),
            need(
              length(duplicate_years()) == 0L,
              paste0(
                "More than one Monthly Site Report is active for: ",
                paste(
                  duplicate_years(),
                  collapse = ", "
                ),
                ". Delete the superseded file on the Data page."
              )
            ),
            need(
              nrow(x) > 0L,
              paste(
                "No site finance data is available",
                "for the selected year."
              )
            )
          )

          metric <- input$metric

          actual <- site_finance_values(
            x,
            metric,
            "Actual"
          )

          budget <- site_finance_values(
            x,
            metric,
            "Budget"
          )

          complete <- is.finite(actual) &
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

      # Revenue opportunity is fixed to the latest selected year and is not
      # controlled by the standard Income / Expenses / Net Result selector.
      
      output$site_opportunity_subtitle <- renderUI({

        x <- period()

        chart_subtitle(
          paste0(
            "Actual income + discount opportunity · ",
            x$to
          )
        )
      })

      output$site_revenue_opportunity <- renderPlot(
        {
          
          x <- snapshot_site_revenue()
          reports <- monthly_reports()
          
          validate(
            need(
              !is.null(reports),
              paste(
                "Upload at least one Monthly Site Report",
                "on the Data page."
              )
            ),
            need(
              length(duplicate_years()) == 0L,
              paste0(
                "More than one Monthly Site Report is active for: ",
                paste(
                  duplicate_years(),
                  collapse = ", "
                ),
                ". Delete the superseded file on the Data page."
              )
            ),
            need(
              nrow(x) > 0L,
              paste(
                "No site revenue opportunity data is available",
                "for the selected year."
              )
            )
          )
          
          complete <- is.finite(
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

      output$site_discount_mix_subtitle <- renderUI({

        x <- period()

        chart_subtitle(
          paste0(
            "Student, staff and other discounts · ",
            x$to
          )
        )
      })

      output$site_discount_mix <- renderPlot(
        {
          
          x <- snapshot_site_discounts()
          reports <- monthly_reports()
          
          validate(
            need(
              !is.null(reports),
              paste(
                "Upload at least one Monthly Site Report",
                "on the Data page."
              )
            ),
            need(
              length(duplicate_years()) == 0L,
              paste0(
                "More than one Monthly Site Report is active for: ",
                paste(
                  duplicate_years(),
                  collapse = ", "
                ),
                ". Delete the superseded file on the Data page."
              )
            ),
            need(
              nrow(x) > 0L,
              paste(
                "No site discount data is available",
                "for the selected year."
              )
            )
          )

          complete <- is.finite(
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
      
      # Facility-level focus stays concise here; each tab gives its own
      # relevant year or period in the subtitle below.
      output$facility_focus <- renderUI({
        
        x <- period()
        req(input$selected_site)
        
        chart_subtitle(
          paste0(
            input$selected_site,
            " · ",
            x$metric,
            " · Facilities and business units"
          )
        )
      })
      
      output$facility_ranking_subtitle <- renderUI({
        
        x <- period()
        req(input$selected_site)
        
        chart_subtitle(
          paste0(
            input$selected_site,
            " · Actual ",
            x$metric,
            " · ",
            x$to,
            " snapshot"
          )
        )
      })

      output$facility_ranking <- renderPlot(
        {

          reports <- site_reports()
          x <- snapshot_facility_finance()

          validate(
            need(
              !is.null(reports),
              paste(
                "Upload the relevant Detailed Site Report",
                "on the Data page."
              )
            ),
            need(
              nrow(x) > 0L,
              paste0(
                "No facility finance data is available for ",
                input$selected_site,
                " in ",
                period()$to,
                "."
              )
            )
          )

          metric <- input$metric

          values <- facility_finance_values(
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
                "values are available for this site and year."
              )
            )
          )

          plot_facility_ranking(
            x,
            metric
          )
        },
        res = 96,
        bg = "white"
      )
      
      output$facility_trend_subtitle <- renderUI({

        req(
          input$selected_site,
          input$metric
        )

        chart_subtitle(
          paste0(
            input$selected_site,
            " · ",
            input$metric,
            " · Multiple Detailed Site Report years required"
          )
        )
      })

      output$facility_discount_mix_subtitle <- renderUI({

        x <- period()
        req(input$selected_site)

        chart_subtitle(
          paste0(
            input$selected_site,
            " · Discount mix · ",
            x$to
          )
        )
      })

      output$facility_discount_mix <- renderPlot(
        {
          reports <- site_reports()
          x <- snapshot_facility_discounts()

          validate(
            need(
              !is.null(reports),
              paste(
                "Upload the relevant Detailed Site Report",
                "on the Data page."
              )
            ),
            need(
              nrow(x) > 0L,
              paste0(
                "No facility discount data is available for ",
                input$selected_site,
                " in ",
                period()$to,
                "."
              )
            )
          )

          complete <- is.finite(
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
                "No complete facility discount mix values",
                "are available for this site and year."
              )
            )
          )

          plot_facility_discount_mix(
            x
          )
        },
        res = 96,
        bg = "white"
      )

      output$facility_discount_trend_subtitle <- renderUI({

        x <- period()
        req(input$selected_site)

        chart_subtitle(
          paste0(
            input$selected_site,
            " · Monthly discount opportunity · ",
            x$to
          )
        )
      })

      output$facility_discount_trend <- renderPlot(
        {

          reports <- site_reports()
          x <- snapshot_facility_monthly_discounts()

          validate(
            need(
              !is.null(reports),
              paste(
                "Upload the relevant Detailed Site Report",
                "on the Data page."
              )
            ),
            need(
              nrow(x) > 0L,
              paste0(
                "No monthly facility discount data is available for ",
                input$selected_site,
                " in ",
                period()$to,
                "."
              )
            )
          )

          complete <- !is.na(
            x$date
          ) &
            is.finite(
              x$total_discount
            )

          validate(
            need(
              any(complete),
              paste(
                "No complete monthly discount opportunity values",
                "are available for this site and year."
              )
            )
          )

          plot_facility_discount_trend(
            x
          )
        },
        res = 96,
        bg = "white"
      )

      output$facility_revenue_gap_subtitle <- renderUI({

        x <- period()
        req(input$selected_site)

        chart_subtitle(
          paste0(
            input$selected_site,
            " · Actual vs potential revenue · ",
            x$to
          )
        )
      })

      output$facility_capture_trend_subtitle <- renderUI({

        x <- period()
        req(input$selected_site)

        chart_subtitle(
          paste0(
            input$selected_site,
            " · Revenue capture rate · ",
            x$label
          )
        )
      })
      
            invisible(
        list(
          period = period,
          selected_site = reactive(
            input$selected_site
          ),
          monthly_reports = monthly_reports,
          available_years = available_years,
          site_annual_finance = site_annual_finance,
          filtered_site_finance = filtered_site_finance,
          snapshot_site_finance = snapshot_site_finance,
          duplicate_years = duplicate_years
        )
      )
    }
  )
}
