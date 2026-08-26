# -------------------------------------------------------------------------
# Site Comparison module
# -------------------------------------------------------------------------

# UI ----------------------------------------------------------------------
# Organises the page from broad comparison to drill-down:
# Site trend -> latest-year site snapshot -> facility-level analysis.

mod_site_comparison_ui <- function(id) {
  
  ns <- NS(id)
  
  nav_panel(
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
        
        placeholder_chart(
          "Compare actual performance across sites year by year"
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
          
          placeholder_chart(
            "Actual performance ranking by site"
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
          
          placeholder_chart(
            "Compare budget and actual performance across sites"
          )
        ),
        
        col_widths = c(5, 7)
      ),
      
      # Section 3: drill down from the selected site to its facilities.
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
            "Choose a site first, then switch between",
            "the latest-year ranking and historical trend."
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
        selected = "Latest-year ranking",
        title = "Facility Analysis",
        full_screen = TRUE,
        
        nav_panel(
          "Latest-year ranking",
          
          div(
            class = "mb-2",
            
            div(
              class = "chart-header-title",
              "Latest-year Facility Ranking"
            ),
            
            uiOutput(
              ns("facility_ranking_subtitle")
            )
          ),
          
          placeholder_chart(
            "Rank facilities within the selected site in the latest selected year"
          )
        ),
        
        nav_panel(
          "Trend over time",
          
          div(
            class = "mb-2",
            
            div(
              class = "chart-header-title",
              "Facility Performance Over Time"
            ),
            
            uiOutput(
              ns("facility_trend_subtitle")
            )
          ),
          
          placeholder_chart(
            "Compare facility performance across the selected year range"
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
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Synchronise the two year selectors so From Year never exceeds To Year.
      observeEvent(
        input$from_year,
        {
          req(input$from_year)
          
          from_year <- as.integer(
            input$from_year
          )
          
          current_to <- if (
            is.null(input$to_year)
          ) {
            max(APP_YEARS)
          } else {
            as.integer(input$to_year)
          }
          
          updateSelectInput(
            session,
            "to_year",
            choices = APP_YEARS[
              APP_YEARS >= from_year
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
          
          current_from <- if (
            is.null(input$from_year)
          ) {
            min(APP_YEARS)
          } else {
            as.integer(input$from_year)
          }
          
          updateSelectInput(
            session,
            "from_year",
            choices = APP_YEARS[
              APP_YEARS <= to_year
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
      
      # Facility-level focus stays concise here; each tab gives its own
      # relevant year or period in the subtitle below.
      output$facility_focus <- renderUI({
        
        x <- period()
        req(input$selected_site)
        
        chart_subtitle(
          paste0(
            input$selected_site,
            " · ",
            x$metric
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
      
      output$facility_trend_subtitle <- renderUI({
        
        x <- period()
        req(input$selected_site)
        
        chart_subtitle(
          paste0(
            input$selected_site,
            " · Actual ",
            x$metric,
            " · ",
            x$label
          )
        )
      })
      
      invisible(
        list(
          period = period,
          selected_site = reactive(
            input$selected_site
          )
        )
      )
    }
  )
}