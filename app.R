library(shiny)
library(bslib)
library(bsicons)

# Theme  ----

theme <- bs_theme(
  version = 5,
  bg = "#F6F8FB",
  fg = "#1F2937",
  primary = "#243B53",
  secondary = "#64748B"
)


# Helper: placeholder chart ----

placeholder_chart <- function(text = "Chart placeholder") {
  
  div(
    class = "chart-placeholder",
    bs_icon("bar-chart-line", size = "32px"),
    div(class = "placeholder-title", text),
    div(
      class = "placeholder-subtitle",
      "Real data will be connected in the next stage"
    )
  )
  
}


# UI  ----

ui <- page_navbar(
  
  title = div(
    class = "brand-title",
    "Monash Sport",
    span("Performance Dashboard")
  ),
  
  theme = theme,
  
  window_title = "Monash Sport Performance Dashboard",
  
  header = tags$head(
    
    tags$style(HTML("

      body {
        background-color: #F6F8FB;
      }

      .navbar {
        border-bottom: 1px solid #E5E7EB;
      }

      .brand-title {
        font-weight: 600;
      }

      .brand-title span {
        font-weight: 400;
        opacity: 0.75;
        margin-left: 6px;
      }

      .page-heading {
        margin-bottom: 24px;
      }

      .page-heading h2 {
        font-weight: 600;
        margin-bottom: 4px;
      }

      .page-heading p {
        color: #64748B;
        margin-bottom: 0;
      }

      .filter-bar {
        background: white;
        border: 1px solid #E5E7EB;
        border-radius: 12px;
        padding: 16px 18px 4px 18px;
        margin-bottom: 20px;
      }

      .card {
        border: 1px solid #E5E7EB;
        border-radius: 14px;
        box-shadow: none;
      }

      .card-header {
        background: white;
        border-bottom: 1px solid #EEF1F4;
        font-weight: 600;
      }

      .chart-placeholder {
        min-height: 260px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        color: #94A3B8;
        text-align: center;
        gap: 8px;
      }

      .placeholder-title {
        color: #475569;
        font-weight: 500;
      }

      .placeholder-subtitle {
        font-size: 0.85rem;
      }

      .section-label {
        color: #64748B;
        font-size: 0.8rem;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        font-weight: 600;
        margin-bottom: 8px;
      }

      .upload-zone {
        border: 2px dashed #CBD5E1;
        border-radius: 14px;
        padding: 45px 20px;
        text-align: center;
        background: #FAFBFC;
      }

      .upload-zone h5 {
        margin-top: 15px;
      }

      .upload-zone p {
        color: #64748B;
      }

      .filter-section-title {
        font-size: 0.82rem;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        font-weight: 600;
        color: #64748B;
        margin-bottom: 10px;
      }

      .snapshot-heading {
        margin: 8px 0 14px 0;
      }

      .snapshot-heading h4 {
        margin-bottom: 3px;
        font-weight: 600;
      }

      .snapshot-heading p {
        margin-bottom: 0;
        color: #64748B;
      }

      .prototype-note {
        display: inline-block;
        margin-top: 6px;
        padding: 4px 9px;
        border-radius: 999px;
        background: #EEF2F7;
        color: #64748B;
        font-size: 0.78rem;
      }

      .chart-header-layout {
        width: 100%;
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 18px;
      }

      .chart-header-title {
        font-weight: 600;
        margin-bottom: 2px;
      }

      .chart-header-subtitle {
        color: #64748B;
        font-size: 0.82rem;
        font-weight: 400;
      }

      .chart-header-control {
        width: 220px;
        flex: 0 0 220px;
      }

      .chart-header-control .form-group {
        margin-bottom: 0;
      }

      .section-heading-custom {
        margin: 10px 0 12px 0;
      }

      .section-heading-custom h4 {
        font-weight: 600;
        margin-bottom: 3px;
      }

      .section-heading-custom .text-secondary {
        font-size: 0.88rem;
      }

      @media (max-width: 768px) {
        .chart-header-layout {
          flex-direction: column;
        }

        .chart-header-control {
          width: 100%;
          flex-basis: auto;
        }
      }
      
      

    "))
    
  ),
  
  
  # PAGE 1 — OVERVIEW  ----
  nav_panel(
    
    "Overview",
    icon = bs_icon("graph-up"),
    
    div(
      class = "p-4",
      
      div(
        class = "page-heading",
        
        h2("Overall Performance"),
        
        p(
          "A high-level view of current performance and long-term trends."
        )
      ),
      
      
      # Analysis period -------------------------------------------------------
      div(
        class = "filter-bar",
        
        div(
          class = "filter-section-title",
          "Analysis period"
        ),
        
        layout_columns(
          
          selectInput(
            "overview_from_year",
            "From Year",
            choices = 2021:2025,
            selected = 2021
          ),
          
          selectInput(
            "overview_to_year",
            "To Year",
            choices = 2021:2025,
            selected = 2025
          ),
          
          col_widths = c(3, 3)
        )
      ),
      
      
      # Snapshot heading ------------------------------------------------------
      uiOutput("overview_snapshot_heading"),
      
      
      # KPI row ---------------------------------------------------------------
      layout_column_wrap(
        
        width = 1 / 4,
        
        value_box(
          title = "Total Income",
          value = "$12.4M",
          uiOutput("overview_income_note"),
          showcase = bs_icon("cash-stack")
        ),
        
        value_box(
          title = "Total Expenses",
          value = "$9.8M",
          uiOutput("overview_expense_note"),
          showcase = bs_icon("receipt")
        ),
        
        value_box(
          title = "Net Result",
          value = "$2.6M",
          uiOutput("overview_net_note"),
          showcase = bs_icon("wallet2")
        ),
        
        value_box(
          title = "Budget Variance",
          value = "+4.2%",
          uiOutput("overview_variance_note"),
          showcase = bs_icon("bullseye")
        )
      ),
      
      
      br(),
      
      
      # Main charts -----------------------------------------------------------
      layout_columns(
        
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              class = "chart-header-layout",
              
              div(
                div(
                  class = "chart-header-title",
                  "Annual Performance Trend"
                ),
                uiOutput("overview_trend_period")
              ),
              
              div(
                class = "chart-header-control",
                selectInput(
                  "overview_metric",
                  "Metric",
                  choices = c(
                    "Income",
                    "Expenses",
                    "Net Result",
                    "Budget Variance"
                  ),
                  selected = "Net Result"
                )
              )
            )
          ),
          
          placeholder_chart("Year-by-year performance")
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            div(
              div(
                class = "chart-header-title",
                "Budget vs Actual by Year"
              ),
              uiOutput("overview_budget_period")
            )
          ),
          placeholder_chart("Budget performance")
        ),
        
        col_widths = c(7, 5)
      )
    )
  ),
  
  
  
  # PAGE 2 — SITE COMPARISON  ----
  
  nav_panel(
    
    "Site Comparison",
    icon = bs_icon("buildings"),
    
    div(
      class = "p-4",
      
      # Page heading ----------------------------------------------------------
      
      div(
        class = "page-heading",
        
        h2("Site & Facility Comparison"),
        
        p(
          "Compare how sites perform across years and explore the drivers behind site-level differences."
        )
      ),
      
      
      # Global filters --------------------------------------------------------
      
      div(
        class = "filter-bar",
        
        div(
          class = "filter-section-title",
          "Comparison period"
        ),
        
        layout_columns(
          
          selectInput(
            "site_from_year",
            "From Year",
            choices = 2021:2025,
            selected = 2021
          ),
          
          selectInput(
            "site_to_year",
            "To Year",
            choices = 2021:2025,
            selected = 2025
          ),
          
          selectInput(
            "site_metric",
            "Metric",
            choices = c(
              "Income",
              "Expenses",
              "Net Result"
            ),
            selected = "Income"
          ),
          
          col_widths = c(3, 3, 4)
        )
      ),
      
      
      # Section 1 — Site trends across years ---------------------------------
      
      div(
        class = "section-heading-custom",
        
        h4(
          class = "mb-1",
          "Site Performance Across Years"
        ),
        
        uiOutput("site_period_subtitle")
      ),
      
      
      card(
        full_screen = TRUE,
        
        card_header(
          div(
            div(
              class = "chart-header-title",
              "Site Performance Over Time"
            ),
            
            uiOutput("site_trend_subtitle")
          )
        ),
        
        placeholder_chart(
          "Compare actual performance across sites year by year"
        )
      ),
      
      
      # Section 2 — Latest-year snapshot -------------------------------------
      
      div(
        class = "section-heading-custom mt-4",
        
        h4(
          class = "mb-1",
          uiOutput("site_snapshot_title", inline = TRUE)
        ),
        
        tags$small(
          class = "text-secondary",
          "A closer look at the latest year in the selected period"
        )
      ),
      
      
      layout_columns(
        
        # Latest-year ranking
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              div(
                class = "chart-header-title",
                "Site Ranking"
              ),
              
              uiOutput("site_ranking_subtitle")
            )
          ),
          
          placeholder_chart(
            "Actual performance ranking by site"
          )
        ),
        
        
        # Budget performance
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              div(
                class = "chart-header-title",
                "Budget Performance"
              ),
              
              uiOutput("site_budget_subtitle")
            )
          ),
          
          placeholder_chart(
            "Compare budget and actual performance across sites"
          )
        ),
        
        col_widths = c(5, 7)
      ),
      
      
      # *Section 3 — Facility drill-down --------------------------------------
      # *important notice: have not dicided what be posted
      
      div(
        class = "section-heading-custom mt-4",
        
        h4(
          class = "mb-1",
          "Facility Breakdown"
        ),
        
        tags$small(
          class = "text-secondary",
          "Start with the latest-year ranking, then switch to the trend view when you need historical context"
        )
      ),
      
      
      navset_card_tab(
        id = "facility_view",
        selected = "Latest-year ranking",
        title = "Facility Analysis",
        full_screen = TRUE,
        
        # Shared controls for both facility views
        header = card_body(
          layout_columns(
            
            selectInput(
              "selected_site",
              label = "Site",
              choices = c(
                "H&F Clayton",
                "Venues Clayton",
                "Aquatics Clayton",
                "Caulfield",
                "Peninsula",
                "Participation Programs"
              ),
              selected = "H&F Clayton"
            ),
            
            div(
              tags$label(
                class = "form-label",
                "Current focus"
              ),
              
              uiOutput("facility_breakdown_subtitle")
            ),
            
            col_widths = c(4, 8)
          )
        ),
        
        
        # Default view: latest-year facility ranking
        nav_panel(
          "Latest-year ranking",
          
          div(
            class = "mb-2",
            
            div(
              class = "chart-header-title",
              "Latest-year Facility Ranking"
            ),
            
            uiOutput("facility_ranking_subtitle")
          ),
          
          placeholder_chart(
            "Rank facilities within the selected site in the latest selected year"
          )
        ),
        
        
        # Optional view: facility trend across the selected year range
        nav_panel(
          "Trend over time",
          
          div(
            class = "mb-2",
            
            div(
              class = "chart-header-title",
              "Facility Performance Over Time"
            ),
            
            uiOutput("facility_trend_subtitle")
          ),
          
          placeholder_chart(
            "Compare facility performance across the selected year range"
          )
        )
      )
    )
  ),
  
  
  # PAGE 3 — DETAILED ANALYSIS  ----
  # Two independent detailed-analysis pages are grouped under one navbar menu.
  # This avoids adding another layer of tabs inside a single page.
  
  nav_menu(
    "Detailed Analysis",
    
    # -----------------------------------------------------------------------
    # 3A — SITE DETAIL
    # Source concept: Site Report files
    # -----------------------------------------------------------------------
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
        
        
        div(
          class = "filter-bar",
          
          div(
            class = "filter-section-title",
            "Site report detail"
          ),
          
          layout_columns(
            
            selectInput(
              "site_detail_year",
              "Year",
              choices = 2025,
              selected = 2025
            ),
            
            selectInput(
              "site_detail_site",
              "Site",
              choices = c(
                "H&F Clayton",
                "Venues Clayton",
                "Aquatics Clayton",
                "Caulfield",
                "Peninsula",
                "Participation Programs"
              ),
              selected = "H&F Clayton"
            ),
            
            selectInput(
              "site_detail_area",
              "Analysis area",
              choices = c(
                "Finance",
                "Direct Visits",
                "Indirect Visits",
                "Monthly Highlights"
              ),
              selected = "Finance"
            ),
            
            col_widths = c(3, 4, 4)
          ),
          
          tags$small(
            class = "text-secondary",
            "Available analysis areas will eventually be detected from the uploaded Site Report."
          )
        ),
        
        
        uiOutput("site_detail_focus_heading"),
        
        
        # KPI row changes with the selected analysis area.
        uiOutput("site_detail_kpis"),
        
        br(),
        
        
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              div(
                class = "chart-header-title",
                uiOutput("site_detail_main_title", inline = TRUE)
              ),
              uiOutput("site_detail_main_subtitle")
            )
          ),
          
          uiOutput("site_detail_main_content")
        ),
        
        
        layout_columns(
          
          card(
            full_screen = TRUE,
            
            card_header(
              div(
                div(
                  class = "chart-header-title",
                  uiOutput("site_detail_breakdown_title", inline = TRUE)
                ),
                uiOutput("site_detail_breakdown_subtitle")
              )
            ),
            
            uiOutput("site_detail_breakdown_content")
          ),
          
          card(
            card_header("Context & Notes"),
            
            div(
              class = "p-3",
              
              p(
                class = "text-secondary",
                "This area can provide context for unusual monthly movements, events, operational changes and reporting notes."
              ),
              
              tags$ul(
                tags$li("Monthly highlights from the Site Report"),
                tags$li("Operational or program changes"),
                tags$li("Potential explanations for unusual peaks or drops")
              )
            )
          ),
          
          col_widths = c(8, 4)
        )
      )
    ),
    
    
    # -----------------------------------------------------------------------
    # 3B — VENUES OPERATIONS
    # Source concept: Venues Tracking Report files
    # -----------------------------------------------------------------------
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
        
        
        div(
          class = "filter-bar",
          
          div(
            class = "filter-section-title",
            "Operational detail"
          ),
          
          layout_columns(
            
            selectInput(
              "venues_year",
              "Year",
              choices = 2022:2025,
              selected = 2025
            ),
            
            selectInput(
              "venues_area",
              "Operational area",
              choices = c(
                "Facility Usage & Attendance",
                "Facility Hire",
                "Membership",
                "Online Sales",
                "Workforce",
                "Finance",
                "Customer Operations",
                "Offers & Targets",
                "Discounts"
              ),
              selected = "Facility Usage & Attendance"
            ),
            
            col_widths = c(3, 5)
          ),
          
          tags$small(
            class = "text-secondary",
            "Operational areas will eventually update according to the sheets available in the selected year's Tracking Report."
          )
        ),
        
        
        uiOutput("venues_focus_heading"),
        
        
        uiOutput("venues_kpis"),
        
        br(),
        
        
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              div(
                class = "chart-header-title",
                "Monthly Operational Trend"
              ),
              uiOutput("venues_main_subtitle")
            )
          ),
          
          placeholder_chart(
            "Month-by-month operational performance"
          )
        ),
        
        
        layout_columns(
          
          card(
            full_screen = TRUE,
            
            card_header(
              div(
                div(
                  class = "chart-header-title",
                  "Operational Breakdown"
                ),
                uiOutput("venues_breakdown_subtitle")
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
                div(
                  class = "chart-header-subtitle",
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
  ),
  
  
  # PAGE 4 — FORECASTING  ----
  
  nav_panel(
    "Forecasting",
    icon = bs_icon("graph-up-arrow"),
    
    div(
      class = "p-4",
      
      div(
        class = "page-heading",
        
        h2("Forecasting"),
        
        p(
          "Explore forward-looking performance based on validated historical trends."
        )
      ),
      
      
      div(
        class = "filter-bar",
        
        div(
          class = "filter-section-title",
          "Forecast settings"
        ),
        
        layout_columns(
          
          selectInput(
            "forecast_level",
            "Forecast level",
            choices = c(
              "Overall",
              "Site"
            ),
            selected = "Overall"
          ),
          
          selectInput(
            "forecast_metric",
            "Metric",
            choices = c(
              "Income",
              "Expenses",
              "Net Result"
            ),
            selected = "Income"
          ),
          
          selectInput(
            "forecast_horizon",
            "Forecast horizon",
            choices = c(
              "3 months",
              "6 months",
              "12 months"
            ),
            selected = "12 months"
          ),
          
          conditionalPanel(
            condition = "input.forecast_level == 'Site'",
            
            selectInput(
              "forecast_site",
              "Site",
              choices = c(
                "H&F Clayton",
                "Venues Clayton",
                "Aquatics Clayton",
                "Caulfield",
                "Peninsula",
                "Participation Programs"
              ),
              selected = "H&F Clayton"
            )
          ),
          
          col_widths = c(3, 3, 3, 3)
        )
      ),
      
      
      div(
        class = "snapshot-heading",
        
        h4("Forecast Prototype"),
        
        p(
          "Forecasts will only be enabled for metrics with sufficient, continuous and consistently defined historical data."
        ),
        
        span(
          class = "prototype-note",
          "Forecast values below are intentionally left as placeholders until the modelling stage"
        )
      ),
      
      
      uiOutput("forecast_kpis"),
      
      br(),
      
      
      card(
        full_screen = TRUE,
        
        card_header(
          div(
            div(
              class = "chart-header-title",
              "Historical Performance & Forecast"
            ),
            uiOutput("forecast_main_subtitle")
          )
        ),
        
        placeholder_chart(
          "Historical actuals, forecast path and prediction intervals"
        )
      ),
      
      
      layout_columns(
        
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              div(
                class = "chart-header-title",
                "Seasonal Pattern"
              ),
              div(
                class = "chart-header-subtitle",
                "Identify recurring high and low months"
              )
            )
          ),
          
          placeholder_chart(
            "Historical monthly seasonality"
          )
        ),
        
        card(
          full_screen = TRUE,
          
          card_header(
            div(
              div(
                class = "chart-header-title",
                "Forecast vs Budget"
              ),
              div(
                class = "chart-header-subtitle",
                "Planning comparison when future budget data is available"
              )
            )
          ),
          
          placeholder_chart(
            "Compare forecast expectations with planned budget"
          )
        ),
        
        col_widths = c(6, 6)
      )
    )
  ),
  
  
  # PAGE 5 — DATA   ----
  
  nav_panel(
    
    "Data",
    icon = bs_icon("database"),
    
    div(
      class = "p-4",
      
      div(
        class = "page-heading",
        
        h2("Data Management"),
        
        p(
          "Upload and manage the reports used throughout the dashboard."
        )
      ),
      
      
      layout_columns(
        
        card(
          
          card_header("Monthly Site Reports"),
          
          card_body(
            
            div(
              class = "upload-zone",
              
              bs_icon(
                "cloud-arrow-up",
                size = "42px"
              ),
              
              h5("Upload Monthly Site Reports"),
              
              p(
                "Upload one or multiple Excel files."
              ),
              
              actionButton(
                "dummy_upload",
                "Browse Files",
                class = "btn-primary"
              ),
              
              br(),
              br(),
              
              tags$small(
                class = "text-secondary",
                "Supported format: .xlsx"
              )
            )
          )
        ),
        
        
        card(
          
          card_header("Data Status"),
          
          card_body(
            
            h6("Detected reporting years"),
            
            tags$div(
              class = "mt-3",
              
              p("✓ 2021 Monthly Site Report"),
              p("✓ 2022 Monthly Site Report"),
              p("✓ 2023 Monthly Site Report"),
              p(
                class = "text-secondary",
                "○ 2024 Not uploaded"
              ),
              p("✓ 2025 Monthly Site Report")
            ),
            
            hr(),
            
            p(
              class = "text-secondary",
              "Detailed site and tracking reports will be supported in a later stage."
            )
          )
        ),
        
        col_widths = c(7, 5)
      )
    )
  )
)


# Server  ----

server <- function(input, output, session) {
  
  # Overview prototype logic ----
  # The dashboard is still using placeholder values. These small reactive
  # elements are only here to demonstrate how the selected period will be
  # communicated clearly in the final dashboard.
  
  observeEvent(input$overview_from_year, {
    req(input$overview_from_year)
    from_year <- as.integer(input$overview_from_year)
    
    updateSelectInput(
      session,
      "overview_to_year",
      choices = from_year:2025,
      selected = max(from_year, if (is.null(input$overview_to_year)) 2025 else as.integer(input$overview_to_year))
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$overview_to_year, {
    req(input$overview_to_year)
    to_year <- as.integer(input$overview_to_year)
    
    updateSelectInput(
      session,
      "overview_from_year",
      choices = 2021:to_year,
      selected = min(to_year, if (is.null(input$overview_from_year)) 2021 else as.integer(input$overview_from_year))
    )
  }, ignoreInit = TRUE)
  
  overview_period <- reactive({
    req(input$overview_from_year, input$overview_to_year)
    
    from_year <- as.integer(input$overview_from_year)
    to_year <- as.integer(input$overview_to_year)
    
    list(
      from = from_year,
      to = to_year,
      previous = to_year - 1,
      label = paste0(from_year, "–", to_year)
    )
  })
  
  output$overview_snapshot_heading <- renderUI({
    period <- overview_period()
    
    div(
      class = "snapshot-heading",
      h4(paste0(period$to, " Performance Snapshot")),
      p(
        paste0(
          "Latest reporting year within the selected period (",
          period$label,
          ")."
        )
      ),
      span(
        class = "prototype-note",
        "Prototype values are placeholders for layout demonstration only"
      )
    )
  })
  
  output$overview_income_note <- renderUI({
    period <- overview_period()
    span(paste0(period$to, " · ↑ 6.8% vs ", period$previous))
  })
  
  output$overview_expense_note <- renderUI({
    period <- overview_period()
    span(paste0(period$to, " · ↑ 3.1% vs ", period$previous))
  })
  
  output$overview_net_note <- renderUI({
    period <- overview_period()
    span(paste0(period$to, " reporting year"))
  })
  
  output$overview_variance_note <- renderUI({
    period <- overview_period()
    span(paste0(period$to, " · placeholder variance"))
  })
  
  output$overview_trend_period <- renderUI({
    period <- overview_period()
    div(
      class = "chart-header-subtitle",
      paste0("Performance over time · ", period$label)
    )
  })
  
  output$overview_budget_period <- renderUI({
    period <- overview_period()
    div(
      class = "chart-header-subtitle",
      paste0("Comparison across the selected period · ", period$label)
    )
  })
  
  
  
  # Site comparison prototype logic ----
  # From/To year controls follow the same logic as the Overview page.
  # "Measure" is intentionally not a global filter here:
  #   - the main trend shows Actual performance;
  #   - Budget Performance compares Budget vs Actual;
  #   - the latest-year ranking shows Actual performance.
  
  observeEvent(input$site_from_year, {
    req(input$site_from_year)
    from_year <- as.integer(input$site_from_year)
    
    current_to <- if (is.null(input$site_to_year)) {
      2025
    } else {
      as.integer(input$site_to_year)
    }
    
    updateSelectInput(
      session,
      "site_to_year",
      choices = from_year:2025,
      selected = max(from_year, current_to)
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$site_to_year, {
    req(input$site_to_year)
    to_year <- as.integer(input$site_to_year)
    
    current_from <- if (is.null(input$site_from_year)) {
      2021
    } else {
      as.integer(input$site_from_year)
    }
    
    updateSelectInput(
      session,
      "site_from_year",
      choices = 2021:to_year,
      selected = min(to_year, current_from)
    )
  }, ignoreInit = TRUE)
  
  
  site_period <- reactive({
    req(
      input$site_from_year,
      input$site_to_year,
      input$site_metric
    )
    
    from_year <- as.integer(input$site_from_year)
    to_year <- as.integer(input$site_to_year)
    
    list(
      from = from_year,
      to = to_year,
      label = paste0(from_year, "–", to_year),
      metric = input$site_metric
    )
  })
  
  
  output$site_period_subtitle <- renderUI({
    period <- site_period()
    
    tags$small(
      class = "text-secondary",
      paste0(
        "Compare ",
        tolower(period$metric),
        " across sites over ",
        period$label,
        "."
      )
    )
  })
  
  
  output$site_trend_subtitle <- renderUI({
    period <- site_period()
    
    div(
      class = "chart-header-subtitle",
      paste0(
        "Actual ",
        period$metric,
        " by site · ",
        period$label
      )
    )
  })
  
  
  output$site_snapshot_title <- renderUI({
    period <- site_period()
    paste0(period$to, " Site Snapshot")
  })
  
  
  output$site_ranking_subtitle <- renderUI({
    period <- site_period()
    
    div(
      class = "chart-header-subtitle",
      paste0(
        "Actual ",
        period$metric,
        " ranking · ",
        period$to
      )
    )
  })
  
  
  output$site_budget_subtitle <- renderUI({
    period <- site_period()
    
    div(
      class = "chart-header-subtitle",
      paste0(
        "Budget vs Actual · ",
        period$metric,
        " · ",
        period$to
      )
    )
  })
  
  
  output$facility_breakdown_subtitle <- renderUI({
    period <- site_period()
    req(input$selected_site)
    
    div(
      class = "chart-header-subtitle",
      paste0(
        input$selected_site,
        " · ",
        period$metric,
        " · latest year ",
        period$to,
        " · trend period ",
        period$label
      )
    )
  })
  
  
  output$facility_ranking_subtitle <- renderUI({
    period <- site_period()
    req(input$selected_site)
    
    div(
      class = "chart-header-subtitle",
      paste0(
        input$selected_site,
        " · Actual ",
        period$metric,
        " · ",
        period$to,
        " snapshot"
      )
    )
  })
  
  
  output$facility_trend_subtitle <- renderUI({
    period <- site_period()
    req(input$selected_site)
    
    div(
      class = "chart-header-subtitle",
      paste0(
        input$selected_site,
        " · Actual ",
        period$metric,
        " · ",
        period$label
      )
    )
  })
  
  
  
  # Detailed analysis — Site Detail prototype logic ----
  # The available analysis area depends on the selected Site Report.
  
  observeEvent(input$site_detail_site, {
    req(input$site_detail_site)
    
    choices <- switch(
      input$site_detail_site,
      
      "H&F Clayton" = c(
        "Finance",
        "Direct Visits",
        "Indirect Visits",
        "Monthly Highlights"
      ),
      
      "Aquatics Clayton" = c(
        "Finance",
        "Direct Visits",
        "Indirect Visits",
        "Enrolments",
        "Monthly Highlights"
      ),
      
      "Participation Programs" = c(
        "Finance",
        "Direct Visits",
        "Indirect Visits",
        "Enrolments",
        "Monthly Highlights"
      ),
      
      c(
        "Finance",
        "Direct Visits",
        "Indirect Visits",
        "Memberships",
        "Monthly Highlights"
      )
    )
    
    updateSelectInput(
      session,
      "site_detail_area",
      choices = choices,
      selected = choices[[1]]
    )
  }, ignoreInit = TRUE)
  
  
  output$site_detail_focus_heading <- renderUI({
    req(input$site_detail_year, input$site_detail_site, input$site_detail_area)
    
    div(
      class = "snapshot-heading",
      h4(
        paste0(
          input$site_detail_year,
          " · ",
          input$site_detail_site,
          " · ",
          input$site_detail_area
        )
      ),
      p(
        "Month-by-month detail from the selected Site Report."
      ),
      span(
        class = "prototype-note",
        "Prototype values are placeholders for layout demonstration only"
      )
    )
  })
  
  
  output$site_detail_kpis <- renderUI({
    req(input$site_detail_area)
    
    area <- input$site_detail_area
    
    if (area == "Monthly Highlights") {
      return(NULL)
    }
    
    boxes <- switch(
      area,
      
      "Finance" = list(
        c("YTD Actual", "$1.24M", "Current year actual", "graph-up-arrow"),
        c("YTD Budget", "$1.18M", "Current year budget", "clipboard-data"),
        c("Variance", "+5.1%", "Actual vs budget", "arrow-up-right")
      ),
      
      "Direct Visits" = list(
        c("Total Direct Visits", "128K", "Year-to-date", "graph-up-arrow"),
        c("Student Share", "54%", "Placeholder audience mix", "bar-chart-line"),
        c("Community Share", "31%", "Placeholder audience mix", "bar-chart-line")
      ),
      
      "Indirect Visits" = list(
        c("Indirect Visits", "42K", "Year-to-date", "graph-up-arrow"),
        c("Largest Activity", "—", "Placeholder", "bar-chart-line"),
        c("Change", "+4.8%", "Placeholder", "arrow-up-right")
      ),
      
      "Memberships" = list(
        c("Active Members", "4,820", "Placeholder", "graph-up-arrow"),
        c("New Members", "640", "Placeholder", "clipboard-data"),
        c("Expiring", "520", "Placeholder", "calendar3")
      ),
      
      "Enrolments" = list(
        c("Total Enrolments", "6,340", "Placeholder", "clipboard-data"),
        c("Student", "3,920", "Placeholder", "bar-chart-line"),
        c("Community", "1,860", "Placeholder", "graph-up-arrow")
      )
    )
    
    layout_column_wrap(
      width = 1 / 3,
      lapply(
        boxes,
        function(x) {
          value_box(
            title = x[[1]],
            value = x[[2]],
            x[[3]],
            showcase = bs_icon(x[[4]])
          )
        }
      )
    )
  })
  
  
  site_detail_titles <- reactive({
    req(input$site_detail_area)
    
    switch(
      input$site_detail_area,
      "Finance" = list(
        main = "Monthly Financial Performance",
        breakdown = "Budget vs Actual Breakdown",
        main_placeholder = "January–December financial trend",
        breakdown_placeholder = "Break down actual and budget performance"
      ),
      "Direct Visits" = list(
        main = "Monthly Direct Visits",
        breakdown = "Visit Type Breakdown",
        main_placeholder = "January–December direct visit trend",
        breakdown_placeholder = "Compare visit categories and audience groups"
      ),
      "Indirect Visits" = list(
        main = "Monthly Indirect Visits",
        breakdown = "Activity Breakdown",
        main_placeholder = "January–December indirect visit trend",
        breakdown_placeholder = "Compare indirect visit activities"
      ),
      "Memberships" = list(
        main = "Monthly Membership Movement",
        breakdown = "Membership Mix",
        main_placeholder = "Active, new and expiring membership trends",
        breakdown_placeholder = "Compare membership categories"
      ),
      "Enrolments" = list(
        main = "Monthly Enrolments",
        breakdown = "Enrolment Mix",
        main_placeholder = "January–December enrolment trend",
        breakdown_placeholder = "Compare enrolment categories"
      ),
      "Monthly Highlights" = list(
        main = "Monthly Highlights Timeline",
        breakdown = "Highlight Categories",
        main_placeholder = "Operational highlights across January–December",
        breakdown_placeholder = "Summarise the types of events and operational notes"
      )
    )
  })
  
  
  output$site_detail_main_title <- renderUI({
    site_detail_titles()$main
  })
  
  output$site_detail_main_subtitle <- renderUI({
    req(input$site_detail_year, input$site_detail_site)
    div(
      class = "chart-header-subtitle",
      paste0(input$site_detail_site, " · ", input$site_detail_year)
    )
  })
  
  output$site_detail_main_content <- renderUI({
    placeholder_chart(site_detail_titles()$main_placeholder)
  })
  
  output$site_detail_breakdown_title <- renderUI({
    site_detail_titles()$breakdown
  })
  
  output$site_detail_breakdown_subtitle <- renderUI({
    req(input$site_detail_area)
    div(
      class = "chart-header-subtitle",
      paste0("Detailed view · ", input$site_detail_area)
    )
  })
  
  output$site_detail_breakdown_content <- renderUI({
    placeholder_chart(site_detail_titles()$breakdown_placeholder)
  })
  
  
  # Detailed analysis — Venues Operations prototype logic ----
  # Tracking Reports change over time, so the list of operational areas is
  # updated to demonstrate how future uploaded files can control availability.
  
  observeEvent(input$venues_year, {
    req(input$venues_year)
    year <- as.integer(input$venues_year)
    
    choices <- c(
      "Facility Usage & Attendance",
      "Facility Hire",
      "Membership",
      "Online Sales",
      "Workforce",
      "Finance",
      "Discounts"
    )
    
    if (year >= 2023) {
      choices <- c(choices, "Customer Operations")
    }
    
    if (year >= 2024) {
      choices <- c(choices, "Offers & Targets")
    }
    
    updateSelectInput(
      session,
      "venues_area",
      choices = choices,
      selected = choices[[1]]
    )
  }, ignoreInit = TRUE)
  
  
  output$venues_focus_heading <- renderUI({
    req(input$venues_year, input$venues_area)
    
    div(
      class = "snapshot-heading",
      h4(paste0(input$venues_year, " · ", input$venues_area)),
      p("Operational detail from the selected Venues Tracking Report."),
      span(
        class = "prototype-note",
        "Prototype values are placeholders for layout demonstration only"
      )
    )
  })
  
  
  output$venues_kpis <- renderUI({
    req(input$venues_area)
    
    boxes <- switch(
      input$venues_area,
      
      "Facility Usage & Attendance" = list(
        c("Total Attendance", "—", "Placeholder", "graph-up-arrow"),
        c("Usage Hours", "—", "Placeholder", "calendar3"),
        c("Peak Activity", "—", "Placeholder", "bar-chart-line")
      ),
      
      "Facility Hire" = list(
        c("Hire Revenue", "—", "Placeholder", "cash-stack"),
        c("Bookings", "—", "Placeholder", "calendar3"),
        c("Avg Revenue / Booking", "—", "Placeholder", "receipt")
      ),
      
      "Membership" = list(
        c("Active Members", "—", "Placeholder", "graph-up-arrow"),
        c("New Members", "—", "Placeholder", "clipboard-data"),
        c("Expiring", "—", "Placeholder", "calendar3")
      ),
      
      "Online Sales" = list(
        c("Online Sales", "—", "Placeholder", "cash-stack"),
        c("Transactions", "—", "Placeholder", "receipt"),
        c("Avg Transaction", "—", "Placeholder", "cash-stack")
      ),
      
      "Workforce" = list(
        c("Casual Hours", "—", "Placeholder", "calendar3"),
        c("Casual Wages", "—", "Placeholder", "cash-stack"),
        c("Avg Cost / Hour", "—", "Placeholder", "receipt")
      ),
      
      "Finance" = list(
        c("YTD Actual", "—", "Placeholder", "graph-up-arrow"),
        c("YTD Budget", "—", "Placeholder", "clipboard-data"),
        c("Variance", "—", "Placeholder", "arrow-up-right")
      ),
      
      "Customer Operations" = list(
        c("Service Desk Calls", "—", "Placeholder", "receipt"),
        c("Cancellations", "—", "Placeholder", "arrow-up-right"),
        c("Resolution Rate", "—", "Placeholder", "bullseye")
      ),
      
      "Offers & Targets" = list(
        c("Active Offers", "—", "Placeholder", "bullseye"),
        c("Target Achievement", "—", "Placeholder", "graph-up-arrow"),
        c("Variance", "—", "Placeholder", "arrow-up-right")
      ),
      
      "Discounts" = list(
        c("Discount Value", "—", "Placeholder", "bullseye"),
        c("Transactions", "—", "Placeholder", "receipt"),
        c("Avg Discount", "—", "Placeholder", "cash-stack")
      )
    )
    
    layout_column_wrap(
      width = 1 / 3,
      lapply(
        boxes,
        function(x) {
          value_box(
            title = x[[1]],
            value = x[[2]],
            x[[3]],
            showcase = bs_icon(x[[4]])
          )
        }
      )
    )
  })
  
  
  output$venues_main_subtitle <- renderUI({
    req(input$venues_year, input$venues_area)
    div(
      class = "chart-header-subtitle",
      paste0(input$venues_area, " · January–December ", input$venues_year)
    )
  })
  
  
  output$venues_breakdown_subtitle <- renderUI({
    req(input$venues_area)
    div(
      class = "chart-header-subtitle",
      paste0("Category-level view · ", input$venues_area)
    )
  })
  
  
  # Forecasting prototype logic ----
  
  output$forecast_kpis <- renderUI({
    req(input$forecast_horizon)
    
    layout_column_wrap(
      width = 1 / 3,
      
      value_box(
        title = paste0("Next ", input$forecast_horizon, " Forecast"),
        value = "—",
        "Model not connected",
        showcase = bs_icon("graph-up-arrow")
      ),
      
      value_box(
        title = "Expected Change",
        value = "—",
        "Compared with the latest equivalent period",
        showcase = bs_icon("arrow-up-right")
      ),
      
      value_box(
        title = "Forecast Range",
        value = "—",
        "Prediction interval will be shown here",
        showcase = bs_icon("bullseye")
      )
    )
  })
  
  
  output$forecast_main_subtitle <- renderUI({
    req(input$forecast_level, input$forecast_metric, input$forecast_horizon)
    
    location <- if (
      identical(input$forecast_level, "Site") &&
      !is.null(input$forecast_site)
    ) {
      input$forecast_site
    } else {
      "Overall"
    }
    
    div(
      class = "chart-header-subtitle",
      paste0(
        location,
        " · ",
        input$forecast_metric,
        " · ",
        input$forecast_horizon,
        " horizon"
      )
    )
  })

}


shinyApp(ui, server)





