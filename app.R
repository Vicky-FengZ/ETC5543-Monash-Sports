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
  
  
  # PAGE 3 — YEAR DETAIL   ----
  
  nav_panel(
    
    "Year Detail",
    icon = bs_icon("calendar3"),
    
    div(
      class = "p-4",
      
      div(
        class = "page-heading",
        
        h2("Year Detail"),
        
        p(
          "Explore monthly patterns and investigate performance within a selected year."
        )
      ),
      
      
      div(
        class = "filter-bar",
        
        layout_columns(
          
          selectInput(
            "detail_year",
            "Year",
            choices = 2021:2025,
            selected = 2025
          ),
          
          selectInput(
            "detail_site",
            "Site",
            choices = c(
              "All Sites",
              "H&F Clayton",
              "Venues Clayton",
              "Aquatics Clayton",
              "Caulfield",
              "Peninsula",
              "Participation Programs"
            )
          ),
          
          selectInput(
            "detail_metric",
            "Analysis area",
            choices = c(
              "Finance",
              "Visits",
              "Membership",
              "Programs"
            )
          ),
          
          col_widths = c(3, 4, 4)
        )
      ),
      
      
      layout_column_wrap(
        
        width = 1 / 3,
        
        value_box(
          "YTD Actual",
          "$1.24M",
          showcase = bs_icon("graph-up-arrow")
        ),
        
        value_box(
          "YTD Budget",
          "$1.18M",
          showcase = bs_icon("clipboard-data")
        ),
        
        value_box(
          "Variance",
          "+5.1%",
          showcase = bs_icon("arrow-up-right")
        )
      ),
      
      
      br(),
      
      
      card(
        full_screen = TRUE,
        card_header("Monthly Performance"),
        placeholder_chart("January – December trend")
      ),
      
      
      layout_columns(
        
        card(
          card_header("Monthly Breakdown"),
          placeholder_chart("Monthly comparison")
        ),
        
        card(
          card_header("Key Highlights"),
          
          div(
            class = "p-3",
            
            h5("March 2025"),
            
            p(
              class = "text-secondary",
              "Monthly operational notes and important events will appear here."
            ),
            
            tags$ul(
              tags$li("Major event or operational change"),
              tags$li("Program or facility update"),
              tags$li("Reason behind unusual performance")
            )
          )
        ),
        
        col_widths = c(8, 4)
      )
    )
  ),
  
  
  # PAGE 4 — DATA   ----
  
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
  
}


shinyApp(ui, server)






