library(shiny)
library(plotly)

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("year_select"), "Select Year", choices = c("All"))),
      column(4, selectInput(ns("metric_select"), "Select Metric", choices = c("Income", "Expense", "Discount"), selected = "Income")),
      column(4, selectInput(ns("group_select"), "Group By", choices = c("Category", "Type", "Variant"), selected = "Category"))
    ),
    fluidRow(
      column(12, plotlyOutput(ns("overview_plot")))
    )
  )
}

mod_overview_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    clean_df <- reactive({
      req(shared_data())
      normalize_schema(shared_data())
    })
    
    observeEvent(clean_df(), {
      df <- clean_df()
      if ("Year" %in% names(df)) {
        years <- c("All", sort(unique(df$Year)))
        updateSelectInput(session, "year_select", choices = years, selected = "All")
      }
    })
    
    plot_data <- reactive({
      req(clean_df(), input$metric_select, input$group_select)
      aggregate_custom_metric(
        df = clean_df(),
        metric_col = input$metric_select,
        group_col = input$group_select,
        selected_year = input$year_select
      )
    })
    
    output$overview_plot <- renderPlotly({
      req(plot_data())
      render_versatile_chart(
        df = plot_data(),
        x_col = "Month",
        y_col = input$metric_select,
        fill_col = input$group_select
      )
    })
  })
}