# -------------------------------------------------------------------------
# Site Detail module
# -------------------------------------------------------------------------

# UI ----------------------------------------------------------------------
# Detailed Analysis -> Site Detail.
# Uses Site Report files to examine one site within one year, month by month.

mod_site_detail_ui <- function(id) {

  ns <- NS(id)

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

      # Primary controls: choose the report year, site and analysis area.
      # Analysis-area choices will eventually come from detected workbook sheets.
      div(
        class = "filter-bar",

        div(
          class = "filter-section-title",
          "Site report detail"
        ),

        layout_columns(

          selectInput(
            ns("year"),
            "Year",
            choices = SITE_DETAIL_YEARS,
            selected = max(SITE_DETAIL_YEARS)
          ),

          selectInput(
            ns("site"),
            "Site",
            choices = SITE_NAMES,
            selected = SITE_NAMES[[1]]
          ),

          selectInput(
            ns("area"),
            "Analysis area",
            choices = prototype_site_detail_areas(
              SITE_NAMES[[1]]
            ),
            selected = "Finance"
          ),

          col_widths = c(3, 4, 4)
        ),

        tags$small(
          class = "text-secondary",
          paste(
            "Available analysis areas will eventually be detected",
            "from the uploaded Site Report."
          )
        )
      ),

      # Dynamic focus heading confirms the current Year / Site / Area.
      uiOutput(
        ns("focus_heading")
      ),

      # KPI cards change according to the selected analysis area.
      uiOutput(
        ns("kpis")
      ),

      br(),

      # Main chart: the month-by-month trend for the selected analysis area.
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

        uiOutput(
          ns("main_content")
        )
      ),

      # Supporting row:
      #   left  = category/budget/membership breakdown
      #   right = qualitative context and monthly notes
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

          uiOutput(
            ns("breakdown_content")
          )
        ),

        card(
          card_header(
            "Context & Notes"
          ),

          div(
            class = "p-3",

            p(
              class = "text-secondary",
              paste(
                "This area can provide context for unusual monthly",
                "movements, events, operational changes and reporting notes."
              )
            ),

            tags$ul(
              tags$li(
                "Monthly highlights from the Site Report"
              ),
              tags$li(
                "Operational or program changes"
              ),
              tags$li(
                "Potential explanations for unusual peaks or drops"
              )
            )
          )
        ),

        col_widths = c(8, 4)
      )
    )
  )
}


# Server ------------------------------------------------------------------
# Updates available analysis areas and switches KPI/chart labels according
# to the selected Site Report content.

mod_site_detail_server <- function(
  id,
  data = NULL
) {

  moduleServer(
    id,
    function(input, output, session) {

      # Prototype availability rule: different sites expose different areas.
      # Later this should be driven by the sheets found in the uploaded file.
      observeEvent(
        input$site,
        {
          req(input$site)

          choices <- prototype_site_detail_areas(
            input$site
          )

          updateSelectInput(
            session,
            "area",
            choices = choices,
            selected = choices[[1]]
          )
        },
        ignoreInit = TRUE
      )

      output$focus_heading <- renderUI({

        req(
          input$year,
          input$site,
          input$area
        )

        div(
          class = "snapshot-heading",

          h4(
            paste0(
              input$year,
              " · ",
              input$site,
              " · ",
              input$area
            )
          ),

          p(
            "Month-by-month detail from the selected Site Report."
          ),

          prototype_note()
        )
      })

      # Defines the KPI layout for each analysis area.
      # Values are placeholders; only the structure changes at this stage.
      kpi_specs <- reactive({

        req(input$area)

        switch(
          input$area,

          "Finance" = list(
            list(
              title = "YTD Actual",
              value = "$1.24M",
              subtitle = "Current year actual",
              icon = "graph-up-arrow"
            ),
            list(
              title = "YTD Budget",
              value = "$1.18M",
              subtitle = "Current year budget",
              icon = "clipboard-data"
            ),
            list(
              title = "Variance",
              value = "+5.1%",
              subtitle = "Actual vs budget",
              icon = "arrow-up-right"
            )
          ),

          "Direct Visits" = list(
            list(
              title = "Total Direct Visits",
              value = "128K",
              subtitle = "Year-to-date",
              icon = "graph-up-arrow"
            ),
            list(
              title = "Student Share",
              value = "54%",
              subtitle = "Placeholder audience mix",
              icon = "bar-chart-line"
            ),
            list(
              title = "Community Share",
              value = "31%",
              subtitle = "Placeholder audience mix",
              icon = "bar-chart-line"
            )
          ),

          "Indirect Visits" = list(
            list(
              title = "Indirect Visits",
              value = "42K",
              subtitle = "Year-to-date",
              icon = "graph-up-arrow"
            ),
            list(
              title = "Largest Activity",
              value = "—",
              subtitle = "Placeholder",
              icon = "bar-chart-line"
            ),
            list(
              title = "Change",
              value = "+4.8%",
              subtitle = "Placeholder",
              icon = "arrow-up-right"
            )
          ),

          "Memberships" = list(
            list(
              title = "Active Members",
              value = "4,820",
              subtitle = "Placeholder",
              icon = "graph-up-arrow"
            ),
            list(
              title = "New Members",
              value = "640",
              subtitle = "Placeholder",
              icon = "clipboard-data"
            ),
            list(
              title = "Expiring",
              value = "520",
              subtitle = "Placeholder",
              icon = "calendar3"
            )
          ),

          "Enrolments" = list(
            list(
              title = "Total Enrolments",
              value = "6,340",
              subtitle = "Placeholder",
              icon = "clipboard-data"
            ),
            list(
              title = "Student",
              value = "3,920",
              subtitle = "Placeholder",
              icon = "bar-chart-line"
            ),
            list(
              title = "Community",
              value = "1,860",
              subtitle = "Placeholder",
              icon = "graph-up-arrow"
            )
          ),

          "Monthly Highlights" = NULL
        )
      })

      output$kpis <- renderUI({

        boxes <- kpi_specs()

        if (is.null(boxes)) {
          return(NULL)
        }

        make_value_box_row(
          boxes,
          width = 1 / 3
        )
      })

      # Centralises chart titles/placeholders so UI text stays consistent
      # when the user switches analysis area.
      titles <- reactive({

        req(input$area)

        switch(
          input$area,

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

      output$main_title <- renderUI({
        titles()$main
      })

      output$main_subtitle <- renderUI({

        req(
          input$year,
          input$site
        )

        chart_subtitle(
          paste0(
            input$site,
            " · ",
            input$year
          )
        )
      })

      output$main_content <- renderUI({
        placeholder_chart(
          titles()$main_placeholder
        )
      })

      output$breakdown_title <- renderUI({
        titles()$breakdown
      })

      output$breakdown_subtitle <- renderUI({

        req(input$area)

        chart_subtitle(
          paste0(
            "Detailed view · ",
            input$area
          )
        )
      })

      output$breakdown_content <- renderUI({
        placeholder_chart(
          titles()$breakdown_placeholder
        )
      })

      invisible(
        list(
          year = reactive(input$year),
          site = reactive(input$site),
          area = reactive(input$area)
        )
      )
    }
  )
}
