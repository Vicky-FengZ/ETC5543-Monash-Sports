# -------------------------------------------------------------------------
# Executive Dashboard visual registry
# -------------------------------------------------------------------------
# This file describes the visuals that may be shown on the Executive
# Dashboard.
#
# It does NOT calculate values or draw charts.
#
# Existing calculation and plot functions remain the source of truth.
# The registry only describes:
#   - what the visual is;
#   - whether it belongs to Finance or Metrics;
#   - which report family supplies its data;
#   - which cleaned dataset it requires;
#   - whether it is selected by default;
#   - its preferred presentation size.


EXECUTIVE_SOURCE_LABELS <- c(
  monthly = "Monthly Site Reports",
  detailed = "Detailed Site Reports",
  venues = "Venues Tracking Reports",
  forecasting = "Forecasting",
  supporting = "Supporting Components"
)


EXECUTIVE_VISUAL_REGISTRY <- list(

  # -----------------------------------------------------------------------
  # Monthly Site Reports
  # -----------------------------------------------------------------------

  monthly_income_expenses = list(
    id = "monthly_income_expenses",
    title = "Income vs Expenses",
    category = "Finance",
    focus = "Overview",
    source_group = "monthly",
    required_data = "finance",
    default_selected = TRUE,
    preferred_size = "wide",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  annual_performance_trend = list(
    id = "annual_performance_trend",
    title = "Annual Performance Trend",
    category = "Finance",
    focus = "Overview",
    source_group = "monthly",
    required_data = "finance",
    default_selected = TRUE,
    preferred_size = "wide",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  budget_vs_actual = list(
    id = "budget_vs_actual",
    title = "Budget vs Actual",
    category = "Finance",
    focus = "Overview",
    source_group = "monthly",
    required_data = "finance",
    default_selected = TRUE,
    preferred_size = "wide",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  site_performance = list(
    id = "site_performance",
    title = "Site Performance Over Time",
    category = "Finance",
    focus = "Site",
    source_group = "monthly",
    required_data = "finance",
    default_selected = TRUE,
    preferred_size = "wide",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  site_ranking = list(
    id = "site_ranking",
    title = "Site Ranking",
    category = "Finance",
    focus = "Site",
    source_group = "monthly",
    required_data = "finance",
    default_selected = TRUE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  site_budget_performance = list(
    id = "site_budget_performance",
    title = "Site Budget Performance",
    category = "Finance",
    focus = "Site",
    source_group = "monthly",
    required_data = "finance",
    default_selected = TRUE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  site_revenue_opportunity = list(
    id = "site_revenue_opportunity",
    title = "Revenue Opportunity",
    category = "Finance",
    focus = "Site",
    source_group = "monthly",
    required_data = c(
      "finance",
      "discounts"
    ),
    default_selected = TRUE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  site_discount_mix = list(
    id = "site_discount_mix",
    title = "Discount Mix",
    category = "Finance",
    focus = "Site",
    source_group = "monthly",
    required_data = "discounts",
    default_selected = TRUE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  revenue_capture = list(
    id = "revenue_capture",
    title = "Revenue Capture",
    category = "Finance",
    focus = "Overview",
    source_group = "monthly",
    required_data = c(
      "finance",
      "discounts"
    ),
    default_selected = TRUE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

    site_values = list(
    id = "site_values",
    title = "Site Values",
    category = "Finance",
    focus = "Site",
    source_group = "monthly",
    component_type = "table",
    required_data = "finance",
    default_selected = TRUE,
    preferred_size = "wide",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),


  # -----------------------------------------------------------------------
  # Detailed Site Reports
  # -----------------------------------------------------------------------

  facility_ranking = list(
    id = "facility_ranking",
    title = "Facility Ranking",
    category = "Finance",
    focus = "Site",
    source_group = "detailed",
    required_data = "finance",
    default_selected = FALSE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  facility_performance = list(
    id = "facility_performance",
    title = "Facility Performance Over Time",
    category = "Finance",
    focus = "Site",
    source_group = "detailed",
    required_data = "finance",
    default_selected = FALSE,
    preferred_size = "wide",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  facility_discount_mix = list(
    id = "facility_discount_mix",
    title = "Facility Discount Mix",
    category = "Finance",
    focus = "Site",
    source_group = "detailed",
    required_data = "discounts",
    default_selected = FALSE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  facility_discount_trend = list(
    id = "facility_discount_trend",
    title = "Discount Opportunity Trend",
    category = "Finance",
    focus = "Site",
    source_group = "detailed",
    required_data = "discounts",
    default_selected = FALSE,
    preferred_size = "wide",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  facility_revenue_opportunity = list(
    id = "facility_revenue_opportunity",
    title = "Actual vs Potential Revenue",
    category = "Finance",
    focus = "Site",
    source_group = "detailed",
    required_data = c(
      "finance",
      "discounts"
    ),
    default_selected = FALSE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  facility_capture_rate = list(
    id = "facility_capture_rate",
    title = "Facility Revenue Capture Rate",
    category = "Finance",
    focus = "Site",
    source_group = "detailed",
    required_data = c(
      "finance",
      "discounts"
    ),
    default_selected = FALSE,
    preferred_size = "standard",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  ),

  # -----------------------------------------------------------------------
  # Supporting components
  # -----------------------------------------------------------------------

  key_insights = list(
    id = "key_insights",
    title = "Key Insights",
    category = "Finance",
    focus = "Overview",
    source_group = "supporting",
    component_type = "insight",
    required_data = character(),
    default_selected = TRUE,
    preferred_size = "supporting",
    supports_year_review = TRUE,
    supports_comparison = TRUE
  )
)


# -------------------------------------------------------------------------
# Registry helpers
# -------------------------------------------------------------------------

executive_visual_component_type <- function(
    visual
) {

  component_type <- visual$component_type

  if (
    is.null(component_type) ||
      length(component_type) == 0L ||
      is.na(component_type[[1L]]) ||
      !nzchar(
        trimws(
          as.character(
            component_type[[1L]]
          )
        )
      )
  ) {
    return("graph")
  }

  component_type <- tolower(
    trimws(
      as.character(
        component_type[[1L]]
      )
    )
  )

  if (
    !component_type %in%
      c(
        "graph",
        "table",
        "insight"
      )
  ) {
    return("graph")
  }

  component_type
}

executive_visual_ids <- function(
    category = NULL,
    source_group = NULL,
    component_type = NULL,
    focus = NULL
) {

  visuals <- EXECUTIVE_VISUAL_REGISTRY

  keep <- rep(
    TRUE,
    length(visuals)
  )


  if (!is.null(category)) {

    keep <- keep & vapply(
      visuals,
      function(x) {
        identical(
          x$category,
          category
        )
      },
      logical(1)
    )
  }


  if (!is.null(focus)) {

    keep <- keep & vapply(
      visuals,
      function(x) {
        identical(
          x$focus,
          focus
        )
      },
      logical(1)
    )
  }


  if (!is.null(source_group)) {

    keep <- keep & vapply(
      visuals,
      function(x) {
        identical(
          x$source_group,
          source_group
        )
      },
      logical(1)
    )
  }


  if (!is.null(component_type)) {

    component_type <- tolower(
      as.character(
        component_type[[1L]]
      )
    )

    keep <- keep & vapply(
      visuals,
      function(x) {
        identical(
          executive_visual_component_type(x),
          component_type
        )
      },
      logical(1)
    )
  }


  names(visuals)[keep]
}

executive_default_visual_ids <- function(
    category = "Finance",
    focus = NULL
) {

  ids <- executive_visual_ids(
    category = category,
    focus = focus
    )

  ids[
    vapply(
      EXECUTIVE_VISUAL_REGISTRY[ids],
      function(x) {
        isTRUE(
          x$default_selected
        )
      },
      logical(1)
    )
  ]
}


executive_visual_source_label <- function(
    source_group
) {

  if (
    is.null(source_group) ||
      !source_group %in% names(
        EXECUTIVE_SOURCE_LABELS
      )
  ) {
    return(
      "Other"
    )
  }

  unname(
    EXECUTIVE_SOURCE_LABELS[[source_group]]
  )
}


executive_registry_table <- function() {

  visuals <- EXECUTIVE_VISUAL_REGISTRY

  rows <- lapply(
    visuals,
    function(x) {

      data.frame(
        id = x$id,
        title = x$title,
        category = x$category,
        focus = x$focus,
        component_type = executive_visual_component_type(x),
        source_group = x$source_group,
        source_label =
          executive_visual_source_label(
            x$source_group
          ),
        required_data = paste(
          x$required_data,
          collapse = ", "
        ),
        default_selected =
          isTRUE(
            x$default_selected
          ),
        preferred_size =
          x$preferred_size,
        supports_year_review =
          isTRUE(
            x$supports_year_review
          ),
        supports_comparison =
          isTRUE(
            x$supports_comparison
          ),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(
    rbind,
    rows
  )

  rownames(result) <- NULL

  result
}

# -------------------------------------------------------------------------
# Data availability helpers
# -------------------------------------------------------------------------

executive_source_data <- function(
    dashboard_data,
    source_group
) {

  if (is.null(dashboard_data)) {
    return(NULL)
  }

  source_group <- as.character(
    source_group
  )[[1L]]

  # Support a report-family object being supplied directly.
  if (
    source_group == "monthly" &&
      inherits(
        dashboard_data,
        "monthly_site_report_data"
      )
  ) {
    return(dashboard_data)
  }

  if (
    source_group == "detailed" &&
      inherits(
        dashboard_data,
        "business_unit_template_report_data"
      )
  ) {
    return(dashboard_data)
  }

  if (
    source_group == "venues" &&
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

  candidate_names <- switch(
    source_group,

    monthly = c(
      "monthly_reports",
      "monthly"
    ),

    detailed = c(
      "site_reports",
      "sites"
    ),

    venues = c(
      "venues_reports",
      "venues"
    ),

    forecasting = character(),

    character()
  )

  for (candidate in candidate_names) {

    if (
      !is.null(
        dashboard_data[[candidate]]
      )
    ) {
      return(
        dashboard_data[[candidate]]
      )
    }
  }

  NULL
}


executive_dataset_available <- function(
    source_data,
    dataset
) {

  if (
    is.null(source_data) ||
      !is.list(source_data) ||
      is.null(source_data[[dataset]])
  ) {
    return(FALSE)
  }

  value <- source_data[[dataset]]

  if (is.data.frame(value)) {
    return(
      nrow(value) > 0L
    )
  }

  length(value) > 0L
}


executive_visual_availability <- function(
    dashboard_data,
    visual_id
) {

  if (
    length(visual_id) != 1L ||
      !visual_id %in%
        names(EXECUTIVE_VISUAL_REGISTRY)
  ) {
    stop(
      paste0(
        "Unknown Executive Dashboard visual: ",
        visual_id
      ),
      call. = FALSE
    )
  }

  visual <-
  EXECUTIVE_VISUAL_REGISTRY[[visual_id]]

  required_data <-
    as.character(
      visual$required_data
    )

  # Some supporting components do not require a specific uploaded table.
  if (length(required_data) == 0L) {

  return(
    list(
      available = TRUE,
      source_available = TRUE,
      missing_data = character(),
      reason = "Available"
    )
  )
}

source_data <- executive_source_data(
  dashboard_data = dashboard_data,
  source_group = visual$source_group
)

  source_label <-
    executive_visual_source_label(
      visual$source_group
    )

  if (is.null(source_data)) {

    return(
      list(
        available = FALSE,
        source_available = FALSE,
        missing_data =
          as.character(
            visual$required_data
          ),
        reason = paste0(
          source_label,
          " not uploaded"
        )
      )
    )
  }

  available_flags <- vapply(
    required_data,
    function(dataset) {

      executive_dataset_available(
        source_data = source_data,
        dataset = dataset
      )
    },
    logical(1)
  )

  missing_data <-
    required_data[
      !available_flags
    ]

  if (length(missing_data) > 0L) {

    return(
      list(
        available = FALSE,
        source_available = TRUE,
        missing_data = missing_data,
        reason = paste0(
          "Required data unavailable: ",
          paste(
            missing_data,
            collapse = ", "
          )
        )
      )
    )
  }

  list(
    available = TRUE,
    source_available = TRUE,
    missing_data = character(),
    reason = "Available"
  )
}


executive_registry_availability_table <- function(
    dashboard_data,
    category = NULL
) {

  registry <- executive_registry_table()

  if (!is.null(category)) {

    registry <- registry[
      registry$category == category,
      ,
      drop = FALSE
    ]
  }

  if (nrow(registry) == 0L) {

    registry$available <- logical()
    registry$availability_reason <-
      character()

    return(registry)
  }

  availability <- lapply(
    registry$id,
    function(visual_id) {

      executive_visual_availability(
        dashboard_data = dashboard_data,
        visual_id = visual_id
      )
    }
  )

  registry$available <- vapply(
    availability,
    function(x) {
      isTRUE(
        x$available
      )
    },
    logical(1)
  )

  registry$availability_reason <-
    vapply(
      availability,
      function(x) {
        x$reason
      },
      character(1)
    )

  registry
}


executive_source_availability_summary <- function(
    dashboard_data,
    category = NULL
) {

  registry <-
    executive_registry_availability_table(
      dashboard_data = dashboard_data,
      category = category
    )

  if (nrow(registry) == 0L) {

    return(
      data.frame(
        source_group = character(),
        source_label = character(),
        available_visuals = integer(),
        total_visuals = integer(),
        stringsAsFactors = FALSE
      )
    )
  }

  source_groups <- unique(
    registry$source_group
  )

  rows <- lapply(
    source_groups,
    function(source_group) {

      subset_data <- registry[
        registry$source_group ==
          source_group,
        ,
        drop = FALSE
      ]

      data.frame(
        source_group = source_group,
        source_label =
          executive_visual_source_label(
            source_group
          ),
        available_visuals =
          sum(
            subset_data$available
          ),
        total_visuals =
          nrow(
            subset_data
          ),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(
    rbind,
    rows
  )

  rownames(result) <- NULL

  result
}