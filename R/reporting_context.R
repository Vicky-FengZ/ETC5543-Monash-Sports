# -------------------------------------------------------------------------
# Shared reporting context
# -------------------------------------------------------------------------
# Central helpers for discovering reporting years and, later, reporting
# scopes/sites from the data that has actually been loaded.
#
# Dashboard modules should ask this layer what reporting context exists
# rather than maintaining their own fixed year lists.


reporting_normalize_years <- function(years) {

  if (
    is.null(years) ||
      length(years) == 0L
  ) {
    return(integer())
  }

  years <- suppressWarnings(
    as.integer(
      as.character(years)
    )
  )

  sort(
    unique(
      years[
        !is.na(years)
      ]
    )
  )
}


reporting_table_years <- function(data) {

  if (
    is.null(data) ||
      !is.data.frame(data) ||
      nrow(data) == 0L ||
      !"report_year" %in% names(data)
  ) {
    return(integer())
  }

  reporting_normalize_years(
    data$report_year
  )
}


reporting_report_years <- function(
    reports,
    fallback_tables = character()
) {

  if (is.data.frame(reports)) {
    return(
      reporting_table_years(reports)
    )
  }

  if (
    is.null(reports) ||
      !is.list(reports)
  ) {
    return(integer())
  }

  # The imported file manifest is the preferred source of truth because
  # a report year exists even when one optional worksheet/table is absent.
  manifest_years <- reporting_table_years(
    reports$files
  )

  if (length(manifest_years) > 0L) {
    return(manifest_years)
  }

  if (length(fallback_tables) == 0L) {
    return(integer())
  }

  years <- unlist(
    lapply(
      fallback_tables,
      function(table_name) {

        if (!table_name %in% names(reports)) {
          return(integer())
        }

        reporting_table_years(
          reports[[table_name]]
        )
      }
    ),
    use.names = FALSE
  )

  reporting_normalize_years(years)
}


reporting_monthly_years <- function(
    monthly_reports
) {

  reporting_report_years(
    monthly_reports,
    fallback_tables = c(
      "finance",
      "discounts",
      "total_revenue"
    )
  )
}


reporting_detailed_years <- function(
    site_reports
) {

  reporting_report_years(
    site_reports,
    fallback_tables = c(
      "finance",
      "discounts",
      "total_revenue",
      "visits",
      "memberships",
      "enrollments"
    )
  )
}


reporting_venues_years <- function(
    venues_reports
) {

  reporting_report_years(
    venues_reports,
    fallback_tables = c(
      "finance",
      "attendance",
      "facility_hire",
      "court_hire",
      "membership",
      "online_sales",
      "merchandise",
      "casual_hours",
      "casual_wages"
    )
  )
}


reporting_latest_year <- function(years) {

  years <- reporting_normalize_years(
    years
  )

  if (length(years) == 0L) {
    return(NA_integer_)
  }

  max(years)
}

# -------------------------------------------------------------------------
# Site catalogue
# -------------------------------------------------------------------------

reporting_empty_site_catalogue <- function() {

  data.frame(
    site_key = character(),
    display_name = character(),
    report_unit = character(),
    campus = character(),
    site_code = character(),
    known_site = logical(),
    stringsAsFactors = FALSE
  )
}


reporting_site_slug <- function(value) {

  value <- as.character(value)
  value[is.na(value)] <- ""

  value <- trimws(
    tolower(value)
  )

  # Keep keys simple and stable:
  # "Health & Fitness" -> "health_fitness"
  value <- gsub(
    "&",
    " ",
    value,
    fixed = TRUE
  )

  value <- gsub(
    "[^a-z0-9]+",
    "_",
    value
  )

  value <- gsub(
    "^_+|_+$",
    "",
    value
  )

  value
}


reporting_site_identity_key <- function(
    report_unit,
    campus
) {

  paste0(
    reporting_site_slug(report_unit),
    "|",
    reporting_site_slug(campus)
  )
}


reporting_detailed_site_catalogue <- function(
    site_reports,
    year = NULL
) {

  empty_result <-
    reporting_empty_site_catalogue()

  if (
    is.null(site_reports) ||
      !is.list(site_reports) ||
      is.null(site_reports$files) ||
      !is.data.frame(site_reports$files) ||
      nrow(site_reports$files) == 0L
  ) {
    return(empty_result)
  }

  files <- site_reports$files

  required_columns <- c(
    "report_unit",
    "campus"
  )

  if (!all(
    required_columns %in% names(files)
  )) {
    return(empty_result)
  }

  # Optional year-specific discovery.
  if (
    !is.null(year) &&
      length(year) > 0L &&
      "report_year" %in% names(files)
  ) {

    selected_year <- suppressWarnings(
      as.integer(year[[1L]])
    )

    if (!is.na(selected_year)) {
      files <- files[
        !is.na(files$report_year) &
          files$report_year == selected_year,
        ,
        drop = FALSE
      ]
    }
  }

  if (nrow(files) == 0L) {
    return(empty_result)
  }

  report_unit <- trimws(
    as.character(
      files$report_unit
    )
  )

  campus <- trimws(
    as.character(
      files$campus
    )
  )

  campus[
    is.na(campus) |
      !nzchar(campus)
  ] <- "Unspecified"

  site_code <- if (
    "site_code" %in% names(files)
  ) {
    trimws(
      as.character(
        files$site_code
      )
    )
  } else {
    rep(
      NA_character_,
      nrow(files)
    )
  }

  site_code[
    is.na(site_code) |
      !nzchar(site_code)
  ] <- NA_character_

  valid <- !is.na(report_unit) &
    nzchar(report_unit)

  if (!any(valid)) {
    return(empty_result)
  }

  candidates <- data.frame(
    report_unit = report_unit[valid],
    campus = campus[valid],
    site_code = site_code[valid],
    stringsAsFactors = FALSE
  )

  candidates$base_key <-
    reporting_site_identity_key(
      candidates$report_unit,
      candidates$campus
    )

  # Collapse the same site appearing across several report years/files.
  # If the source explicitly supplies multiple site codes for the same
  # report-unit/campus pair, retain them as separate identities.
  grouped <- split(
    candidates,
    candidates$base_key
  )

  catalogue_rows <- lapply(
    grouped,
    function(rows) {

      codes <- unique(
        rows$site_code[
          !is.na(rows$site_code) &
            nzchar(rows$site_code)
        ]
      )

      if (length(codes) <= 1L) {

        return(
          data.frame(
            site_key = rows$base_key[[1L]],
            report_unit =
              rows$report_unit[[1L]],
            campus =
              rows$campus[[1L]],
            site_code = if (
              length(codes) == 1L
            ) {
              codes[[1L]]
            } else {
              NA_character_
            },
            stringsAsFactors = FALSE
          )
        )
      }

      data.frame(
        site_key = paste0(
          rows$base_key[[1L]],
          "|",
          reporting_site_slug(codes)
        ),
        report_unit =
          rows$report_unit[[1L]],
        campus =
          rows$campus[[1L]],
        site_code = codes,
        stringsAsFactors = FALSE
      )
    }
  )

  catalogue <- do.call(
    rbind,
    catalogue_rows
  )

  rownames(catalogue) <- NULL

  # Match existing Monash Sport sites to their established dashboard labels.
  known_catalogue <- if (
    exists(
      "KNOWN_SITE_CATALOGUE",
      inherits = TRUE
    )
  ) {
    get(
      "KNOWN_SITE_CATALOGUE",
      inherits = TRUE
    )
  } else {
    data.frame(
      display_name = character(),
      report_unit = character(),
      campus = character(),
      stringsAsFactors = FALSE
    )
  }

  known_keys <-
    reporting_site_identity_key(
      known_catalogue$report_unit,
      known_catalogue$campus
    )

  matched_known <- match(
    reporting_site_identity_key(
      catalogue$report_unit,
      catalogue$campus
    ),
    known_keys
  )

  catalogue$known_site <-
    !is.na(matched_known)

  catalogue$display_name <-
    catalogue$report_unit

  catalogue$display_name[
    catalogue$known_site
  ] <- known_catalogue$display_name[
    matched_known[
      catalogue$known_site
    ]
  ]

  # Unknown units need a campus suffix only when the same report-unit name
  # appears across more than one campus.
  unknown_rows <- which(
    !catalogue$known_site
  )

  if (length(unknown_rows) > 0L) {

    unknown_units <-
      reporting_site_slug(
        catalogue$report_unit[
          unknown_rows
        ]
      )

    unknown_campuses <-
      reporting_site_slug(
        catalogue$campus[
          unknown_rows
        ]
      )

    needs_campus <- vapply(
      seq_along(unknown_rows),
      function(index) {

        same_unit <-
          unknown_units ==
            unknown_units[[index]]

        campuses <- unique(
          unknown_campuses[
            same_unit &
              nzchar(unknown_campuses)
          ]
        )

        length(campuses) > 1L
      },
      logical(1)
    )

    needs_campus <-
      unknown_rows[
        needs_campus
      ]

    if (length(needs_campus) > 0L) {

      catalogue$display_name[
        needs_campus
      ] <- paste0(
        catalogue$report_unit[
          needs_campus
        ],
        " · ",
        catalogue$campus[
          needs_campus
        ]
      )
    }
  }

  # If site codes identify multiple entities that otherwise still have
  # the same display label, use the code only as the final disambiguator.
  duplicated_labels <-
    duplicated(catalogue$display_name) |
    duplicated(
      catalogue$display_name,
      fromLast = TRUE
    )

  coded_duplicates <-
    duplicated_labels &
    !is.na(catalogue$site_code)

  catalogue$display_name[
    coded_duplicates
  ] <- paste0(
    catalogue$display_name[
      coded_duplicates
    ],
    " · ",
    catalogue$site_code[
      coded_duplicates
    ]
  )

  # Preserve familiar configured-site order first, then sort discovered
  # future sites alphabetically.
  known_order <- match(
    catalogue$display_name,
    known_catalogue$display_name
  )

  order_group <- ifelse(
    is.na(known_order),
    2L,
    1L
  )

  order_within <- ifelse(
    is.na(known_order),
    rank(
      tolower(
        catalogue$display_name
      ),
      ties.method = "first"
    ),
    known_order
  )

  catalogue <- catalogue[
    order(
      order_group,
      order_within,
      tolower(
        catalogue$display_name
      )
    ),
    ,
    drop = FALSE
  ]

  rownames(catalogue) <- NULL

  catalogue[
    c(
      "site_key",
      "display_name",
      "report_unit",
      "campus",
      "site_code",
      "known_site"
    )
  ]
}

reporting_monthly_site_catalogue <- function(
    monthly_reports,
    year = NULL
) {

  empty_result <-
    reporting_empty_site_catalogue()

  if (
    is.null(monthly_reports) ||
      !is.list(monthly_reports)
  ) {
    return(empty_result)
  }

  table_names <- c(
    "finance",
    "discounts",
    "total_revenue"
  )

  candidate_rows <- lapply(
    table_names,
    function(table_name) {

      data <- monthly_reports[[table_name]]

      if (
        is.null(data) ||
          !is.data.frame(data) ||
          nrow(data) == 0L ||
          !"site" %in% names(data)
      ) {
        return(NULL)
      }

      if (
        !is.null(year) &&
          length(year) > 0L &&
          "report_year" %in% names(data)
      ) {

        selected_year <- suppressWarnings(
          as.integer(year[[1L]])
        )

        if (!is.na(selected_year)) {

          data <- data[
            !is.na(data$report_year) &
              data$report_year ==
                selected_year,
            ,
            drop = FALSE
          ]
        }
      }

      if (nrow(data) == 0L) {
        return(NULL)
      }

      site <- trimws(
        as.character(
          data$site
        )
      )

      site_code <- if (
        "site_code" %in% names(data)
      ) {
        trimws(
          as.character(
            data$site_code
          )
        )
      } else {
        rep(
          NA_character_,
          nrow(data)
        )
      }

      site_code[
        is.na(site_code) |
          !nzchar(site_code)
      ] <- NA_character_

      data.frame(
        site = site,
        site_code = site_code,
        stringsAsFactors = FALSE
      )
    }
  )

  candidate_rows <- Filter(
    Negate(is.null),
    candidate_rows
  )

  if (length(candidate_rows) == 0L) {
    return(empty_result)
  }

  candidates <- do.call(
    rbind,
    candidate_rows
  )

  valid <- !is.na(candidates$site) &
    nzchar(candidates$site)

  candidates <- candidates[
    valid,
    ,
    drop = FALSE
  ]

  if (nrow(candidates) == 0L) {
    return(empty_result)
  }

  # Organisation-wide summary rows are not selectable sites.
  candidates <- candidates[
    reporting_site_slug(
      candidates$site
    ) != "monash_sport",
    ,
    drop = FALSE
  ]

  if (nrow(candidates) == 0L) {
    return(empty_result)
  }

  candidates$site_group <-
    reporting_site_slug(
      candidates$site
    )

  candidates <- unique(
    candidates
  )

  known_catalogue <- if (
    exists(
      "KNOWN_SITE_CATALOGUE",
      inherits = TRUE
    )
  ) {
    get(
      "KNOWN_SITE_CATALOGUE",
      inherits = TRUE
    )
  } else {
    data.frame(
      display_name = character(),
      report_unit = character(),
      campus = character(),
      stringsAsFactors = FALSE
    )
  }

  known_names <-
    reporting_site_slug(
      known_catalogue$display_name
    )

  grouped <- split(
    candidates,
    candidates$site_group
  )

  catalogue_rows <- lapply(
    grouped,
    function(rows) {

      site_name <-
        rows$site[[1L]]

      site_group <-
        rows$site_group[[1L]]

      known_match <- match(
        site_group,
        known_names
      )

      codes <- sort(
        unique(
          rows$site_code[
            !is.na(rows$site_code) &
              nzchar(rows$site_code)
          ]
        )
      )

      # Existing configured sites retain their established name and mapping.
      if (!is.na(known_match)) {

        site_code <- if (
          length(codes) == 1L
        ) {
          codes[[1L]]
        } else {
          NA_character_
        }

        return(
          data.frame(
            site_key =
              reporting_site_identity_key(
                known_catalogue$report_unit[
                  known_match
                ],
                known_catalogue$campus[
                  known_match
                ]
              ),
            display_name =
              known_catalogue$display_name[
                known_match
              ],
            report_unit =
              known_catalogue$report_unit[
                known_match
              ],
            campus =
              known_catalogue$campus[
                known_match
              ],
            site_code = site_code,
            known_site = TRUE,
            stringsAsFactors = FALSE
          )
        )
      }

      # A newly discovered site with zero or one source code is one site.
      if (length(codes) <= 1L) {

        return(
          data.frame(
            site_key =
              reporting_site_slug(
                site_name
              ),
            display_name = site_name,
            report_unit = site_name,
            campus = NA_character_,
            site_code = if (
              length(codes) == 1L
            ) {
              codes[[1L]]
            } else {
              NA_character_
            },
            known_site = FALSE,
            stringsAsFactors = FALSE
          )
        )
      }

      # Multiple explicit source codes indicate separate source identities.
      data.frame(
        site_key = paste0(
          reporting_site_slug(
            site_name
          ),
          "|",
          reporting_site_slug(
            codes
          )
        ),
        display_name = paste0(
          site_name,
          " · ",
          codes
        ),
        report_unit = site_name,
        campus = NA_character_,
        site_code = codes,
        known_site = FALSE,
        stringsAsFactors = FALSE
      )
    }
  )

  catalogue <- do.call(
    rbind,
    catalogue_rows
  )

  rownames(catalogue) <- NULL

  known_order <- match(
    catalogue$display_name,
    known_catalogue$display_name
  )

  catalogue <- catalogue[
    order(
      is.na(known_order),
      ifelse(
        is.na(known_order),
        Inf,
        known_order
      ),
      tolower(
        catalogue$display_name
      )
    ),
    ,
    drop = FALSE
  ]

  rownames(catalogue) <- NULL

  catalogue
}