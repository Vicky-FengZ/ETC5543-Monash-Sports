# -------------------------------------------------------------------------
# Shared reporting context
# -------------------------------------------------------------------------

stopifnot(
  identical(
    reporting_normalize_years(
      c(
        "2027",
        "2025",
        NA,
        "2025",
        "invalid"
      )
    ),
    c(
      2025L,
      2027L
    )
  )
)


stopifnot(
  identical(
    reporting_table_years(
      data.frame(
        report_year = c(
          2028L,
          2026L,
          2028L
        )
      )
    ),
    c(
      2026L,
      2028L
    )
  )
)


stopifnot(
  length(
    reporting_table_years(
      data.frame(
        value = 1
      )
    )
  ) == 0L
)


# File manifests take precedence over individual tables.
manifest_reports <- list(
  files = data.frame(
    report_year = c(
      2027L,
      2025L
    )
  ),
  attendance = data.frame(
    report_year = 2024L
  )
)

stopifnot(
  identical(
    reporting_report_years(
      manifest_reports,
      fallback_tables = "attendance"
    ),
    c(
      2025L,
      2027L
    )
  )
)


# Older/synthetic objects without a usable file manifest fall back to
# their actual report tables.
fallback_reports <- list(
  files = data.frame(
    report_year = integer()
  ),
  attendance = data.frame(
    report_year = c(
      2026L,
      2024L
    )
  ),
  finance = data.frame(
    report_year = 2025L
  )
)

stopifnot(
  identical(
    reporting_report_years(
      fallback_reports,
      fallback_tables = c(
        "attendance",
        "finance"
      )
    ),
    c(
      2024L,
      2025L,
      2026L
    )
  )
)


# Report-family helpers all use the same manifest contract.
future_monthly <- list(
  files = data.frame(
    report_year = c(
      2027L,
      2028L
    )
  )
)

future_detailed <- list(
  files = data.frame(
    report_year = c(
      2026L,
      2028L
    )
  )
)

future_venues <- list(
  files = data.frame(
    report_year = c(
      2025L,
      2029L
    )
  )
)

stopifnot(
  identical(
    reporting_monthly_years(
      future_monthly
    ),
    c(
      2027L,
      2028L
    )
  ),

  identical(
    reporting_detailed_years(
      future_detailed
    ),
    c(
      2026L,
      2028L
    )
  ),

  identical(
    reporting_venues_years(
      future_venues
    ),
    c(
      2025L,
      2029L
    )
  )
)


stopifnot(
  identical(
    reporting_latest_year(
      c(
        2024L,
        2028L,
        2026L
      )
    ),
    2028L
  ),

  is.na(
    reporting_latest_year(
      integer()
    )
  )
)

# -------------------------------------------------------------------------
# Detailed Site catalogue discovery
# -------------------------------------------------------------------------

site_catalogue_fixture <- list(
  files = data.frame(
    report_year = c(
      2027L,
      2028L,
      2027L,
      2028L,
      2027L,
      2027L
    ),
    report_unit = c(
      "Health & Fitness",
      "Health & Fitness",
      "Esports",
      "Esports",
      "Fitness Centre",
      "Fitness Centre"
    ),
    campus = c(
      "Clayton",
      "Clayton",
      "Clayton",
      "Clayton",
      "Clayton",
      "Caulfield"
    ),
    site_code = c(
      "Z100",
      "Z100",
      "E01",
      "E01",
      "F01",
      "F02"
    ),
    stringsAsFactors = FALSE
  )
)


site_catalogue <-
  reporting_detailed_site_catalogue(
    site_catalogue_fixture
  )


stopifnot(
  nrow(site_catalogue) == 4L,

  identical(
    site_catalogue$display_name,
    c(
      "H&F Clayton",
      "Esports",
      "Fitness Centre · Caulfield",
      "Fitness Centre · Clayton"
    )
  )
)


stopifnot(
  identical(
    site_catalogue$site_key[
      site_catalogue$display_name ==
        "H&F Clayton"
    ],
    "health_fitness|clayton"
  ),

  identical(
    site_catalogue$site_key[
      site_catalogue$display_name ==
        "Esports"
    ],
    "esports|clayton"
  )
)


stopifnot(
  isTRUE(
    site_catalogue$known_site[
      site_catalogue$display_name ==
        "H&F Clayton"
    ]
  ),

  !site_catalogue$known_site[
    site_catalogue$display_name ==
      "Esports"
  ]
)


# The same site across multiple years appears once.
stopifnot(
  sum(
    site_catalogue$display_name ==
      "Esports"
  ) == 1L
)


# A year-specific catalogue uses only reports available in that year.
site_catalogue_2028 <-
  reporting_detailed_site_catalogue(
    site_catalogue_fixture,
    year = 2028L
  )


stopifnot(
  identical(
    site_catalogue_2028$display_name,
    c(
      "H&F Clayton",
      "Esports"
    )
  )
)


# Truly separate source identities at the same unit/campus can use site_code
# as the final discriminator.
coded_site_fixture <- list(
  files = data.frame(
    report_year = c(
      2028L,
      2028L
    ),
    report_unit = c(
      "Racquet Sports",
      "Racquet Sports"
    ),
    campus = c(
      "Clayton",
      "Clayton"
    ),
    site_code = c(
      "R01",
      "R02"
    ),
    stringsAsFactors = FALSE
  )
)


coded_sites <-
  reporting_detailed_site_catalogue(
    coded_site_fixture
  )


stopifnot(
  nrow(coded_sites) == 2L,

  identical(
    sort(
      coded_sites$display_name
    ),
    c(
      "Racquet Sports · R01",
      "Racquet Sports · R02"
    )
  ),

  length(
    unique(
      coded_sites$site_key
    )
  ) == 2L
)

# -------------------------------------------------------------------------
# Monthly Site catalogue discovery
# -------------------------------------------------------------------------

monthly_site_fixture <- list(
  finance = data.frame(
    report_year = c(
      2027L,
      2028L,
      2027L,
      2028L,
      2028L,
      2028L,
      2028L
    ),
    site = c(
      "H&F Clayton",
      "H&F Clayton",
      "Esports",
      "Esports",
      "Monash Sport",
      "Racquet Sports",
      "Racquet Sports"
    ),
    site_code = c(
      "Z100",
      "Z100",
      "E01",
      "E01",
      NA,
      "R01",
      "R02"
    ),
    stringsAsFactors = FALSE
  ),
  discounts = data.frame(
    report_year = c(
      2027L,
      2028L
    ),
    site = c(
      "Esports",
      "Esports"
    ),
    site_code = c(
      "E01",
      "E01"
    ),
    stringsAsFactors = FALSE
  ),
  total_revenue = data.frame(
    report_year = 2028L,
    site = "H&F Clayton",
    site_code = "Z100",
    stringsAsFactors = FALSE
  )
)


monthly_catalogue <-
  reporting_monthly_site_catalogue(
    monthly_site_fixture
  )


stopifnot(
  identical(
    monthly_catalogue$display_name,
    c(
      "H&F Clayton",
      "Esports",
      "Racquet Sports · R01",
      "Racquet Sports · R02"
    )
  )
)


stopifnot(
  !any(
    monthly_catalogue$display_name ==
      "Monash Sport"
  )
)


stopifnot(
  sum(
    monthly_catalogue$display_name ==
      "Esports"
  ) == 1L
)


stopifnot(
  isTRUE(
    monthly_catalogue$known_site[
      monthly_catalogue$display_name ==
        "H&F Clayton"
    ]
  ),

  !monthly_catalogue$known_site[
    monthly_catalogue$display_name ==
      "Esports"
  ]
)


monthly_catalogue_2027 <-
  reporting_monthly_site_catalogue(
    monthly_site_fixture,
    year = 2027L
  )


stopifnot(
  identical(
    monthly_catalogue_2027$display_name,
    c(
      "H&F Clayton",
      "Esports"
    )
  )
)

cat(
  "PASS shared reporting context years\n"
)