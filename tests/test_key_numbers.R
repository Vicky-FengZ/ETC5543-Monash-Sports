# Key-number checks for the full report set parsed in
# test_real_upload_parsers.R (objects `monthly`, `venues` and `sites`).
#
# 1. Snapshot: the dashboard's annual figures and the totals of every
#    cleaned table are compared with the last accepted run, so a change to
#    the import or calculation code cannot alter the numbers unnoticed.
#    - Sample workbooks: tests/snapshots/sample_key_numbers.csv (committed).
#    - Real reports: dashboard_key_numbers.csv inside the data folder
#      (data/raw is never committed), created on the first run.
#    After an intended change, rerun with MONASH_SPORT_UPDATE_KEY_NUMBERS=true
#    and review the differences before keeping the new file.
# 2. Expected values: optional expected_values.csv in the data folder with
#    figures confirmed against the Excel reports by Monash Sport staff
#    (columns check, key, expected, tolerance; keys as in the snapshot).

key_number_root <- Sys.getenv("MONASH_SPORT_REAL_DATA_ROOT", unset = "")

if (!nzchar(key_number_root)) {
  cat("SKIP test_key_numbers.R (MONASH_SPORT_REAL_DATA_ROOT is not set)\n")
} else {
  key_number_id_columns <- c("report_year", "salary_year", "year", "month")

  # One row per figure: id columns form the key, numeric columns the values.
  key_number_rows <- function(frame, check) {
    if (!is.data.frame(frame) || nrow(frame) == 0L) {
      return(NULL)
    }
    numeric <- vapply(frame, is.numeric, logical(1)) &
      !names(frame) %in% key_number_id_columns
    ids <- names(frame)[!numeric]
    id_text <- if (length(ids) > 0L) {
      do.call(paste, c(lapply(frame[ids], as.character), sep = " | "))
    } else {
      rep("all", nrow(frame))
    }
    do.call(rbind, lapply(names(frame)[numeric], function(column) {
      data.frame(
        check = check,
        key = paste(id_text, column, sep = " | "),
        value = as.numeric(frame[[column]]),
        stringsAsFactors = FALSE
      )
    }))
  }

  # Row count and column totals of each cleaned table, per report year.
  table_total_rows <- function(report, report_type) {
    tables <- c(BASIC_UPLOAD_REPORTED_TABLES[[report_type]], "total_revenue")
    do.call(rbind, lapply(tables, function(table_name) {
      table <- report[[table_name]]
      if (!is.data.frame(table) || nrow(table) == 0L ||
          !"report_year" %in% names(table)) {
        return(NULL)
      }
      numeric <- names(table)[vapply(table, is.numeric, logical(1)) &
        !names(table) %in% c(key_number_id_columns, "source_row", "source_column")]
      do.call(rbind, lapply(split(table, table$report_year), function(part) {
        year <- part$report_year[[1L]]
        data.frame(
          check = "Table totals",
          key = paste(report_type, table_name, year, c("rows", numeric), sep = " | "),
          value = c(
            nrow(part),
            vapply(numeric, function(column) sum(part[[column]], na.rm = TRUE), numeric(1))
          ),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }

  current_key_numbers <- rbind(
    key_number_rows(calculate_overview_annual_finance(monthly), "Overview annual finance"),
    key_number_rows(calculate_site_annual_finance(monthly), "Site annual finance"),
    table_total_rows(monthly, "monthly_site"),
    table_total_rows(sites, "site_detail"),
    table_total_rows(venues, "venues_tracking")
  )
  rownames(current_key_numbers) <- NULL
  stopifnot(
    nrow(current_key_numbers) > 100L,
    !anyDuplicated(paste(current_key_numbers$check, current_key_numbers$key))
  )

  # In the real reports the organisation summary equals the sum of the site
  # summaries, so the Site Values on the Executive Dashboard add up to its
  # KPIs. (The synthetic sample figures are random and do not add up.)
  is_sample_root <- identical(
    normalizePath(key_number_root, winslash = "/", mustWork = FALSE),
    normalizePath(file.path("data", "sample"), winslash = "/", mustWork = FALSE)
  )
  if (!is_sample_root) {
    organisation <- calculate_overview_annual_finance(monthly)
    site_totals <- stats::aggregate(
      calculate_site_annual_finance(monthly)[c("income_actual", "expenses_actual")],
      calculate_site_annual_finance(monthly)["report_year"],
      sum,
      na.rm = TRUE
    )
    paired <- merge(organisation, site_totals, by = "report_year", suffixes = c("", "_sites"))
    gap <- abs(paired$income_actual - paired$income_actual_sites) > 1 |
      abs(paired$expenses_actual - paired$expenses_actual_sites) > 1
    if (any(gap)) {
      stop("Site totals do not add up to the Monash Sport summary in: ",
        paste(paired$report_year[gap], collapse = ", "), call. = FALSE)
    }
    cat("PASS site totals add up to the organisation summary\n")
  }

  snapshot_path <- if (is_sample_root) {
    file.path("tests", "snapshots", "sample_key_numbers.csv")
  } else {
    file.path(key_number_root, "dashboard_key_numbers.csv")
  }

  write_key_numbers <- function(numbers, path) {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    numbers$value <- ifelse(is.na(numbers$value), "", sprintf("%.15g", numbers$value))
    utils::write.csv(numbers, path, row.names = FALSE, na = "")
  }

  read_key_numbers <- function(path) {
    numbers <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    numbers$value <- suppressWarnings(as.numeric(numbers$value))
    numbers
  }

  same_number <- function(a, b, tolerance) {
    (is.na(a) & is.na(b)) |
      (!is.na(a) & !is.na(b) & abs(a - b) <= tolerance)
  }

  update <- tolower(Sys.getenv("MONASH_SPORT_UPDATE_KEY_NUMBERS")) %in% c("true", "1", "yes")

  if (!file.exists(snapshot_path) || update) {
    write_key_numbers(current_key_numbers, snapshot_path)
    cat("SAVED key-number snapshot:", snapshot_path, "(", nrow(current_key_numbers), "figures )\n")
  } else {
    accepted <- read_key_numbers(snapshot_path)
    compared <- merge(
      accepted, current_key_numbers,
      by = c("check", "key"), all = TRUE, suffixes = c("_accepted", "_now")
    )
    tolerance <- 1e-6 * pmax(1, abs(compared$value_accepted), abs(compared$value_now), na.rm = TRUE)
    present <- !is.na(match(paste(compared$check, compared$key),
      paste(accepted$check, accepted$key))) &
      !is.na(match(paste(compared$check, compared$key),
        paste(current_key_numbers$check, current_key_numbers$key)))
    changed <- !present | !same_number(compared$value_accepted, compared$value_now, tolerance)

    if (any(changed)) {
      shown <- compared[changed, , drop = FALSE]
      stop(
        sum(changed), " key figure(s) differ from ", snapshot_path, ":\n",
        paste(utils::capture.output(print(utils::head(shown, 15L), row.names = FALSE)), collapse = "\n"),
        "\nIf the change is intended, rerun with MONASH_SPORT_UPDATE_KEY_NUMBERS=true ",
        "and review the new file.",
        call. = FALSE
      )
    }
    cat("PASS key-number snapshot (", nrow(accepted), "figures )\n")
  }

  expected_path <- if (is_sample_root) {
    file.path("tests", "snapshots", "sample_expected_values.csv")
  } else {
    file.path(key_number_root, "expected_values.csv")
  }
  if (file.exists(expected_path)) {
    expected <- utils::read.csv(expected_path, stringsAsFactors = FALSE, check.names = FALSE)
    stopifnot(all(c("check", "key", "expected") %in% names(expected)))
    if (!"tolerance" %in% names(expected)) expected$tolerance <- 1
    expected$tolerance[is.na(expected$tolerance)] <- 1
    now <- current_key_numbers$value[match(
      paste(expected$check, expected$key),
      paste(current_key_numbers$check, current_key_numbers$key)
    )]
    wrong <- is.na(now) | !same_number(as.numeric(expected$expected), now, expected$tolerance)
    if (any(wrong)) {
      stop(
        sum(wrong), " figure(s) differ from the values confirmed in ", expected_path, ":\n",
        paste0("  ", expected$check[wrong], " | ", expected$key[wrong],
          ": expected ", expected$expected[wrong], ", dashboard ",
          ifelse(is.na(now[wrong]), "(missing)", format(now[wrong], big.mark = ",")),
          collapse = "\n"),
        call. = FALSE
      )
    }
    cat("PASS expected values (", nrow(expected), "figures )\n")
  } else {
    cat("SKIP expected values (no", expected_path, ")\n")
  }

  cat("PASS test_key_numbers.R\n")
}
