# -------------------------------------------------------------------------
# Basic Data Upload module
# -------------------------------------------------------------------------
# Session-only upload, parsing progress and file deletion. Parsed data can be
# saved to a file on the user's own computer and loaded again later (see
# "Saved data" below); nothing is stored on a server.

BASIC_UPLOAD_REPORT_TYPES <- c(
  monthly_site = "Monthly Site Reports",
  site_detail = "Detailed Site Reports",
  venues_tracking = "Venues Tracking Reports"
)

# Report type of each uploaded file, recognised from its file name with the
# same rules as the folder importers (see docs/user_guide.md). NA for files
# that are not Excel workbooks or whose name matches no report.
classify_report_upload <- function(file_names) {

  # Upload names are plain file names. basename() is avoided: on Windows it
  # tilde-expands names such as "~$Report.xlsx" (Excel lock files).
  names <- as.character(file_names)
  excel <- grepl("\\.(xlsx|xls)$", names, ignore.case = TRUE) &
    !startsWith(names, "~$")
  type <- rep(NA_character_, length(names))
  type[excel & grepl("(Site|Program)[[:space:]_-]*Report", names, ignore.case = TRUE)] <-
    "site_detail"
  type[excel & grepl("Venues[[:space:]_-]*Tracking[[:space:]_-]*Report", names,
    ignore.case = TRUE)] <- "venues_tracking"
  type[excel & grepl("^Monthly[[:space:]_-]*Site[[:space:]_-]*Report", names,
    ignore.case = TRUE)] <- "monthly_site"
  type
}


# Hint added to a failed file when its name suggests another upload area.
basic_upload_mismatch_hint <- function(file_name, report_type) {

  guessed <- classify_report_upload(file_name)

  if (is.na(guessed) || identical(guessed, report_type)) {
    return("")
  }

  paste0(
    " The file name looks like one of the ",
    BASIC_UPLOAD_REPORT_TYPES[[guessed]],
    "; try Upload all reports or the ",
    BASIC_UPLOAD_REPORT_TYPES[[guessed]],
    " area."
  )
}


# Client-side lock and messages for the upload area. Selecting files locks
# every upload button until the server reports that parsing has finished,
# because a second upload sent while the first is parsing fails in the
# browser version (Shinylive). A failed transfer is explained in plain words.
basic_upload_script <- function() {
  tags$script(HTML("
(function () {
  if (window.monashUploadArea) return;
  window.monashUploadArea = true;

  function areaOf(node) { return node && node.closest ? node.closest('.basic-upload-area') : null; }

  function setLocked(area, locked) {
    area.classList.toggle('is-locked', locked);
    area.querySelectorAll('input[type=file]').forEach(function (input) { input.disabled = locked; });
  }

  function showBanner(area, kind, title, text) {
    var banner = area.querySelector('.basic-upload-banner');
    if (!banner) return;
    banner.className = 'basic-upload-banner is-' + kind;
    banner.textContent = '';
    var strong = document.createElement('strong');
    strong.textContent = title;
    var span = document.createElement('span');
    span.textContent = text;
    banner.appendChild(strong);
    banner.appendChild(span);
    banner.hidden = false;
  }

  document.addEventListener('change', function (event) {
    var input = event.target;
    if (!input.matches || !input.matches('.basic-upload-area input[type=file]')) return;
    var count = input.files ? input.files.length : 0;
    if (!count) return;
    var area = areaOf(input);
    area.querySelectorAll('.progress-bar').forEach(function (bar) { delete bar.dataset.uploadFailed; });
    setLocked(area, true);
    if (input.closest('.basic-saved-data-card')) {
      showBanner(area, 'busy', 'Loading saved data\u2026',
        'Keep this page open. Other uploads are paused until this finishes.');
      return;
    }
    showBanner(area, 'busy',
      'Uploading and parsing ' + count + (count === 1 ? ' file' : ' files') + '\u2026',
      'Keep this page open. Other uploads are paused until this finishes; ' +
      'large batches can take a few minutes.');
  });

  // Shiny marks a failed transfer with a red progress bar reading 'error'.
  var watcher = new MutationObserver(function (records) {
    records.forEach(function (record) {
      var node = record.target.nodeType === 3 ? record.target.parentElement : record.target;
      var bar = node && node.closest ? node.closest('.progress-bar') : null;
      var area = areaOf(bar);
      if (!bar || !area || bar.dataset.uploadFailed) return;
      var failed = bar.classList.contains('progress-bar-danger') ||
        /^\\s*error/i.test(bar.textContent || '');
      if (!failed) return;
      bar.dataset.uploadFailed = '1';
      setLocked(area, false);
      showBanner(area, 'error', 'The upload did not finish.',
        'These files did not reach the dashboard and were not parsed. Wait until ' +
        'any parsing has finished, then select the files again.');
    });
  });

  function watchAreas() {
    document.querySelectorAll('.basic-upload-area').forEach(function (area) {
      watcher.observe(area, { subtree: true, childList: true, characterData: true,
        attributes: true, attributeFilter: ['class'] });
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', watchAreas);
  } else {
    watchAreas();
  }

  // The browser's UTC offset, so saved-data dates follow the user's clock.
  if (window.jQuery) {
    jQuery(document).on('shiny:connected', function () {
      Shiny.setInputValue('monash_client_utc_offset', new Date().getTimezoneOffset());
    });
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('basic-upload-finished', function (message) {
      var area = document.getElementById(message.area);
      if (!area) return;
      setLocked(area, false);
      var banner = area.querySelector('.basic-upload-banner');
      if (banner && banner.classList.contains('is-busy')) banner.hidden = true;
    });
  }
})();
"))
}


attach_basic_upload_sheet_order <- function(result, path) {
  existing_order <- attr(result, "source_sheet_order", exact = TRUE)

  if (length(existing_order) > 0L) {
    return(result)
  }

  sheet_order <- tryCatch(
    readxl::excel_sheets(path),
    error = function(error) character()
  )
  attr(result, "source_sheet_order") <- as.character(sheet_order)
  result
}


empty_basic_upload_registry <- function() {
  data.frame(
    file_id = character(),
    report_type = character(),
    original_name = character(),
    stored_path = character(),
    size = double(),
    status = character(),
    progress = double(),
    detail = character(),
    warning_count = integer(),
    error_message = character(),
    stringsAsFactors = FALSE
  )
}


basic_upload_parsers <- function() {
  # Each parser runs the report's importer, then the upload checks below.
  run <- function(loader, report_type) {
    function(path, source_name) {
      result <- loader(paths = path, source_names = source_name, strict = FALSE)
      result <- attach_basic_upload_sheet_order(result, path)
      add_basic_upload_checks(result, path, report_type, source_name)
    }
  }

  list(
    monthly_site = run(load_monthly_site_reports, "monthly_site"),
    site_detail = run(load_business_unit_template_reports, "site_detail"),
    venues_tracking = run(load_venues_tracking_reports, "venues_tracking")
  )
}


# Upload checks -----------------------------------------------------------
# Checks that only make sense for one uploaded workbook at a time: the year
# in the file name must match the year inside the workbook, and the
# workbook must contain some figures (a blank template adds an empty year).

# Tables that hold figures typed into each report (calculated checks are
# left out, because a blank template still produces them).
BASIC_UPLOAD_REPORTED_TABLES <- list(
  monthly_site = c("finance", "discounts"),
  site_detail = c("finance", "discounts", "visits", "memberships", "enrollments"),
  venues_tracking = c(
    "finance", "attendance", "facility_hire", "membership", "online_sales",
    "merchandise", "casual_hours", "casual_wages"
  )
)

BASIC_UPLOAD_ID_COLUMNS <- c(
  "report_year", "salary_year", "month", "source_row", "source_column"
)


# Number of reported figures (non-missing numeric cells) in a result.
basic_upload_reported_values <- function(result, report_type) {
  tables <- BASIC_UPLOAD_REPORTED_TABLES[[report_type]]
  sum(vapply(
    tables,
    function(table_name) {
      table <- result[[table_name]]
      if (!is.data.frame(table) || nrow(table) == 0L) {
        return(0L)
      }
      numeric <- vapply(table, is.numeric, logical(1)) &
        !names(table) %in% BASIC_UPLOAD_ID_COLUMNS
      sum(!is.na(unlist(table[numeric], use.names = FALSE)))
    },
    integer(1)
  ))
}


# Report year found inside the workbook, or NA when it cannot be told.
# Monthly Site Reports: the month headings of the Finance Summary sheet are
# dates, and at least three quarters of them must share one year.
# Venues Tracking Reports: sheet names such as "Court Hire 2024 vs 2025" and
# "2025 Casual Hours". Detailed Site Reports carry no year inside.
basic_upload_content_year <- function(path, report_type) {
  tryCatch(
    {
      sheets <- readxl::excel_sheets(path)

      if (identical(report_type, "venues_tracking")) {
        versus <- regmatches(
          sheets,
          regexpr("vs[[:space:]]*(19|20)[0-9]{2}[[:space:]]*$", sheets, ignore.case = TRUE)
        )
        casual <- regmatches(
          sheets,
          regexpr(
            "^[[:space:]]*(19|20)[0-9]{2}(?=[[:space:]]+Casual Hours)",
            sheets,
            perl = TRUE,
            ignore.case = TRUE
          )
        )
        years <- unique(suppressWarnings(as.integer(c(
          gsub("[^0-9]", "", versus),
          trimws(casual)
        ))))
        years <- years[!is.na(years)]
        return(if (length(years) == 1L) years else NA_integer_)
      }

      if (identical(report_type, "monthly_site")) {
        sheet <- grep("finance summary", sheets, ignore.case = TRUE, value = TRUE)[1]
        if (is.na(sheet)) {
          return(NA_integer_)
        }
        cells <- suppressMessages(readxl::read_excel(
          path,
          sheet = sheet,
          col_names = FALSE,
          n_max = 40,
          col_types = "list",
          .name_repair = "minimal"
        ))
        years <- unlist(lapply(cells, function(column) {
          unlist(lapply(column, function(value) {
            if (inherits(value, c("POSIXct", "Date"))) {
              as.integer(format(value, "%Y"))
            }
          }))
        }))
        if (length(years) < 6L) {
          return(NA_integer_)
        }
        counts <- table(years)
        if (max(counts) / length(years) < 0.75) {
          return(NA_integer_)
        }
        return(as.integer(names(counts)[which.max(counts)]))
      }

      NA_integer_
    },
    error = function(error) NA_integer_
  )
}


basic_upload_name_year <- function(result, source_name) {
  files <- result$files
  years <- if (is.data.frame(files) && "report_year" %in% names(files)) {
    files$report_year
  } else {
    basic_saved_result_years(result, source_name)
  }
  years <- unique(suppressWarnings(as.integer(years)))
  years <- years[!is.na(years)]
  if (length(years) == 1L) years else NA_integer_
}


add_basic_upload_checks <- function(result, path, report_type, source_name) {
  issues <- list()
  name_year <- basic_upload_name_year(result, source_name)
  content_year <- basic_upload_content_year(path, report_type)

  if (!is.na(name_year) && !is.na(content_year) && name_year != content_year) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "error",
      source_file = source_name,
      message = paste0(
        "The file name says ", name_year, ", but the workbook holds ",
        content_year, " figures. Rename the file to the correct year ",
        "(", content_year, "), then upload it again."
      )
    )
  }

  if (basic_upload_reported_values(result, report_type) == 0L) {
    issues[[length(issues) + 1L]] <- make_import_issue(
      level = "error",
      source_file = source_name,
      message = paste(
        "This workbook has no figures yet; it looks like a blank template.",
        "Upload it once at least one month has been filled in."
      )
    )
  }

  if (length(issues) > 0L) {
    result$validation <- bind_import_tables(
      c(list(result$validation), issues),
      empty_import_issue_table()
    )
  }

  result
}


# What a ready file covers: one key per report year (and per business unit
# for Detailed Site Reports). Two files with the same key would be counted
# twice, so only one of them may be used.
basic_upload_coverage <- function(result, report_type, file_name) {
  files <- result$files
  years <- if (is.data.frame(files) && "report_year" %in% names(files)) {
    files$report_year
  } else {
    basic_saved_result_years(result, file_name)
  }
  years <- suppressWarnings(as.integer(years))
  units <- if (identical(report_type, "site_detail") &&
    is.data.frame(files) && "report_unit" %in% names(files) &&
    length(files$report_unit) == length(years)) {
    as.character(files$report_unit)
  } else {
    rep(NA_character_, length(years))
  }
  keep <- !is.na(years)
  years <- years[keep]
  units <- units[keep]

  if (length(years) == 0L) {
    return(character())
  }

  labels <- paste0(
    BASIC_UPLOAD_REPORT_TYPES[[report_type]],
    " for ",
    ifelse(is.na(units), "", paste0(units, " ")),
    years
  )
  keys <- paste(report_type, tolower(trimws(units)), years, sep = "|")
  labels <- labels[!duplicated(keys)]
  stats::setNames(labels, unique(keys))
}


# Message for a file that covers a year already provided by another ready
# file, or NULL. `others` are registry rows; `results` their parsed data.
basic_upload_conflict <- function(result, report_type, file_name, others, results) {
  coverage <- basic_upload_coverage(result, report_type, file_name)

  if (length(coverage) == 0L || nrow(others) == 0L) {
    return(NULL)
  }

  for (index in seq_len(nrow(others))) {
    other <- results[[others$file_id[[index]]]]
    if (is.null(other)) {
      next
    }
    shared <- intersect(
      names(coverage),
      names(basic_upload_coverage(other, others$report_type[[index]], others$original_name[[index]]))
    )
    if (length(shared) > 0L) {
      return(paste0(
        "Not used: the ", coverage[[shared[[1L]]]], " are already loaded from ",
        "\u201c", others$original_name[[index]], "\u201d. Using both files would ",
        "count the same figures twice. Keep one file per year: remove the ",
        "other file first, or give this file the same name so it replaces it."
      ))
    }
  }

  NULL
}


# Saved data --------------------------------------------------------------
# "Save current data" writes the cleaned results of every ready file to one
# .rds file on the user's computer; "Load saved data" restores them without
# reading the Excel workbooks again. The file carries a manifest (source
# files, report years, save date) so its contents can be checked on load.

BASIC_SAVED_DATA_FORMAT <- "monash-sport-saved-data"

# Increase this when the cleaned tables produced by the importers change
# shape. Files saved by another version are then refused with a clear
# message instead of breaking the dashboard pages.
BASIC_SAVED_DATA_VERSION <- 1L

# Saved data older than this is flagged as possibly out of date.
BASIC_SAVED_DATA_STALE_DAYS <- 30L

BASIC_UPLOAD_RESULT_CLASSES <- c(
  monthly_site = "monthly_site_report_data",
  site_detail = "business_unit_template_report_data",
  venues_tracking = "venues_tracking_report_data"
)


# Reporting years in a parsed result, from its report_year columns, or from
# a four-digit year in the file name when the tables do not carry one.
basic_saved_result_years <- function(result, file_name = NA_character_) {
  tables <- Filter(is.data.frame, unclass(result))
  years <- unlist(lapply(tables, function(table) {
    if ("report_year" %in% names(table)) {
      suppressWarnings(as.integer(as.character(table$report_year)))
    }
  }), use.names = FALSE)
  years <- sort(unique(years[!is.na(years)]))

  if (length(years) == 0L && !is.na(file_name)) {
    match <- regmatches(file_name, regexpr("(19|20)[0-9]{2}", file_name))
    years <- suppressWarnings(as.integer(match))
  }

  years
}


# Compact year list: 2021-2025, or 2021-2022, 2025 when years are missing.
format_basic_year_span <- function(years, dash = "–", join = ", ") {
  years <- sort(unique(suppressWarnings(as.integer(unlist(years)))))
  years <- years[!is.na(years)]

  if (length(years) == 0L) {
    return("")
  }

  ends <- c(which(diff(years) != 1L), length(years))
  starts <- c(1L, utils::head(ends, -1L) + 1L)
  paste(
    ifelse(
      starts == ends,
      years[starts],
      paste0(years[starts], dash, years[ends])
    ),
    collapse = join
  )
}


basic_saved_data_file_table <- function(rows, results) {
  data.frame(
    original_name = as.character(rows$original_name),
    report_type = as.character(rows$report_type),
    years = vapply(
      seq_len(nrow(rows)),
      function(index) {
        paste(
          basic_saved_result_years(results[[index]], rows$original_name[[index]]),
          collapse = ", "
        )
      },
      character(1)
    ),
    size = as.numeric(rows$size),
    warning_count = as.integer(rows$warning_count),
    detail = as.character(rows$detail),
    stringsAsFactors = FALSE
  )
}


# Files and years per report type, in the dashboard's report-type order.
basic_saved_data_summary <- function(files) {
  types <- intersect(names(BASIC_UPLOAD_REPORT_TYPES), unique(files$report_type))

  data.frame(
    report_type = types,
    label = unname(BASIC_UPLOAD_REPORT_TYPES[types]),
    files = vapply(types, function(type) sum(files$report_type == type), integer(1)),
    years = vapply(
      types,
      function(type) {
        format_basic_year_span(strsplit(
          files$years[files$report_type == type],
          ",[[:space:]]*"
        ))
      },
      character(1)
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


basic_saved_data_summary_text <- function(files) {
  summary <- basic_saved_data_summary(files)
  paste0(
    summary$label, ": ", summary$files,
    ifelse(summary$files == 1L, " file", " files"),
    ifelse(nzchar(summary$years), paste0(" (", summary$years, ")"), "")
  )
}


# e.g. monash-sport-saved-data_2021-2025_39-files_saved-2026-10-31.rds
basic_saved_data_filename <- function(files, saved_at = Sys.time(), utc_offset = NULL) {
  years <- format_basic_year_span(
    strsplit(files$years, ",[[:space:]]*"),
    dash = "-",
    join = "+"
  )
  count <- nrow(files)

  paste0(
    "monash-sport-saved-data_",
    if (nzchar(years)) paste0(years, "_"),
    count, if (count == 1L) "-file" else "-files",
    "_saved-", format(basic_saved_local_date(saved_at, utc_offset), "%Y-%m-%d"),
    ".rds"
  )
}


# Everything needed to restore the ready files: cleaned results, warning
# notes and a manifest. Raw workbooks are never included.
build_basic_saved_data <- function(rows, results, warnings, saved_at = Sys.time()) {
  ready <- rows$status == "ready" & rows$file_id %in% names(results)
  rows <- rows[ready, , drop = FALSE]
  ordered_results <- unname(results[rows$file_id])

  list(
    format = BASIC_SAVED_DATA_FORMAT,
    version = BASIC_SAVED_DATA_VERSION,
    saved_at = saved_at,
    files = basic_saved_data_file_table(rows, ordered_results),
    results = ordered_results,
    warnings = lapply(rows$file_id, function(file_id) warnings[[file_id]])
  )
}


# Calendar date of a time on the user's clock. The browser version of R runs
# in UTC, so the browser's UTC offset (minutes, as from JavaScript's
# getTimezoneOffset()) is applied when it is known.
basic_saved_local_date <- function(time, utc_offset = NULL) {
  if (inherits(time, "Date")) {
    return(time)
  }
  offset <- suppressWarnings(as.numeric(utc_offset))
  if (length(offset) == 1L && !is.na(offset)) {
    return(as.Date(format(time - offset * 60, "%Y-%m-%d", tz = "UTC")))
  }
  as.Date(format(time, "%Y-%m-%d"))
}


basic_saved_data_days_old <- function(saved_at, today = Sys.time(), utc_offset = NULL) {
  as.integer(
    basic_saved_local_date(today, utc_offset) -
      basic_saved_local_date(saved_at, utc_offset)
  )
}


# Reads and checks a saved-data file. Stops with a message written for the
# person who chose the file.
read_basic_saved_data <- function(path) {
  not_saved_data <- paste(
    "This file is not saved dashboard data. Choose a .rds file created",
    "with \"Save current data\"."
  )
  saved <- tryCatch(readRDS(path), error = function(error) NULL)

  if (!is.list(saved) || !identical(saved$format, BASIC_SAVED_DATA_FORMAT)) {
    stop(not_saved_data, call. = FALSE)
  }

  version <- suppressWarnings(as.integer(saved$version))
  saved_on <- tryCatch(
    format(basic_saved_local_date(saved$saved_at), "%d %b %Y"),
    error = function(error) "an unknown date"
  )

  if (length(version) != 1L || is.na(version)) {
    stop(not_saved_data, call. = FALSE)
  }
  if (version > BASIC_SAVED_DATA_VERSION) {
    stop(
      "This file was saved by a newer version of the dashboard. Reload the ",
      "page to get the latest version, then load the file again.",
      call. = FALSE
    )
  }
  if (version < BASIC_SAVED_DATA_VERSION) {
    stop(
      "This file was saved on ", saved_on, " by an older version of the ",
      "dashboard, and its cleaned tables no longer match this version. ",
      "Upload the original Excel reports again, then save a new copy.",
      call. = FALSE
    )
  }

  files <- saved$files
  required <- c("original_name", "report_type", "years", "size", "warning_count")
  complete <- is.data.frame(files) &&
    all(required %in% names(files)) &&
    all(files$report_type %in% names(BASIC_UPLOAD_RESULT_CLASSES)) &&
    is.list(saved$results) &&
    length(saved$results) == nrow(files) &&
    length(saved$warnings) == nrow(files) &&
    inherits(saved$saved_at, c("POSIXct", "Date")) &&
    all(vapply(
      seq_len(nrow(files)),
      function(index) {
        inherits(
          saved$results[[index]],
          BASIC_UPLOAD_RESULT_CLASSES[[files$report_type[[index]]]]
        )
      },
      logical(1)
    ))

  if (!isTRUE(complete)) {
    stop(
      "This saved file is incomplete or damaged, so nothing was loaded. ",
      "Upload the original Excel reports instead.",
      call. = FALSE
    )
  }

  saved
}


basic_upload_result_issues <- function(result) {
  validation <- result$validation

  if (!is.data.frame(validation) || nrow(validation) == 0L) {
    return(list(errors = 0L, warnings = 0L, first_error = NA_character_))
  }

  levels <- tolower(as.character(validation$level))
  error_rows <- !is.na(levels) & levels == "error"
  warning_rows <- !is.na(levels) & levels == "warning"
  first_error <- if (any(error_rows)) {
    as.character(validation$message[which(error_rows)[[1L]]])
  } else {
    NA_character_
  }

  list(
    errors = sum(error_rows),
    warnings = sum(warning_rows),
    first_error = first_error
  )
}


basic_upload_warning_impact <- function(message) {
  text <- tolower(message)

  if (grepl("retained as additional data", text, fixed = TRUE)) {
    return("Open this worksheet in View Cleaned Data to inspect the retained values and their source positions.")
  }
  if (grepl("no configured data layout", text, fixed = TRUE)) {
    return("This worksheet needs a layout rule before its contents can appear in the cleaned tables.")
  }

  if (startsWith(text, "sheet ") && grepl("was skipped", text, fixed = TRUE)) {
    return("This worksheet was skipped. Other readable worksheets may still be available.")
  }
  if (grepl("extraction was skipped", text, fixed = TRUE)) {
    return("This part of the worksheet was not extracted. Review the source layout before using these figures.")
  }
  if (grepl("discounts section was not found", text, fixed = TRUE)) {
    return("Discount opportunity data are unavailable for this worksheet; Finance can still be viewed.")
  }
  if (grepl("total revenue (actual + opp) row was not found", text, fixed = TRUE)) {
    return("The reported revenue cannot be checked against income and discounts for this worksheet.")
  }
  if (grepl("finance summary sheet was not found", text, fixed = TRUE)) {
    return("The organisation summary is missing. Check site coverage before using organisation-wide totals.")
  }
  if (grepl("validation failed", text, fixed = TRUE)) {
    return("Calculated and reported figures differ. Review the source totals before using these figures.")
  }
  if (grepl("validation was incomplete|validation incomplete", text)) {
    return("Some checks could not be completed. Review missing inputs before using totals.")
  }
  if (grepl("duplicate", text, fixed = TRUE)) {
    return("Some period labels or records repeat. Check the source before adding up these figures.")
  }
  if (grepl("missing month", text, fixed = TRUE)) {
    return("Some expected months were not identified. Coverage may be incomplete.")
  }
  if (grepl("no reported total|no total .*block|no ytd", text)) {
    return("A reported total was not identified, so the corresponding total check may be unavailable.")
  }
  if (grepl("sheet was not found|sheet is empty", text)) {
    return("This part of the report has no extracted data. Other available parts can still be viewed.")
  }
  if (grepl("no recognised|no recognizable|no recognisable|could not be identified", text)) {
    return("The expected layout was not identified; some data or checks may be unavailable.")
  }

  "Review this note against the source workbook before using the affected figures."
}


basic_upload_warning_rows <- function(result) {
  empty <- data.frame(
    sheet = character(), message = character(), impact = character(),
    stringsAsFactors = FALSE
  )
  validation <- result$validation

  if (!is.data.frame(validation) || nrow(validation) == 0L ||
      !"level" %in% names(validation)) {
    return(empty)
  }

  levels <- tolower(as.character(validation$level))
  rows <- validation[!is.na(levels) & levels == "warning", , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(empty)
  }

  sheets <- if ("sheet" %in% names(rows)) {
    trimws(as.character(rows$sheet))
  } else {
    rep(NA_character_, nrow(rows))
  }
  messages <- if ("message" %in% names(rows)) {
    as.character(rows$message)
  } else {
    rep(NA_character_, nrow(rows))
  }
  sheets[is.na(sheets) | !nzchar(sheets)] <- "File-level notes"
  messages[is.na(messages) | !nzchar(trimws(messages))] <-
    "No warning description was supplied by the importer."

  data.frame(
    sheet = sheets,
    message = messages,
    impact = vapply(messages, basic_upload_warning_impact, character(1L)),
    stringsAsFactors = FALSE
  )
}


basic_upload_warning_details_ui <- function(warnings, file_name) {
  if (is.null(warnings) || nrow(warnings) == 0L) {
    return(NULL)
  }

  count <- nrow(warnings)
  label <- paste(count, if (count == 1L) "warning" else "warnings")
  tags$details(
    class = "basic-upload-warning-details",
    tags$summary(
      `aria-label` = paste("View", label, "for", file_name),
      bs_icon("chevron-right", class = "basic-upload-warning-chevron", size = "12px"),
      span(class = "basic-upload-warning-count", label),
      span(class = "basic-upload-warning-toggle", "View details")
    ),
    div(
      class = "basic-upload-warning-body",
      lapply(unique(warnings$sheet), function(sheet) {
        rows <- warnings[warnings$sheet == sheet, , drop = FALSE]
        div(
          class = "basic-upload-warning-group",
          div(class = "basic-upload-warning-group-heading", sheet),
          tags$ul(lapply(seq_len(nrow(rows)), function(index) {
            tags$li(
              p(class = "basic-upload-warning-message", rows$message[[index]]),
              p(
                class = "basic-upload-warning-impact",
                span("What this means: "), rows$impact[[index]]
              )
            )
          }))
        )
      })
    )
  )
}


combine_basic_upload_results <- function(objects, result_class) {
  objects <- Filter(Negate(is.null), objects)

  if (length(objects) == 0L) {
    return(NULL)
  }

  if (length(objects) == 1L) {
    return(objects[[1L]])
  }

  fields <- unique(unlist(lapply(objects, names), use.names = FALSE))
  combined <- lapply(
    fields,
    function(field) {
      values <- lapply(objects, function(object) object[[field]])

      if (all(vapply(values, is.data.frame, logical(1L)))) {
        value <- do.call(rbind, values)
        rownames(value) <- NULL
        return(value)
      }

      values
    }
  )
  names(combined) <- fields
  sheet_order <- unique(unlist(
    lapply(
      objects,
      function(object) attr(object, "source_sheet_order", exact = TRUE)
    ),
    use.names = FALSE
  ))
  structure(
    combined,
    class = result_class,
    source_sheet_order = as.character(sheet_order)
  )
}


BASIC_PREVIEW_REPORT_FIELDS <- c(
  monthly_site = "monthly_reports",
  site_detail = "site_reports",
  venues_tracking = "venues_reports"
)

BASIC_PREVIEW_TABLES <- list(
  monthly_site = c("finance", "discounts", "total_revenue"),
  site_detail = c(
    "finance",
    "discounts",
    "total_revenue",
    "visits",
    "memberships",
    "enrollments"
  ),
  venues_tracking = c(
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

BASIC_PREVIEW_ALL_SHEETS <- "__all_source_sheets__"


basic_preview_report <- function(data, report_type) {
  if (!is.list(data) ||
      length(report_type) != 1L ||
      is.na(report_type) ||
      !report_type %in% names(BASIC_PREVIEW_REPORT_FIELDS)) {
    return(NULL)
  }

  data[[unname(BASIC_PREVIEW_REPORT_FIELDS[[report_type]])]]
}


basic_preview_table_names <- function(report, report_type = NULL) {
  if (!is.list(report) || is.null(names(report))) {
    return(character())
  }

  data_frame_fields <- names(report)[vapply(report, is.data.frame, logical(1L))]

  if (length(report_type) == 1L &&
      !is.na(report_type) &&
      report_type %in% names(BASIC_PREVIEW_TABLES)) {
    tables <- intersect(BASIC_PREVIEW_TABLES[[report_type]], data_frame_fields)
    if (is.data.frame(report$additional_data) && nrow(report$additional_data) > 0L) {
      tables <- c(tables, "additional_data")
    }
    return(tables)
  }

  excluded <- data_frame_fields %in% c("validation", "files", "checks") |
    grepl("_checks$", data_frame_fields)

  data_frame_fields[!excluded]
}


basic_preview_table_label <- function(table_name) {
  labels <- gsub("_", " ", as.character(table_name), fixed = TRUE)
  tools::toTitleCase(labels)
}


empty_monthly_site_preview_table <- function() {
  data.frame(
    data_section = character(),
    report_year = integer(),
    period_date = as.Date(character()),
    period_type = character(),
    month = integer(),
    month_label = character(),
    site = character(),
    site_code = character(),
    business_unit = character(),
    is_summary = logical(),
    measure = character(),
    value_type = character(),
    value = double(),
    validation_status = character(),
    validation_difference = double(),
    validation_relative_difference = double(),
    validation_tolerance = double(),
    source_file = character(),
    source_sheet = character(),
    source_row = integer(),
    source_column = integer(),
    stringsAsFactors = FALSE
  )
}


basic_preview_column <- function(data, column, missing_value) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(rep(missing_value, 0L))
  }

  if (!column %in% names(data)) {
    return(rep(missing_value, nrow(data)))
  }

  data[[column]]
}


basic_preview_as_date <- function(value) {
  if (inherits(value, "Date")) {
    return(as.Date(value))
  }

  if (inherits(value, c("POSIXct", "POSIXlt"))) {
    return(as.Date(value))
  }

  if (is.numeric(value)) {
    return(as.Date(value, origin = "1970-01-01"))
  }

  suppressWarnings(as.Date(as.character(value)))
}


make_monthly_site_preview_rows <- function(
  data,
  data_section,
  period_date,
  period_type,
  measure,
  value_type,
  value,
  section_order,
  value_order = 1L
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(NULL)
  }

  row_count <- nrow(data)
  recycle <- function(values) rep_len(values, row_count)

  data.frame(
    data_section = rep(data_section, row_count),
    report_year = suppressWarnings(as.integer(as.character(
      basic_preview_column(data, "report_year", NA_integer_)
    ))),
    period_date = basic_preview_as_date(recycle(period_date)),
    period_type = as.character(recycle(period_type)),
    month = suppressWarnings(as.integer(as.character(
      basic_preview_column(data, "month", NA_integer_)
    ))),
    month_label = as.character(
      basic_preview_column(data, "month_label", NA_character_)
    ),
    site = as.character(
      basic_preview_column(data, "site", NA_character_)
    ),
    site_code = as.character(
      basic_preview_column(data, "site_code", NA_character_)
    ),
    business_unit = as.character(
      basic_preview_column(data, "business_unit", NA_character_)
    ),
    is_summary = as.logical(
      basic_preview_column(data, "is_summary", NA)
    ),
    measure = as.character(recycle(measure)),
    value_type = as.character(recycle(value_type)),
    value = suppressWarnings(as.numeric(recycle(value))),
    validation_status = as.character(
      basic_preview_column(data, "validation_status", NA_character_)
    ),
    validation_difference = suppressWarnings(as.numeric(
      basic_preview_column(data, "validation_difference", NA_real_)
    )),
    validation_relative_difference = suppressWarnings(as.numeric(
      basic_preview_column(
        data,
        "validation_relative_difference",
        NA_real_
      )
    )),
    validation_tolerance = suppressWarnings(as.numeric(
      basic_preview_column(data, "validation_tolerance", NA_real_)
    )),
    source_file = as.character(
      basic_preview_column(data, "source_file", NA_character_)
    ),
    source_sheet = as.character(
      basic_preview_column(data, "source_sheet", NA_character_)
    ),
    source_row = suppressWarnings(as.integer(as.character(
      basic_preview_column(data, "source_row", NA_integer_)
    ))),
    source_column = suppressWarnings(as.integer(as.character(
      basic_preview_column(data, "source_column", NA_integer_)
    ))),
    .preview_section_order = rep(as.integer(section_order), row_count),
    .preview_value_order = as.integer(recycle(value_order)),
    stringsAsFactors = FALSE
  )
}


basic_preview_monthly_site_data <- function(report) {
  if (!is.list(report)) {
    return(NULL)
  }

  finance <- report$finance
  discounts <- report$discounts
  total_revenue <- report$total_revenue
  rows <- list()

  if (is.data.frame(finance) && nrow(finance) > 0L) {
    rows[[length(rows) + 1L]] <- make_monthly_site_preview_rows(
      data = finance,
      data_section = "Finance",
      period_date = basic_preview_column(finance, "date", as.Date(NA)),
      period_type = "Month",
      measure = basic_preview_column(finance, "metric", NA_character_),
      value_type = basic_preview_column(
        finance,
        "value_type",
        NA_character_
      ),
      value = basic_preview_column(finance, "value", NA_real_),
      section_order = 1L
    )
  }

  if (is.data.frame(discounts) && nrow(discounts) > 0L) {
    rows[[length(rows) + 1L]] <- make_monthly_site_preview_rows(
      data = discounts,
      data_section = "Discounts",
      period_date = basic_preview_column(discounts, "date", as.Date(NA)),
      period_type = "Month",
      measure = "Discount Opportunity",
      value_type = basic_preview_column(
        discounts,
        "discount_type",
        NA_character_
      ),
      value = basic_preview_column(discounts, "value", NA_real_),
      section_order = 2L
    )
  }

  revenue_fields <- c(
    actual_income = "Actual Income",
    discount_opportunity = "Discount Opportunity",
    calculated_total_revenue = "Calculated Total Revenue",
    reported_total_revenue = "Reported Total Revenue"
  )

  if (is.data.frame(total_revenue) && nrow(total_revenue) > 0L) {
    for (field_index in seq_along(revenue_fields)) {
      field <- names(revenue_fields)[[field_index]]

      if (!field %in% names(total_revenue)) {
        next
      }

      rows[[length(rows) + 1L]] <- make_monthly_site_preview_rows(
        data = total_revenue,
        data_section = "Total Revenue",
        period_date = basic_preview_column(
          total_revenue,
          "period_end",
          as.Date(NA)
        ),
        period_type = "Report Year",
        measure = "Total Revenue",
        value_type = unname(revenue_fields[[field_index]]),
        value = basic_preview_column(total_revenue, field, NA_real_),
        section_order = 3L,
        value_order = field_index
      )
    }
  }

  extra <- report$additional_data
  if (is.data.frame(extra) && nrow(extra) > 0L) {
    retained <- make_monthly_site_preview_rows(
      data = extra, data_section = "Additional data", period_date = as.Date(NA),
      period_type = "Unclassified", measure = extra$field,
      value_type = extra$interpretation, value = extra$value, section_order = 4L
    )
    retained$raw_value <- extra$raw_value
    rows[[length(rows) + 1L]] <- retained
  }

  rows <- Filter(
    function(data) is.data.frame(data) && nrow(data) > 0L,
    rows
  )

  if (length(rows) == 0L) {
    return(empty_monthly_site_preview_table())
  }

  result <- bind_basic_preview_tables(rows)
  sheet_key <- basic_preview_sheet_key(result$source_sheet)
  row_order <- order(
    result$report_year,
    result$source_file,
    sheet_key,
    result$.preview_section_order,
    is.na(result$source_row),
    result$source_row,
    is.na(result$source_column),
    result$source_column,
    result$.preview_value_order,
    na.last = TRUE
  )
  result <- result[row_order, , drop = FALSE]
  result$.preview_section_order <- NULL
  result$.preview_value_order <- NULL
  rownames(result) <- NULL
  result
}


basic_preview_typed_missing <- function(prototype, size) {
  prototype[rep(NA_integer_, as.integer(size))]
}


bind_basic_preview_tables <- function(tables) {
  tables <- Filter(is.data.frame, tables)

  if (length(tables) == 0L) {
    return(data.frame())
  }

  column_names <- unique(unlist(lapply(tables, names), use.names = FALSE))
  prototypes <- stats::setNames(
    lapply(
      column_names,
      function(column) {
        table_index <- which(vapply(
          tables,
          function(data) column %in% names(data),
          logical(1L)
        ))[[1L]]
        tables[[table_index]][[column]]
      }
    ),
    column_names
  )

  aligned <- lapply(
    tables,
    function(data) {
      missing_columns <- setdiff(column_names, names(data))

      for (column in missing_columns) {
        data[[column]] <- basic_preview_typed_missing(
          prototypes[[column]],
          nrow(data)
        )
      }

      data[, column_names, drop = FALSE]
    }
  )

  result <- do.call(rbind, aligned)
  rownames(result) <- NULL
  result
}


basic_preview_combined_report_data <- function(report, report_type) {
  if (!is.list(report) ||
      length(report_type) != 1L ||
      is.na(report_type) ||
      !report_type %in% names(BASIC_PREVIEW_TABLES)) {
    return(NULL)
  }

  table_names <- basic_preview_table_names(report, report_type)

  if (length(table_names) == 0L) {
    return(data.frame())
  }

  tables <- lapply(
    seq_along(table_names),
    function(table_index) {
      data <- report[[table_names[[table_index]]]]

      if (!is.data.frame(data)) {
        return(NULL)
      }

      data$.preview_table_order <- rep.int(as.integer(table_index), nrow(data))
      data$.preview_row_order <- seq_len(nrow(data))
      data
    }
  )
  result <- bind_basic_preview_tables(tables)
  result <- result[, setdiff(names(result), c("unit", "aggregation")), drop = FALSE]

  if (nrow(result) == 0L) {
    result$.preview_table_order <- NULL
    result$.preview_row_order <- NULL
    return(result)
  }

  sheet_order <- basic_preview_sheet_key(
    attr(report, "source_sheet_order", exact = TRUE)
  )
  sheet_rank <- match(
    basic_preview_sheet_key(
      basic_preview_column(result, "source_sheet", NA_character_)
    ),
    sheet_order
  )
  sheet_rank[is.na(sheet_rank)] <-
    length(sheet_order) + seq_len(sum(is.na(sheet_rank)))
  report_year <- suppressWarnings(as.integer(as.character(
    basic_preview_column(result, "report_year", NA_integer_)
  )))
  source_file <- as.character(
    basic_preview_column(result, "source_file", NA_character_)
  )
  row_order <- order(
    report_year,
    source_file,
    sheet_rank,
    result$.preview_table_order,
    result$.preview_row_order,
    na.last = TRUE
  )
  result <- result[row_order, , drop = FALSE]
  result$.preview_table_order <- NULL
  result$.preview_row_order <- NULL
  rownames(result) <- NULL
  result
}


basic_preview_drop_empty_columns <- function(data) {
  if (!is.data.frame(data) || ncol(data) == 0L || nrow(data) == 0L) {
    return(data)
  }

  has_value <- vapply(
    data,
    function(column) {
      if (is.character(column) || is.factor(column)) {
        return(any(!is.na(column) & nzchar(trimws(as.character(column)))))
      }

      any(!is.na(column))
    },
    logical(1L)
  )

  data[, has_value, drop = FALSE]
}


basic_preview_years <- function(data) {
  if (!is.data.frame(data) || !"report_year" %in% names(data)) {
    return(character())
  }

  years <- suppressWarnings(as.integer(as.character(data$report_year)))
  as.character(sort(unique(years[!is.na(years)])))
}


basic_preview_sheet_key <- function(value) {
  tolower(basic_preview_sheet_label(value))
}


basic_preview_sheet_label <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  labels <- trimws(gsub("[[:space:]]+", " ", value))
  comparison_keys <- tolower(trimws(gsub(
    "[[:space:]_-]+",
    " ",
    labels
  )))

  casual_hours <- grepl(
    paste0(
      "^(?:(19|20)[0-9]{2} )?casual hours",
      "(?: (19|20)[0-9]{2})?$"
    ),
    comparison_keys
  )
  court_hire <- grepl(
    paste0(
      "^(?:(19|20)[0-9]{2} )?court hire",
      "(?: (19|20)[0-9]{2} vs (19|20)[0-9]{2})?$"
    ),
    comparison_keys
  )
  casual_hire <- grepl(
    paste0(
      "^(?:(19|20)[0-9]{2} )?casual hire",
      "(?: (19|20)[0-9]{2}(?: vs (19|20)[0-9]{2})?)?$"
    ),
    comparison_keys
  )

  labels[casual_hours] <- "Casual Hours"
  labels[court_hire | casual_hire] <- "Casual Hire"
  labels
}


basic_preview_source_sheets <- function(data, source_sheet_order = character()) {
  if (!is.data.frame(data) || !"source_sheet" %in% names(data)) {
    return(character())
  }

  source_sheets <- as.character(data$source_sheet)
  labels <- basic_preview_sheet_label(source_sheets)
  keys <- basic_preview_sheet_key(source_sheets)
  present <- nzchar(keys)
  labels <- labels[present & !duplicated(keys)]
  keys <- keys[present & !duplicated(keys)]

  if (length(labels) == 0L || length(source_sheet_order) == 0L) {
    return(labels)
  }

  workbook_order <- basic_preview_sheet_key(source_sheet_order)
  rank <- match(keys, workbook_order)
  unmatched <- is.na(rank)
  rank[unmatched] <- length(workbook_order) + seq_len(sum(unmatched))
  labels[order(rank)]
}


basic_preview_field_values <- function(data, column) {
  if (!is.data.frame(data) || !column %in% names(data)) {
    return(character())
  }

  values <- as.character(data[[column]])
  present <- !is.na(values) & nzchar(trimws(values))
  unique(values[present])
}


basic_preview_valid_selections <- function(selected, available) {
  if (is.null(selected) || length(selected) == 0L) {
    return(character())
  }

  selected <- as.character(selected)
  selected <- selected[!is.na(selected) & nzchar(selected)]
  selected[selected %in% available]
}


filter_basic_preview_table <- function(
  data,
  year = character(),
  source_sheet = BASIC_PREVIEW_ALL_SHEETS,
  measures = character(),
  value_types = character(),
  search = ""
) {
  if (!is.data.frame(data)) {
    return(NULL)
  }

  filtered <- data
  selected_years <- if (is.null(year)) character() else as.character(year)
  selected_years <- selected_years[
    !is.na(selected_years) &
      nzchar(selected_years) &
      selected_years != "__all__"
  ]

  if ("report_year" %in% names(filtered) &&
      length(selected_years) > 0L) {
    filtered <- filtered[
      !is.na(filtered$report_year) &
        as.character(filtered$report_year) %in% selected_years,
      ,
      drop = FALSE
    ]
  }

  selected_sheet <- if (is.null(source_sheet)) {
    BASIC_PREVIEW_ALL_SHEETS
  } else {
    as.character(source_sheet)
  }
  if ("source_sheet" %in% names(filtered) &&
      length(selected_sheet) == 1L &&
      !is.na(selected_sheet) &&
      nzchar(selected_sheet) &&
      selected_sheet != BASIC_PREVIEW_ALL_SHEETS) {
    filtered <- filtered[
      !is.na(filtered$source_sheet) &
        basic_preview_sheet_key(filtered$source_sheet) ==
          basic_preview_sheet_key(selected_sheet),
      ,
      drop = FALSE
    ]
  }

  selected_measures <- if (is.null(measures)) {
    character()
  } else {
    as.character(measures)
  }
  selected_measures <- selected_measures[
    !is.na(selected_measures) & nzchar(selected_measures)
  ]
  if ("measure" %in% names(filtered) && length(selected_measures) > 0L) {
    filtered <- filtered[
      !is.na(filtered$measure) &
        as.character(filtered$measure) %in% selected_measures,
      ,
      drop = FALSE
    ]
  }

  selected_value_types <- if (is.null(value_types)) {
    character()
  } else {
    as.character(value_types)
  }
  selected_value_types <- selected_value_types[
    !is.na(selected_value_types) & nzchar(selected_value_types)
  ]
  if ("value_type" %in% names(filtered) &&
      length(selected_value_types) > 0L) {
    filtered <- filtered[
      !is.na(filtered$value_type) &
        as.character(filtered$value_type) %in% selected_value_types,
      ,
      drop = FALSE
    ]
  }

  search_text <- if (is.null(search)) "" else trimws(as.character(search))
  if (length(search_text) == 1L &&
      !is.na(search_text) &&
      nzchar(search_text) &&
      nrow(filtered) > 0L) {
    search_text <- tolower(search_text)
    matches <- Reduce(
      `|`,
      lapply(
        filtered,
        function(column) {
          grepl(
            search_text,
            tolower(as.character(column)),
            fixed = TRUE
          )
        }
      )
    )
    matches[is.na(matches)] <- FALSE
    filtered <- filtered[matches, , drop = FALSE]
  }

  rownames(filtered) <- NULL
  filtered
}


format_basic_preview_table <- function(data) {
  if (!is.data.frame(data)) {
    return(NULL)
  }

  formatted <- data

  if ("source_sheet" %in% names(formatted)) {
    formatted$source_sheet <- basic_preview_sheet_label(
      formatted$source_sheet
    )
  }

  date_columns <- names(formatted)[vapply(
    formatted,
    inherits,
    logical(1L),
    what = "Date"
  )]

  for (column in date_columns) {
    values <- formatted[[column]]
    formatted_values <- format(values, "%Y-%m-%d")
    formatted_values[is.na(values)] <- NA_character_
    formatted[[column]] <- formatted_values
  }

  formatted
}


next_basic_preview_sort <- function(
  clicked_column,
  current_column = "__none__",
  current_direction = "asc"
) {
  clicked_column <- as.character(clicked_column)
  current_column <- as.character(current_column)
  current_direction <- tolower(as.character(current_direction))

  if (length(clicked_column) != 1L ||
      is.na(clicked_column) ||
      !nzchar(clicked_column)) {
    return(list(
      column = current_column[[1L]],
      direction = current_direction[[1L]]
    ))
  }

  direction <- if (
    length(current_column) == 1L &&
      !is.na(current_column) &&
      identical(clicked_column, current_column)
  ) {
    if (identical(current_direction, "desc")) "asc" else "desc"
  } else {
    "desc"
  }

  list(column = clicked_column, direction = direction)
}


basic_preview_cell_text <- function(value) {
  if (length(value) == 0L || is.na(value[[1L]])) {
    return("")
  }

  value <- value[[1L]]

  if (is.numeric(value)) {
    return(format(
      value,
      trim = TRUE,
      scientific = FALSE,
      digits = 12
    ))
  }

  as.character(value)
}


basic_preview_table_ui <- function(
  data,
  sort_column = "__none__",
  sort_direction = "asc",
  sort_input_id
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(NULL)
  }

  columns <- names(data)
  active_column <- if (
    length(sort_column) == 1L &&
      !is.na(sort_column) &&
      sort_column %in% columns
  ) {
    sort_column
  } else {
    "__none__"
  }
  active_direction <- if (identical(as.character(sort_direction), "desc")) {
    "desc"
  } else {
    "asc"
  }

  header_cells <- lapply(columns, function(column) {
    active <- identical(column, active_column)
    arrow <- if (!active) {
      "\u21c5"
    } else if (identical(active_direction, "desc")) {
      "\u2193"
    } else {
      "\u2191"
    }
    next_direction <- if (active && identical(active_direction, "desc")) {
      "ascending"
    } else {
      "descending"
    }

    tags$th(
      scope = "col",
      `aria-sort` = if (active) {
        if (identical(active_direction, "desc")) "descending" else "ascending"
      } else {
        "none"
      },
      tags$button(
        type = "button",
        class = paste(
          "cleaned-preview-sort-button",
          if (active) "active" else NULL
        ),
        `data-column` = column,
        title = paste("Sort", next_direction, "by", column),
        `aria-label` = paste("Sort", next_direction, "by", column),
        onclick = paste0(
          "Shiny.setInputValue('",
          sort_input_id,
          "', this.dataset.column, {priority: 'event'});"
        ),
        span(
          class = "cleaned-preview-sort-arrow",
          `aria-hidden` = "true",
          arrow
        ),
        span(class = "cleaned-preview-column-label", column)
      )
    )
  })

  body_rows <- lapply(seq_len(nrow(data)), function(row_index) {
    tags$tr(lapply(columns, function(column) {
      tags$td(basic_preview_cell_text(data[[column]][row_index]))
    }))
  })

  tags$table(
    class = "table table-striped table-hover table-sm cleaned-preview-html-table",
    tags$thead(tags$tr(header_cells)),
    tags$tbody(body_rows)
  )
}


sort_basic_preview_table <- function(
  data,
  column = "__none__",
  direction = "asc"
) {
  if (!is.data.frame(data)) {
    return(NULL)
  }

  column <- if (is.null(column)) "__none__" else as.character(column)
  if (length(column) != 1L ||
      is.na(column) ||
      column == "__none__" ||
      !column %in% names(data) ||
      nrow(data) < 2L) {
    return(data)
  }

  decreasing <- identical(as.character(direction), "desc")
  row_order <- tryCatch(
    order(data[[column]], decreasing = decreasing, na.last = TRUE),
    error = function(error) seq_len(nrow(data))
  )
  sorted <- data[row_order, , drop = FALSE]
  rownames(sorted) <- NULL
  sorted
}


paginate_basic_preview_table <- function(data, page = 1L, page_size = 25L) {
  if (!is.data.frame(data)) {
    return(list(
      data = NULL,
      page = 1L,
      pages = 1L,
      from = 0L,
      to = 0L,
      total = 0L
    ))
  }

  page_size <- suppressWarnings(as.integer(page_size))
  if (length(page_size) != 1L || is.na(page_size) || page_size < 1L) {
    page_size <- 25L
  }

  total <- nrow(data)
  pages <- max(1L, as.integer(ceiling(total / page_size)))
  page <- suppressWarnings(as.integer(page))
  if (length(page) != 1L || is.na(page)) {
    page <- 1L
  }
  page <- min(max(page, 1L), pages)

  if (total == 0L) {
    page_data <- data[FALSE, , drop = FALSE]
    from <- 0L
    to <- 0L
  } else {
    from <- (page - 1L) * page_size + 1L
    to <- min(page * page_size, total)
    page_data <- data[seq.int(from, to), , drop = FALSE]
  }

  list(
    data = page_data,
    page = page,
    pages = pages,
    from = from,
    to = to,
    total = total
  )
}


basic_cleaned_export_sheet_names <- function(labels) {
  labels <- as.character(labels)
  labels[is.na(labels)] <- ""

  for (forbidden in c("\\", "/", "?", "*", "[", "]", ":")) {
    labels <- gsub(forbidden, " ", labels, fixed = TRUE)
  }

  labels <- trimws(gsub("[[:space:]]+", " ", labels))
  labels <- sub("^'+", "", labels)
  labels <- sub("'+$", "", labels)
  labels[!nzchar(labels)] <- "Cleaned Data"

  result <- character(length(labels))

  for (index in seq_along(labels)) {
    base_name <- substr(labels[[index]], 1L, 31L)
    candidate <- base_name
    copy_number <- 2L

    while (tolower(candidate) %in% tolower(result[seq_len(index - 1L)])) {
      suffix <- paste0(" (", copy_number, ")")
      available <- max(1L, 31L - nchar(suffix))
      candidate <- paste0(substr(base_name, 1L, available), suffix)
      copy_number <- copy_number + 1L
    }

    result[[index]] <- candidate
  }

  result
}


basic_cleaned_export_table <- function(data) {
  if (!is.data.frame(data)) {
    return(NULL)
  }

  result <- data

  if ("source_sheet" %in% names(result)) {
    result$source_sheet <- basic_preview_sheet_label(result$source_sheet)
  }

  for (column in names(result)) {
    values <- result[[column]]

    if (is.factor(values)) {
      result[[column]] <- as.character(values)
    } else if (inherits(values, "POSIXlt")) {
      result[[column]] <- as.POSIXct(values)
    } else if (is.list(values)) {
      result[[column]] <- vapply(
        values,
        function(value) paste(as.character(value), collapse = ", "),
        character(1L)
      )
    }
  }

  rownames(result) <- NULL
  result
}


basic_cleaned_export_sheets <- function(
  data,
  source_sheets = character(),
  years = character(),
  measures = character(),
  value_types = character(),
  search = "",
  sort_column = "__none__",
  sort_direction = "asc"
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(list())
  }

  filtered <- filter_basic_preview_table(
    data,
    year = years,
    source_sheet = BASIC_PREVIEW_ALL_SHEETS,
    measures = measures,
    value_types = value_types,
    search = search
  )

  if (!is.data.frame(filtered) || nrow(filtered) == 0L) {
    return(list())
  }

  available_sheets <- basic_preview_source_sheets(filtered, source_sheets)

  if (length(available_sheets) == 0L) {
    table <- basic_preview_drop_empty_columns(filtered)
    table <- sort_basic_preview_table(
      table,
      column = sort_column,
      direction = sort_direction
    )
    return(stats::setNames(
      list(basic_cleaned_export_table(table)),
      "Cleaned Data"
    ))
  }

  tables <- lapply(
    available_sheets,
    function(source_sheet) {
      table <- filter_basic_preview_table(
        filtered,
        source_sheet = source_sheet
      )
      table <- basic_preview_drop_empty_columns(table)
      table <- sort_basic_preview_table(
        table,
        column = sort_column,
        direction = sort_direction
      )
      basic_cleaned_export_table(table)
    }
  )
  keep <- vapply(
    tables,
    function(table) is.data.frame(table) && nrow(table) > 0L,
    logical(1L)
  )
  tables <- tables[keep]

  if (length(tables) == 0L) {
    return(list())
  }

  names(tables) <- basic_cleaned_export_sheet_names(available_sheets[keep])
  tables
}


basic_cleaned_export_filename <- function(report_type, years = character()) {
  report_label <- unname(BASIC_UPLOAD_REPORT_TYPES[[report_type]])

  if (length(report_label) != 1L || is.na(report_label)) {
    report_label <- "data"
  }

  report_slug <- tolower(gsub("[^A-Za-z0-9]+", "-", report_label))
  report_slug <- gsub("(^-|-$)", "", report_slug)
  years <- suppressWarnings(as.integer(as.character(years)))
  years <- sort(unique(years[!is.na(years)]))
  year_slug <- if (length(years) == 0L) {
    "all-years"
  } else {
    paste(years, collapse = "-")
  }

  paste0("cleaned-", report_slug, "-", year_slug, ".xlsx")
}


basic_upload_card <- function(
  ns,
  title,
  input_id,
  heading,
  description,
  icon,
  template_filename = NULL
) {
  card(
    class = "basic-upload-source-card",
    card_header(title),
    card_body(
      div(
        class = "basic-upload-zone",
        bs_icon(icon, size = "38px"),
        h5(heading),
        p(class = "basic-upload-description", description),
        fileInput(
          ns(input_id),
          label = NULL,
          multiple = TRUE,
          accept = c(".xlsx", ".xls"),
          buttonLabel = "Browse Files",
          placeholder = "No files selected"
        ),
        tags$small(
          class = "text-secondary",
          "Files are parsed immediately and kept for this session only."
        ),
        if (!is.null(template_filename)) tags$a(
          class = "basic-template-download",
          href = paste0("templates/", utils::URLencode(template_filename)),
          download = template_filename,
          bs_icon("file-earmark-arrow-down"),
          "Download Excel template"
        )
      )
    )
  )
}


# UI ----------------------------------------------------------------------

mod_data_upload_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Data",
    icon = bs_icon("database"),
    div(
      class = "p-4 data-management-page",
      div(
        class = "page-heading",
        h2("Data Management"),
        p(
          paste(
            "Upload and manage Excel reports, then inspect the cleaned",
            "tables used by the dashboard."
          )
        )
      ),
      div(
        class = "data-section-navigation",
        navset_pill(
        id = ns("data_section"),
        selected = "upload",
        nav_panel(
          span(
            class = "data-section-tab-label",
            bs_icon("cloud-arrow-up"),
            tags$span("Upload & Manage")
          ),
          value = "upload",
          div(
            class = "pt-3",
            div(
              class = "basic-session-note",
              bs_icon("info-circle"),
              div(
                tags$strong("Session-only upload"),
                tags$small(
                  paste(
                    "Refreshing or closing the dashboard clears these uploads.",
                    "Use Save current data to keep the cleaned data on your",
                    "computer. Your original Excel files are never modified."
                  )
                )
              )
            ),
            div(
              id = ns("upload_area"),
              class = "basic-upload-area",
              basic_upload_script(),
              div(
                class = "basic-upload-banner",
                role = "status",
                `aria-live` = "polite",
                hidden = NA
              ),
              card(
                class = "basic-saved-data-card",
                card_header(
                  div(
                    class = "basic-upload-all-header",
                    tags$strong("Saved data"),
                    tags$small(
                      class = "text-secondary",
                      "Skip re-reading the Excel reports next time"
                    )
                  )
                ),
                card_body(
                  layout_columns(
                    div(
                      class = "basic-saved-data-column",
                      tags$strong("Load saved data"),
                      p(
                        class = "basic-upload-description",
                        paste(
                          "Open a file made with Save current data. Its reports",
                          "appear below in seconds, and you can still add newer",
                          "reports on top."
                        )
                      ),
                      fileInput(
                        ns("saved_data"),
                        label = NULL,
                        multiple = FALSE,
                        accept = ".rds",
                        buttonLabel = "Choose saved file",
                        placeholder = "No file selected"
                      ),
                      uiOutput(ns("saved_data_notice"))
                    ),
                    div(
                      class = "basic-saved-data-column",
                      tags$strong("Save current data"),
                      uiOutput(ns("saved_data_actions"))
                    ),
                    col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6))
                  )
                )
              ),
              card(
                class = "basic-upload-all-card",
                card_header(
                  div(
                    class = "basic-upload-all-header",
                    tags$strong("Upload all reports"),
                    span(class = "basic-upload-recommended", "Recommended")
                  )
                ),
                card_body(
                  p(
                    class = "basic-upload-description",
                    paste(
                      "Select every report workbook at once, or choose the folder",
                      "that holds them. Each file is matched to its report type",
                      "from its file name and parsed one after another."
                    )
                  ),
                  layout_columns(
                    fileInput(
                      ns("all_reports"),
                      label = "Workbooks",
                      multiple = TRUE,
                      accept = c(".xlsx", ".xls"),
                      buttonLabel = "Choose files",
                      placeholder = "No files selected"
                    ),
                    htmltools::tagQuery(
                      fileInput(
                        ns("all_reports_folder"),
                        label = "Or a whole folder (including subfolders)",
                        multiple = TRUE,
                        buttonLabel = "Choose folder",
                        placeholder = "No folder selected"
                      )
                    )$find("input")$filter(
                      function(tag, index) {
                        identical(htmltools::tagGetAttribute(tag, "type"), "file")
                      }
                    )$addAttrs(
                      webkitdirectory = NA,
                      directory = NA
                    )$allTags(),
                    col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6))
                  ),
                  tags$small(
                    class = "text-secondary",
                    paste(
                      "File names must contain the report name and one four-digit",
                      "year, e.g. Monthly Site Report 2025 -December.xlsx."
                    )
                  )
                )
              ),
              div(
                class = "basic-upload-section-label",
                "Or add files to one report type"
              ),
              layout_columns(
                basic_upload_card(
                  ns = ns,
                  title = "Monthly Site Reports",
                  input_id = "monthly_reports",
                  heading = "Overview and site comparison",
                  description = paste(
                    "Upload one or more annual Monthly Site Report",
                    "workbooks."
                  ),
                  icon = "cloud-arrow-up",
                  template_filename = "Monthly Site Report 2026.xlsx"
                ),
                basic_upload_card(
                  ns = ns,
                  title = "Detailed Site Reports",
                  input_id = "site_reports",
                  heading = "Site detail",
                  description = paste(
                    "Upload Business Unit Template site or program",
                    "reports."
                  ),
                  icon = "building",
                  template_filename = "New Sport Site Report 2026.xlsx"
                ),
                basic_upload_card(
                  ns = ns,
                  title = "Venues Tracking Reports",
                  input_id = "venues_reports",
                  heading = "Venues operations",
                  description = paste(
                    "Upload annual Venues Tracking Report workbooks."
                  ),
                  icon = "activity",
                  template_filename = "2026 Venues Tracking Report.xlsx"
                ),
                col_widths = c(4, 4, 4)
              )
            ),
            uiOutput(ns("upload_notice")),
            br(),
            card(
              class = "basic-upload-status-card",
              full_screen = TRUE,
              card_header(
                div(
                  class = "basic-upload-status-card-header",
                  tags$strong("Uploaded files"),
                  tags$small(
                    paste(
                      "Follow parsing progress or use the remove action beside",
                      "a file. Original Excel files remain unchanged."
                    )
                  )
                )
              ),
              card_body(uiOutput(ns("file_status")))
            )
          )
        ),
        nav_panel(
          span(
            class = "data-section-tab-label",
            bs_icon("table"),
            tags$span("View Cleaned Data")
          ),
          value = "preview",
          div(
            class = "pt-3 cleaned-preview-page",
            div(
              class = "cleaned-preview-intro",
              h4("Cleaned Data Preview"),
              p(
                paste(
                  "Inspect the cleaned tables created from successfully",
                  "parsed files. Previewing does not read the Excel files again."
                )
              )
            ),
            layout_columns(
              card(
                class = "cleaned-preview-controls",
                card_header("Preview controls"),
                card_body(
                  selectInput(
                    ns("preview_report_type"),
                    "Report family",
                    choices = stats::setNames(
                      names(BASIC_UPLOAD_REPORT_TYPES),
                      unname(BASIC_UPLOAD_REPORT_TYPES)
                    ),
                    selected = names(BASIC_UPLOAD_REPORT_TYPES)[[1L]],
                    width = "100%"
                  ),
                  selectizeInput(
                    ns("preview_year"),
                    "Reporting year",
                    choices = character(),
                    selected = character(),
                    multiple = TRUE,
                    options = list(
                      placeholder = "All reporting years",
                      plugins = list("remove_button"),
                      closeAfterSelect = TRUE
                    ),
                    width = "100%"
                  ),
                  conditionalPanel(
                    condition = "input.preview_report_type === 'monthly_site'",
                    selectizeInput(
                      ns("preview_measure"),
                      "Measure",
                      choices = character(),
                      selected = character(),
                      multiple = TRUE,
                      options = list(
                        placeholder = "All measures",
                        plugins = list("remove_button"),
                        closeAfterSelect = TRUE
                      ),
                      width = "100%"
                    ),
                    selectizeInput(
                      ns("preview_value_type"),
                      "Value type",
                      choices = character(),
                      selected = character(),
                      multiple = TRUE,
                      options = list(
                        placeholder = "All value types",
                        plugins = list("remove_button"),
                        closeAfterSelect = TRUE
                      ),
                      width = "100%"
                    ),
                    ns = ns
                  ),
                  textInput(
                    ns("preview_search"),
                    "Search rows",
                    value = "",
                    placeholder = "Search any displayed column",
                    width = "100%"
                  ),
                  selectInput(
                    ns("preview_sort_column"),
                    "Sort by",
                    choices = c("Original order" = "__none__"),
                    selected = "__none__",
                    width = "100%"
                  ),
                  selectInput(
                    ns("preview_sort_direction"),
                    "Sort direction",
                    choices = c(
                      "Ascending" = "asc",
                      "Descending" = "desc"
                    ),
                    selected = "asc",
                    width = "100%"
                  ),
                  selectInput(
                    ns("preview_page_size"),
                    "Rows per page",
                    choices = c(
                      "10 rows" = "10",
                      "25 rows" = "25",
                      "50 rows" = "50",
                      "100 rows" = "100"
                    ),
                    selected = "25",
                    width = "100%"
                  )
                )
              ),
              card(
                class = "cleaned-preview-data-card",
                full_screen = TRUE,
                card_header(
                  div(
                    class = "cleaned-preview-data-header",
                    tags$span("Cleaned table"),
                    uiOutput(ns("preview_download_ui"))
                  )
                ),
                card_body(
                  div(
                    class = "workbook-sheet-browser",
                    div(
                      class = "workbook-sheet-heading",
                      bs_icon("files"),
                      div(
                        tags$strong("Source sheet"),
                        tags$small(
                          "Switch between the worksheets represented in the cleaned data."
                        )
                      )
                    ),
                    div(
                      class = "workbook-sheet-tabs",
                      radioButtons(
                        ns("preview_source_sheet"),
                        label = NULL,
                        choices = c(
                          "All sheets" = BASIC_PREVIEW_ALL_SHEETS
                        ),
                        selected = BASIC_PREVIEW_ALL_SHEETS,
                        inline = TRUE
                      )
                    )
                  ),
                  uiOutput(ns("preview_summary")),
                  div(
                    class = "cleaned-preview-table",
                    uiOutput(ns("cleaned_data_table"))
                  ),
                  div(
                    class = "cleaned-preview-pagination",
                    actionButton(
                      ns("preview_previous"),
                      "Previous",
                      icon = bs_icon("chevron-left"),
                      class = "btn-outline-secondary"
                    ),
                    uiOutput(ns("preview_page_info")),
                    actionButton(
                      ns("preview_next"),
                      "Next",
                      icon = bs_icon("chevron-right"),
                      class = "btn-outline-secondary"
                    )
                  )
                )
              ),
              col_widths = c(3, 9)
            )
          )
        )
      )
      )
    )
  )
}


# Server ------------------------------------------------------------------

mod_data_upload_server <- function(
  id,
  parsers = basic_upload_parsers()
) {
  moduleServer(
    id,
    function(input, output, session) {
      session_directory <- tempfile("monash-sport-session-")
      dir.create(session_directory, recursive = TRUE, showWarnings = FALSE)

      registry <- reactiveVal(empty_basic_upload_registry())
      parsed_results <- reactiveVal(list())
      warning_details <- reactiveVal(list())

      session$onSessionEnded(function() {
        if (dir.exists(session_directory)) {
          unlink(session_directory, recursive = TRUE, force = TRUE)
        }
      })

      update_registry_row <- function(file_id, ...) {
        rows <- registry()
        index <- match(file_id, rows$file_id)

        if (is.na(index)) {
          return(invisible(FALSE))
        }

        updates <- list(...)
        for (field in names(updates)) {
          rows[index, field] <- updates[[field]]
        }

        registry(rows)
        invisible(TRUE)
      }

      remove_session_files <- function(file_ids) {
        rows <- registry()
        file_ids <- intersect(as.character(file_ids), rows$file_id)

        if (length(file_ids) == 0L) {
          return(invisible(FALSE))
        }

        removed_rows <- rows[rows$file_id %in% file_ids, , drop = FALSE]
        stored_paths <- removed_rows$stored_path[
          !is.na(removed_rows$stored_path) & nzchar(removed_rows$stored_path)
        ]
        existing_paths <- stored_paths[file.exists(stored_paths)]

        if (length(existing_paths) > 0L) {
          unlink(existing_paths, force = TRUE)
        }

        parsed <- parsed_results()
        parsed[file_ids] <- NULL
        parsed_results(parsed)
        notes <- warning_details()
        notes[file_ids] <- NULL
        warning_details(notes)
        registry(rows[!rows$file_id %in% file_ids, , drop = FALSE])
        invisible(TRUE)
      }

      process_upload_batch <- function(
        report_type,
        upload_info,
        progress = NULL,
        offset = 0L,
        total = NULL
      ) {
        if (!is.data.frame(upload_info) || nrow(upload_info) == 0L) {
          return(invisible(NULL))
        }

        parser <- parsers[[report_type]]
        if (!is.function(parser)) {
          stop("No parser is configured for report type: ", report_type)
        }

        queued_ids <- character(nrow(upload_info))
        queued_rows <- vector("list", nrow(upload_info))
        now_key <- format(Sys.time(), "%Y%m%d%H%M%OS6")

        for (index in seq_len(nrow(upload_info))) {
          extension <- tolower(tools::file_ext(upload_info$name[[index]]))
          file_id <- paste(
            report_type,
            now_key,
            index,
            sample.int(999999L, 1L),
            sep = "-"
          )
          queued_ids[[index]] <- file_id
          stored_path <- file.path(
            session_directory,
            paste0(file_id, if (nzchar(extension)) paste0(".", extension) else "")
          )
          supported <- extension %in% c("xlsx", "xls")
          copied <- supported && isTRUE(file.copy(
            from = upload_info$datapath[[index]],
            to = stored_path,
            overwrite = TRUE
          ))

          status <- if (copied) "queued" else "failed"
          detail <- if (!supported) {
            "Unsupported file type; please use .xlsx or .xls."
          } else if (!copied) {
            "The temporary upload could not be copied for parsing."
          } else {
            "Waiting to be parsed"
          }

          queued_rows[[index]] <- data.frame(
            file_id = file_id,
            report_type = report_type,
            original_name = as.character(upload_info$name[[index]]),
            stored_path = if (copied) stored_path else NA_character_,
            size = as.numeric(upload_info$size[[index]]),
            status = status,
            progress = if (copied) 0 else 1,
            detail = detail,
            warning_count = 0L,
            error_message = if (copied) NA_character_ else detail,
            stringsAsFactors = FALSE
          )
        }

        registry(rbind(
          registry(),
          do.call(rbind, queued_rows)
        ))

        # Upload all reports shares one progress bar across report types.
        if (is.null(total)) {
          total <- nrow(upload_info)
        }
        if (is.null(progress)) {
          progress <- Progress$new(
            session = session,
            min = 0,
            max = total
          )
          on.exit(progress$close(), add = TRUE)
        }

        for (index in seq_len(nrow(upload_info))) {
          file_id <- queued_ids[[index]]
          current <- registry()
          row <- current[current$file_id == file_id, , drop = FALSE]

          if (nrow(row) == 0L || row$status[[1L]] == "failed") {
            next
          }

          progress$set(
            value = offset + index - 0.85,
            message = paste0(
              "Parsing ",
              offset + index,
              " of ",
              total
            ),
            detail = row$original_name[[1L]]
          )
          update_registry_row(
            file_id,
            status = "parsing",
            progress = 0.15,
            detail = "Reading, cleaning and validating workbook"
          )

          parsed <- tryCatch(
            parser(
              path = row$stored_path[[1L]],
              source_name = row$original_name[[1L]]
            ),
            error = identity
          )

          if (inherits(parsed, "error")) {
            message <- paste0(
              conditionMessage(parsed),
              basic_upload_mismatch_hint(row$original_name[[1L]], report_type)
            )
            update_registry_row(
              file_id,
              status = "failed",
              progress = 1,
              detail = "Parsing failed",
              error_message = message
            )
          } else {
            issues <- basic_upload_result_issues(parsed)
            # Keep notes separate so failed validation can also be inspected
            # without making that file's data available to the dashboard.
            file_warnings <- basic_upload_warning_rows(parsed)
            notes <- warning_details()
            notes[[file_id]] <- if (nrow(file_warnings) > 0L) file_warnings else NULL
            warning_details(notes)

            if (issues$errors > 0L) {
              update_registry_row(
                file_id,
                status = "failed",
                progress = 1,
                detail = paste(issues$errors, "validation error(s)"),
                warning_count = issues$warnings,
                error_message = paste0(
                  issues$first_error,
                  basic_upload_mismatch_hint(row$original_name[[1L]], report_type)
                )
              )
            } else {
              # Commit same-name replacement only after the candidate has
              # parsed and validated successfully. A failed candidate leaves
              # the previous ready file and its dashboard data untouched.
              current <- registry()
              replacement_ids <- current$file_id[
                current$file_id != file_id &
                  current$report_type == report_type &
                  tolower(trimws(current$original_name)) ==
                    tolower(trimws(row$original_name[[1L]]))
              ]
              conflict <- basic_upload_conflict(
                parsed,
                report_type,
                row$original_name[[1L]],
                current[
                  current$status == "ready" &
                    current$report_type == report_type &
                    !current$file_id %in% c(file_id, replacement_ids),
                  ,
                  drop = FALSE
                ],
                parsed_results()
              )

              if (!is.null(conflict)) {
                update_registry_row(
                  file_id,
                  status = "failed",
                  progress = 1,
                  detail = "Same report year already loaded",
                  warning_count = issues$warnings,
                  error_message = conflict
                )
                progress$set(
                  value = offset + index,
                  detail = paste(row$original_name[[1L]], "complete")
                )
                next
              }

              remove_session_files(replacement_ids)

              results <- parsed_results()
              results[[file_id]] <- parsed
              parsed_results(results)
              detail <- if (issues$warnings > 0L) {
                paste("Parsed with", issues$warnings, "warning(s)")
              } else {
                "Parsed successfully"
              }
              update_registry_row(
                file_id,
                status = "ready",
                progress = 1,
                detail = detail,
                warning_count = issues$warnings,
                error_message = NA_character_
              )
            }
          }

          progress$set(
            value = offset + index,
            detail = paste(row$original_name[[1L]], "complete")
          )
        }

        invisible(NULL)
      }

      upload_notice <- reactiveVal(NULL)

      # Unlocks the upload buttons in the browser once parsing has finished.
      finish_upload <- function() {
        session$sendCustomMessage(
          "basic-upload-finished",
          list(area = session$ns("upload_area"))
        )
      }

      process_single_type <- function(report_type, upload_info) {
        on.exit(finish_upload(), add = TRUE)
        upload_notice(NULL)
        process_upload_batch(report_type, upload_info)
      }

      process_all_reports <- function(upload_info) {
        on.exit(finish_upload(), add = TRUE)

        if (!is.data.frame(upload_info) || nrow(upload_info) == 0L) {
          return(invisible(NULL))
        }

        names <- as.character(upload_info$name)
        types <- classify_report_upload(names)
        excel <- grepl("\\.(xlsx|xls)$", names, ignore.case = TRUE) &
          !startsWith(names, "~$")
        matched <- !is.na(types)
        order <- names(BASIC_UPLOAD_REPORT_TYPES)
        counts <- vapply(order, function(type) sum(types %in% type), integer(1))

        upload_notice(list(
          counts = counts,
          unrecognised = names[excel & !matched],
          ignored = sum(!excel)
        ))

        if (!any(matched)) {
          return(invisible(NULL))
        }

        progress <- Progress$new(session = session, min = 0, max = sum(matched))
        on.exit(progress$close(), add = TRUE)
        offset <- 0L

        for (type in order) {
          rows <- which(types %in% type)
          if (length(rows) == 0L) next
          process_upload_batch(
            type,
            upload_info[rows, , drop = FALSE],
            progress = progress,
            offset = offset,
            total = sum(matched)
          )
          offset <- offset + length(rows)
        }

        invisible(NULL)
      }

      observeEvent(
        input$monthly_reports,
        process_single_type("monthly_site", input$monthly_reports),
        ignoreInit = TRUE
      )
      observeEvent(
        input$site_reports,
        process_single_type("site_detail", input$site_reports),
        ignoreInit = TRUE
      )
      observeEvent(
        input$venues_reports,
        process_single_type("venues_tracking", input$venues_reports),
        ignoreInit = TRUE
      )
      observeEvent(
        input$all_reports,
        process_all_reports(input$all_reports),
        ignoreInit = TRUE
      )
      observeEvent(
        input$all_reports_folder,
        process_all_reports(input$all_reports_folder),
        ignoreInit = TRUE
      )

      # Saved data ------------------------------------------------------

      saved_data_notice <- reactiveVal(NULL)

      client_utc_offset <- function() {
        root <- tryCatch(session$rootScope(), error = function(error) NULL)
        if (is.null(root)) NULL else root$input$monash_client_utc_offset
      }

      load_saved_data <- function(upload_info) {
        on.exit(finish_upload(), add = TRUE)
        upload_notice(NULL)

        if (!is.data.frame(upload_info) || nrow(upload_info) == 0L) {
          return(invisible(NULL))
        }

        saved <- tryCatch(
          read_basic_saved_data(upload_info$datapath[[1L]]),
          error = identity
        )
        if (inherits(saved, "error")) {
          saved_data_notice(list(
            error = conditionMessage(saved),
            file_name = as.character(upload_info$name[[1L]])
          ))
          return(invisible(NULL))
        }

        files <- saved$files
        now_key <- format(Sys.time(), "%Y%m%d%H%M%OS6")
        file_ids <- paste(
          "saved",
          now_key,
          seq_len(nrow(files)),
          sample.int(999999L, nrow(files), replace = TRUE),
          sep = "-"
        )

        # A saved file replaces a file of the same name and type that is
        # already in the session, as a fresh upload would.
        current <- registry()
        current_keys <- paste(
          current$report_type,
          tolower(trimws(current$original_name))
        )
        saved_keys <- paste(files$report_type, tolower(trimws(files$original_name)))
        replaced_ids <- current$file_id[current_keys %in% saved_keys]
        remove_session_files(replaced_ids)

        saved_on <- format(
          basic_saved_local_date(saved$saved_at, client_utc_offset()),
          "%d %b %Y"
        )
        new_rows <- data.frame(
          file_id = file_ids,
          report_type = files$report_type,
          original_name = files$original_name,
          stored_path = NA_character_,
          size = as.numeric(files$size),
          status = "ready",
          progress = 1,
          detail = paste("Loaded from saved data (saved", paste0(saved_on, ")")),
          warning_count = as.integer(files$warning_count),
          error_message = NA_character_,
          stringsAsFactors = FALSE
        )

        # A saved file whose year is already provided by another file in the
        # session (under a different name) is not used, as for uploads.
        current <- registry()
        current <- current[current$status == "ready", , drop = FALSE]
        current_results <- parsed_results()
        conflicts <- vapply(
          seq_len(nrow(files)),
          function(index) {
            message <- basic_upload_conflict(
              saved$results[[index]],
              files$report_type[[index]],
              files$original_name[[index]],
              current[current$report_type == files$report_type[[index]], , drop = FALSE],
              current_results
            )
            if (is.null(message)) NA_character_ else message
          },
          character(1)
        )
        used <- is.na(conflicts)
        new_rows$status[!used] <- "failed"
        new_rows$detail[!used] <- "Same report year already loaded"
        new_rows$error_message[!used] <- conflicts[!used]
        registry(rbind(registry(), new_rows))

        results <- parsed_results()
        results[file_ids[used]] <- saved$results[used]
        parsed_results(results)

        notes <- warning_details()
        for (index in which(used)) {
          if (!is.null(saved$warnings[[index]])) {
            notes[[file_ids[[index]]]] <- saved$warnings[[index]]
          }
        }
        warning_details(notes)

        saved_data_notice(list(
          file_name = as.character(upload_info$name[[1L]]),
          saved_at = saved$saved_at,
          files = files,
          replaced = length(replaced_ids),
          not_used = files$original_name[!used]
        ))
        invisible(NULL)
      }

      observeEvent(
        input$saved_data,
        load_saved_data(input$saved_data),
        ignoreInit = TRUE
      )

      saved_data_payload <- reactive({
        build_basic_saved_data(registry(), parsed_results(), warning_details())
      })

      output$saved_data_notice <- renderUI({
        notice <- saved_data_notice()

        if (is.null(notice)) {
          return(NULL)
        }

        if (!is.null(notice$error)) {
          return(div(
            class = "basic-upload-notice is-error",
            role = "alert",
            bs_icon("x-circle"),
            div(
              tags$strong(paste(notice$file_name, "was not loaded.")),
              p(notice$error)
            )
          ))
        }

        offset <- client_utc_offset()
        days_old <- basic_saved_data_days_old(notice$saved_at, utc_offset = offset)
        age <- if (days_old <= 0L) {
          "today"
        } else if (days_old == 1L) {
          "1 day ago"
        } else {
          paste(days_old, "days ago")
        }
        stale <- days_old > BASIC_SAVED_DATA_STALE_DAYS
        caution <- stale || length(notice$not_used) > 0L
        files <- notice$files

        div(
          class = paste(
            "basic-upload-notice basic-saved-data-manifest",
            if (caution) "is-warning" else "is-success"
          ),
          role = "status",
          bs_icon(if (caution) "exclamation-triangle" else "check2-circle"),
          div(
            tags$strong(paste0(
              "Saved data loaded · saved on ",
              format(basic_saved_local_date(notice$saved_at, offset), "%d %b %Y"),
              " (", age, ")"
            )),
            tags$ul(lapply(basic_saved_data_summary_text(files), tags$li)),
            if (stale) {
              p(paste0(
                "This data is more than ", BASIC_SAVED_DATA_STALE_DAYS,
                " days old. Reports updated since then are not included; ",
                "upload the newer reports to replace them, then save again."
              ))
            },
            if (length(notice$not_used) > 0L) {
              p(paste0(
                "Not used, because another file in this session already ",
                "covers the same year: ",
                paste(notice$not_used, collapse = ", "),
                ". See the file list below."
              ))
            },
            if (notice$replaced > 0L) {
              p(paste0(
                notice$replaced,
                if (notice$replaced == 1L) " file" else " files",
                " already in this session had the same name and",
                " were replaced by the saved copy."
              ))
            },
            tags$details(
              tags$summary(paste0(
                "Show the ", nrow(files), " source ",
                if (nrow(files) == 1L) "file" else "files"
              )),
              tags$ul(
                class = "basic-saved-data-files",
                lapply(order(files$report_type, files$original_name), function(index) {
                  tags$li(files$original_name[[index]])
                })
              )
            ),
            tags$small(class = "text-secondary", paste("From", notice$file_name))
          )
        )
      })

      output$saved_data_actions <- renderUI({
        files <- saved_data_payload()$files
        pending <- sum(registry()$status %in% c("queued", "parsing"))

        if (nrow(files) == 0L) {
          return(tagList(
            p(
              class = "basic-upload-description",
              paste(
                "Upload or load reports first. Saving keeps the cleaned data",
                "of every ready file in one small file on your computer."
              )
            ),
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary",
              disabled = "disabled",
              bs_icon("download"),
              "Save current data"
            )
          ))
        }

        tagList(
          p(
            class = "basic-upload-description",
            paste0(
              "Will save ", nrow(files),
              if (nrow(files) == 1L) " ready file" else " ready files",
              ". The file name shows the years, file count and save date."
            )
          ),
          tags$ul(
            class = "basic-saved-data-summary",
            lapply(basic_saved_data_summary_text(files), tags$li)
          ),
          if (pending > 0L) {
            p(class = "basic-upload-description", "Files still parsing are not included.")
          },
          downloadButton(
            session$ns("download_saved_data"),
            "Save current data",
            icon = bs_icon("download"),
            class = "btn btn-primary"
          ),
          tags$small(
            class = "basic-saved-data-caution",
            paste(
              "The saved file holds the same confidential figures as the",
              "reports. Keep it where the original reports are stored and do",
              "not email or share it outside the team."
            )
          )
        )
      })

      output$download_saved_data <- downloadHandler(
        filename = function() {
          basic_saved_data_filename(
            saved_data_payload()$files,
            utc_offset = client_utc_offset()
          )
        },
        content = function(file) {
          payload <- saved_data_payload()
          shiny::req(nrow(payload$files) > 0L)
          payload$saved_at <- Sys.time()
          saveRDS(payload, file)
        },
        contentType = "application/octet-stream"
      )

      output$upload_notice <- renderUI({
        notice <- upload_notice()

        if (is.null(notice)) {
          return(NULL)
        }

        counts <- notice$counts[notice$counts > 0L]
        matched_text <- if (length(counts) > 0L) {
          paste0(
            "Matched ", sum(counts), if (sum(counts) == 1L) " file: " else " files: ",
            paste(
              paste(counts, BASIC_UPLOAD_REPORT_TYPES[names(counts)]),
              collapse = ", "
            ),
            "."
          )
        } else {
          "No report workbooks were recognised in this selection."
        }
        problems <- length(notice$unrecognised) > 0L || length(counts) == 0L

        div(
          class = paste(
            "basic-upload-notice",
            if (problems) "is-warning" else "is-success"
          ),
          role = "status",
          bs_icon(if (problems) "exclamation-triangle" else "check2-circle"),
          div(
            tags$strong(matched_text),
            if (length(notice$unrecognised) > 0L) {
              p(
                paste0(
                  "Not recognised, so not uploaded: ",
                  paste(notice$unrecognised, collapse = ", "),
                  ". Rename these files to include the report name and one",
                  " four-digit year (see the user guide), or upload them",
                  " through the matching report type below."
                )
              )
            },
            if (notice$ignored > 0L) {
              p(
                paste0(
                  notice$ignored,
                  if (notice$ignored == 1L) " other file was" else " other files were",
                  " ignored because they are not Excel workbooks."
                )
              )
            }
          )
        )
      })

      observeEvent(
        input$remove_file,
        {
          file_id <- as.character(input$remove_file)

          if (length(file_id) != 1L || !nzchar(file_id)) {
            return()
          }

          rows <- registry()
          row <- rows[rows$file_id == file_id, , drop = FALSE]

          if (nrow(row) == 0L) {
            showNotification(
              "That upload has already been removed.",
              type = "warning"
            )
            return()
          }

          remove_session_files(file_id)
          showNotification(
            paste(row$original_name[[1L]], "was removed from this session."),
            type = "message"
          )
        },
        ignoreInit = TRUE
      )

      output$file_status <- renderUI({
        rows <- registry()
        notes <- warning_details()

        if (nrow(rows) == 0L) {
          return(div(
            class = "basic-upload-empty",
            bs_icon("inbox", size = "28px"),
            p("No files have been uploaded in this session.")
          ))
        }

        file_row <- function(index) {
            row <- rows[index, , drop = FALSE]
            percent <- round(100 * row$progress[[1L]])
            error_text <- row$error_message[[1L]]

            remove_script <- sprintf(
              paste0("Shiny.setInputValue('%s', this.dataset.fileId, ",
                     "{priority: 'event'});"),
              session$ns("remove_file")
            )

            div(
              class = paste(
                "basic-upload-status-row",
                paste0("status-", row$status[[1L]])
              ),
              div(
                class = "basic-upload-status-heading",
                div(
                  class = "basic-upload-file-name",
                  row$original_name[[1L]],
                  tags$small(
                    unname(BASIC_UPLOAD_REPORT_TYPES[[row$report_type[[1L]]]])
                  )
                ),
                div(
                  class = "basic-upload-status-actions",
                  span(
                    class = paste("basic-status-badge", row$status[[1L]]),
                    row$status[[1L]]
                  ),
                  tags$button(
                    type = "button",
                    class = "basic-upload-remove-button",
                    title = paste("Remove", row$original_name[[1L]]),
                    `aria-label` = paste(
                      "Remove uploaded file",
                      row$original_name[[1L]]
                    ),
                    `data-file-id` = row$file_id[[1L]],
                    onclick = remove_script,
                    bs_icon("trash", size = "15px")
                  )
                )
              ),
              tags$progress(value = percent, max = 100),
              div(
                class = "basic-upload-detail",
                paste0(percent, "% · ", row$detail[[1L]])
              ),
              if (!is.na(error_text) && nzchar(error_text)) {
                div(class = "basic-upload-error", error_text)
              },
              basic_upload_warning_details_ui(
                notes[[row$file_id[[1L]]]], row$original_name[[1L]]
              )
            )
        }

        # One collapsible group per report type keeps a long session (dozens
        # of workbooks) scannable. A group opens while any of its files is
        # still queued, parsing or failed.
        report_types <- intersect(
          names(BASIC_UPLOAD_REPORT_TYPES),
          unique(rows$report_type)
        )

        tagList(lapply(
          report_types,
          function(report_type) {
            indexes <- which(rows$report_type == report_type)
            statuses <- rows$status[indexes]
            n_ready <- sum(statuses == "ready")
            n_failed <- sum(statuses == "failed")
            n_pending <- length(indexes) - n_ready - n_failed
            n_warnings <- if ("warning_count" %in% names(rows)) {
              sum(rows$warning_count[indexes], na.rm = TRUE)
            } else {
              0L
            }

            summary_parts <- c(
              paste(length(indexes), if (length(indexes) == 1L) "file" else "files"),
              if (n_ready > 0L) paste(n_ready, "ready"),
              if (n_pending > 0L) paste(n_pending, "in progress"),
              if (n_failed > 0L) paste(n_failed, "failed"),
              if (n_warnings > 0L) {
                paste(n_warnings, if (n_warnings == 1L) "warning" else "warnings")
              }
            )

            tags$details(
              class = "basic-upload-group",
              open = if (n_pending > 0L || n_failed > 0L) NA else NULL,
              tags$summary(
                class = "basic-upload-group-summary",
                span(
                  class = "basic-upload-group-title",
                  unname(BASIC_UPLOAD_REPORT_TYPES[[report_type]])
                ),
                span(
                  class = "basic-upload-group-meta",
                  paste(summary_parts, collapse = " \u00b7 ")
                ),
                if (n_failed > 0L) {
                  span(class = "basic-status-badge failed", "failed")
                } else if (n_pending > 0L) {
                  span(class = "basic-status-badge parsing", "parsing")
                } else {
                  span(class = "basic-status-badge ready", "ready")
                }
              ),
              div(
                class = "basic-upload-group-body",
                lapply(indexes, file_row)
              )
            )
          }
        ))
      })

      analysis_data <- reactive({
        results <- parsed_results()

        select_objects <- function(result_class) {
          Filter(
            function(result) inherits(result, result_class),
            unname(results)
          )
        }

        monthly <- combine_basic_upload_results(
          select_objects("monthly_site_report_data"),
          "monthly_site_report_data"
        )
        sites <- combine_basic_upload_results(
          select_objects("business_unit_template_report_data"),
          "business_unit_template_report_data"
        )
        venues <- combine_basic_upload_results(
          select_objects("venues_tracking_report_data"),
          "venues_tracking_report_data"
        )

        list(
          monthly_reports = monthly,
          site_reports = sites,
          venues_reports = venues,
          monthly = monthly,
          sites = sites,
          venues = venues
        )
      })

      dashboard_data <- reactive({
        c(
          analysis_data(),
          list(
            uploads = registry()
          )
        )
      })

      selected_preview_report_type <- reactive({
        report_type <- input$preview_report_type

        if (is.null(report_type) ||
            length(report_type) != 1L ||
            !report_type %in% names(BASIC_PREVIEW_REPORT_FIELDS)) {
          return(names(BASIC_PREVIEW_REPORT_FIELDS)[[1L]])
        }

        report_type
      })

      preview_report <- reactive({
        basic_preview_report(
          dashboard_data(),
          selected_preview_report_type()
        )
      })


      preview_table_data <- reactive({
        report <- preview_report()
        report_type <- selected_preview_report_type()

        if (identical(report_type, "monthly_site")) {
          preview <- basic_preview_monthly_site_data(report)

          if (is.data.frame(preview) && "data_section" %in% names(preview)) {
            preview$data_section <- NULL
          }

          return(preview)
        }

        basic_preview_combined_report_data(report, report_type)
      })

      selected_preview_years <- reactive({
        available_years <- basic_preview_years(preview_table_data())
        basic_preview_valid_selections(input$preview_year, available_years)
      })

      preview_sheet_data <- reactive({
        filter_basic_preview_table(
          preview_table_data(),
          year = selected_preview_years(),
          source_sheet = BASIC_PREVIEW_ALL_SHEETS,
          search = ""
        )
      })

      preview_source_sheets <- reactive({
        basic_preview_source_sheets(
          preview_sheet_data(),
          source_sheet_order = attr(
            preview_report(),
            "source_sheet_order",
            exact = TRUE
          )
        )
      })

      selected_preview_source_sheet <- reactive({
        monthly_site_preview <- identical(
          selected_preview_report_type(),
          "monthly_site"
        )
        source_sheets <- preview_source_sheets()
        selected_sheet <- input$preview_source_sheet
        selected_is_available <-
          length(selected_sheet) == 1L &&
          !is.na(selected_sheet) &&
          selected_sheet %in% source_sheets

        if (selected_is_available) {
          return(selected_sheet)
        }

        if (monthly_site_preview && length(source_sheets) > 0L) {
          return(source_sheets[[1L]])
        }

        BASIC_PREVIEW_ALL_SHEETS
      })

      preview_measure_data <- reactive({
        filter_basic_preview_table(
          preview_table_data(),
          year = selected_preview_years(),
          source_sheet = selected_preview_source_sheet(),
          search = ""
        )
      })

      preview_measure_choices <- reactive({
        if (!identical(selected_preview_report_type(), "monthly_site")) {
          return(character())
        }

        basic_preview_field_values(preview_measure_data(), "measure")
      })

      selected_preview_measures <- reactive({
        basic_preview_valid_selections(
          input$preview_measure,
          preview_measure_choices()
        )
      })

      preview_value_type_data <- reactive({
        filter_basic_preview_table(
          preview_measure_data(),
          measures = selected_preview_measures(),
          search = ""
        )
      })

      preview_value_type_choices <- reactive({
        if (!identical(selected_preview_report_type(), "monthly_site")) {
          return(character())
        }

        basic_preview_field_values(preview_value_type_data(), "value_type")
      })

      selected_preview_value_types <- reactive({
        basic_preview_valid_selections(
          input$preview_value_type,
          preview_value_type_choices()
        )
      })

      observe({
        years <- basic_preview_years(preview_table_data())
        choices <- stats::setNames(years, years)
        current <- isolate(input$preview_year)
        selected <- basic_preview_valid_selections(current, years)

        updateSelectizeInput(
          session,
          "preview_year",
          choices = choices,
          selected = selected,
          server = TRUE
        )
      })

      observe({
        monthly_site_preview <- identical(
          selected_preview_report_type(),
          "monthly_site"
        )
        source_sheets <- preview_source_sheets()
        choices <- if (monthly_site_preview && length(source_sheets) > 0L) {
          stats::setNames(source_sheets, source_sheets)
        } else if (monthly_site_preview) {
          c("No sheets available" = BASIC_PREVIEW_ALL_SHEETS)
        } else {
          c(
            "All sheets" = BASIC_PREVIEW_ALL_SHEETS,
            stats::setNames(source_sheets, source_sheets)
          )
        }
        current <- isolate(input$preview_source_sheet)
        selected <- if (!is.null(current) && current %in% unname(choices)) {
          current
        } else if (monthly_site_preview && length(source_sheets) > 0L) {
          source_sheets[[1L]]
        } else {
          BASIC_PREVIEW_ALL_SHEETS
        }

        updateRadioButtons(
          session,
          "preview_source_sheet",
          choices = choices,
          selected = selected,
          inline = TRUE
        )
      })

      observe({
        choices <- preview_measure_choices()
        selected <- basic_preview_valid_selections(
          isolate(input$preview_measure),
          choices
        )

        updateSelectizeInput(
          session,
          "preview_measure",
          choices = stats::setNames(choices, choices),
          selected = selected,
          server = TRUE
        )
      })

      observe({
        choices <- preview_value_type_choices()
        selected <- basic_preview_valid_selections(
          isolate(input$preview_value_type),
          choices
        )

        updateSelectizeInput(
          session,
          "preview_value_type",
          choices = stats::setNames(choices, choices),
          selected = selected,
          server = TRUE
        )
      })

      observe({
        table_data <- preview_display_data()
        columns <- if (is.data.frame(table_data)) names(table_data) else character()
        choices <- c(
          "Original order" = "__none__",
          stats::setNames(columns, basic_preview_table_label(columns))
        )
        current <- isolate(input$preview_sort_column)
        selected <- if (!is.null(current) && current %in% unname(choices)) {
          current
        } else {
          "__none__"
        }

        updateSelectInput(
          session,
          "preview_sort_column",
          choices = choices,
          selected = selected
        )
      })

      observeEvent(input$preview_column_sort, {
        clicked_column <- as.character(input$preview_column_sort)
        table_data <- preview_display_data()

        if (!is.data.frame(table_data) ||
            length(clicked_column) != 1L ||
            is.na(clicked_column) ||
            !clicked_column %in% names(table_data)) {
          return()
        }

        next_sort <- next_basic_preview_sort(
          clicked_column = clicked_column,
          current_column = input$preview_sort_column,
          current_direction = input$preview_sort_direction
        )

        updateSelectInput(
          session,
          "preview_sort_column",
          selected = next_sort$column
        )
        updateSelectInput(
          session,
          "preview_sort_direction",
          selected = next_sort$direction
        )
      }, ignoreInit = TRUE)

      preview_filtered_data <- reactive({
        filter_basic_preview_table(
          preview_table_data(),
          year = selected_preview_years(),
          source_sheet = selected_preview_source_sheet(),
          measures = selected_preview_measures(),
          value_types = selected_preview_value_types(),
          search = input$preview_search
        )
      })

      preview_display_data <- reactive({
        basic_preview_drop_empty_columns(preview_filtered_data())
      })

      preview_sorted_data <- reactive({
        sort_basic_preview_table(
          preview_display_data(),
          column = input$preview_sort_column,
          direction = input$preview_sort_direction
        )
      })

      preview_export_sheets <- reactive({
        basic_cleaned_export_sheets(
          data = preview_table_data(),
          source_sheets = preview_source_sheets(),
          years = selected_preview_years(),
          measures = selected_preview_measures(),
          value_types = selected_preview_value_types(),
          search = input$preview_search,
          sort_column = input$preview_sort_column,
          sort_direction = input$preview_sort_direction
        )
      })

      output$preview_download_ui <- renderUI({
        if (length(preview_export_sheets()) == 0L) {
          return(tags$button(
            type = "button",
            class = paste(
              "btn btn-sm btn-outline-secondary",
              "cleaned-preview-download"
            ),
            disabled = "disabled",
            bs_icon("download"),
            tags$span("Download Excel")
          ))
        }

        downloadButton(
          session$ns("download_cleaned_data"),
          "Download Excel",
          icon = bs_icon("download"),
          class = "btn btn-primary btn-sm cleaned-preview-download"
        )
      })

      output$download_cleaned_data <- downloadHandler(
        filename = function() {
          basic_cleaned_export_filename(
            selected_preview_report_type(),
            selected_preview_years()
          )
        },
        content = function(file) {
          sheets <- preview_export_sheets()
          shiny::req(length(sheets) > 0L)
          writexl::write_xlsx(sheets, path = file, format_headers = TRUE)
        },
        contentType = paste0(
          "application/vnd.openxmlformats-officedocument.",
          "spreadsheetml.sheet"
        )
      )

      preview_page <- reactiveVal(1L)

      observeEvent(
        list(
          input$preview_report_type,
          input$preview_year,
          input$preview_source_sheet,
          input$preview_measure,
          input$preview_value_type,
          input$preview_search,
          input$preview_sort_column,
          input$preview_sort_direction,
          input$preview_page_size
        ),
        preview_page(1L),
        ignoreInit = TRUE
      )

      preview_page_state <- reactive({
        paginate_basic_preview_table(
          preview_sorted_data(),
          page = preview_page(),
          page_size = input$preview_page_size
        )
      })

      observeEvent(input$preview_previous, {
        page_state <- preview_page_state()
        preview_page(max(1L, page_state$page - 1L))
      })

      observeEvent(input$preview_next, {
        page_state <- preview_page_state()
        preview_page(min(page_state$pages, page_state$page + 1L))
      })

      output$preview_summary <- renderUI({
        report <- preview_report()
        report_type <- selected_preview_report_type()
        monthly_site_preview <- identical(report_type, "monthly_site")
        table_data <- preview_table_data()

        if (is.null(report)) {
          return(div(
            class = "cleaned-preview-empty",
            bs_icon("table"),
            div(
              tags$strong("No cleaned data is available yet"),
              tags$small(
                "Upload and successfully parse a file in Upload & Manage."
              )
            )
          ))
        }

        if (is.null(table_data)) {
          return(div(
            class = "cleaned-preview-empty",
            bs_icon("table"),
            div(
              tags$strong("No preview table is available"),
              tags$small("Cleaned rows appear after parsing finishes.")
            )
          ))
        }

        filtered_data <- preview_filtered_data()
        selected_years <- selected_preview_years()
        year_label <- if (length(selected_years) == 0L) {
          "All years"
        } else {
          paste(selected_years, collapse = ", ")
        }
        selected_sheet <- selected_preview_source_sheet()
        valid_selected_sheet <-
          length(selected_sheet) == 1L &&
          !is.na(selected_sheet) &&
          nzchar(selected_sheet)
        sheet_label <- if (
          !valid_selected_sheet ||
            selected_sheet == BASIC_PREVIEW_ALL_SHEETS
        ) {
          "All source sheets"
        } else {
          paste("Sheet:", selected_sheet)
        }
        display_title <- if (
          valid_selected_sheet &&
            selected_sheet != BASIC_PREVIEW_ALL_SHEETS
        ) {
          selected_sheet
        } else {
          paste(
            unname(BASIC_UPLOAD_REPORT_TYPES[[report_type]]),
            "contents"
          )
        }
        content_label <- if (monthly_site_preview) {
          selected_measures <- selected_preview_measures()

          if (length(selected_measures) == 0L) {
            "All measures"
          } else {
            paste(selected_measures, collapse = ", ")
          }
        } else {
          sheet_label
        }

        div(
          class = "cleaned-preview-summary",
          div(
            tags$strong(display_title),
            tags$small(
              paste(
                unname(BASIC_UPLOAD_REPORT_TYPES[[
                  report_type
                ]]),
                "·",
                year_label,
                "·",
                content_label
              )
            )
          ),
          div(
            class = "cleaned-preview-counts",
            span(
              paste0(
                format(nrow(filtered_data), big.mark = ",", scientific = FALSE),
                " of ",
                format(nrow(table_data), big.mark = ",", scientific = FALSE),
                " rows"
              )
            ),
            span(paste(ncol(preview_display_data()), "columns"))
          ),
          if (nrow(table_data) == 0L) {
            p(
              class = "cleaned-preview-no-match",
              "This table was created, but it contains no cleaned rows."
            )
          } else if (nrow(filtered_data) == 0L) {
            p(
              class = "cleaned-preview-no-match",
              "No rows match the current filters."
            )
          }
        )
      })

      output$cleaned_data_table <- renderUI({
        page_state <- preview_page_state()

        if (is.null(page_state$data) || page_state$total == 0L) {
          return(NULL)
        }

        basic_preview_table_ui(
          data = format_basic_preview_table(page_state$data),
          sort_column = input$preview_sort_column,
          sort_direction = input$preview_sort_direction,
          sort_input_id = session$ns("preview_column_sort")
        )
      })

      output$preview_page_info <- renderUI({
        page_state <- preview_page_state()

        text <- if (page_state$total == 0L) {
          "No rows"
        } else {
          paste0(
            format(page_state$from, big.mark = ",", scientific = FALSE),
            "–",
            format(page_state$to, big.mark = ",", scientific = FALSE),
            " of ",
            format(page_state$total, big.mark = ",", scientific = FALSE),
            " · Page ",
            page_state$page,
            " of ",
            page_state$pages
          )
        }

        span(class = "cleaned-preview-page-info", text)
      })

      dashboard_data
    }
  )
}
