# Whole-dashboard smoke test: starts the app's own server (app.R), sets
# every control to the value it shows when the page opens, and renders
# every output on every page for several data situations. A page that
# breaks with partial, single-year or saved data is caught, and the main
# figures must appear once their data are loaded.
# Outputs that wait for data (req() or validate()) are fine; errors fail.

smoke_app <- new.env()
sys.source("app.R", envir = smoke_app)

smoke_decode <- function(text) {
  for (pair in list(c("&amp;", "&"), c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"))) {
    text <- gsub(pair[[1L]], pair[[2L]], text, fixed = TRUE)
  }
  text
}

smoke_attribute <- function(tag, name) {
  pattern <- paste0('\\s', name, '="([^"]*)"')
  if (!grepl(pattern, tag, perl = TRUE)) {
    return(NA_character_)
  }
  smoke_decode(sub(paste0(".*", pattern, ".*"), "\\1", tag, perl = TRUE))
}

# Values the browser would send for the controls in some HTML: the selected
# option of each select, the active tab of each tab set, the checked radio
# button, text and number values, and checkboxes. File inputs are left alone.
smoke_default_inputs <- function(html) {
  values <- list()

  selects <- regmatches(html, gregexpr("(?s)<select[^>]*>.*?</select>", html, perl = TRUE))[[1L]]
  for (select in selects) {
    opening <- regmatches(select, regexpr("<select[^>]*>", select))
    id <- smoke_attribute(opening, "id")
    if (is.na(id)) next
    options <- regmatches(select, gregexpr("<option[^>]*>", select))[[1L]]
    chosen <- options[grepl("\\sselected", options)]
    if (length(chosen) == 0L && !grepl("\\smultiple", opening) && length(options) > 0L) {
      chosen <- options[[1L]]
    }
    if (length(chosen) > 0L) {
      values[[id]] <- vapply(chosen, smoke_attribute, character(1), name = "value", USE.NAMES = FALSE)
    }
  }

  # Tab sets (navset_pill and friends): the active tab's value.
  navs <- regmatches(html, gregexpr("(?s)<ul[^>]*shiny-tab-input[^>]*>.*?</ul>", html, perl = TRUE))[[1L]]
  for (nav in navs) {
    id <- smoke_attribute(regmatches(nav, regexpr("<ul[^>]*>", nav)), "id")
    # Shiny marks the selected tab on its <li> (or on the link itself).
    links <- c(
      regmatches(nav, gregexpr('(?s)<li[^>]*class="[^"]*\\bactive\\b[^"]*"[^>]*>\\s*<a[^>]*>', nav, perl = TRUE))[[1L]],
      Filter(function(link) grepl('class="[^"]*\\bactive\\b', link, perl = TRUE),
        regmatches(nav, gregexpr("<a[^>]*>", nav))[[1L]]),
      regmatches(nav, gregexpr("<a[^>]*data-value[^>]*>", nav))[[1L]]
    )
    active <- sub("(?s).*(<a[^>]*>)$", "\\1", links, perl = TRUE)
    if (!is.na(id) && length(active) > 0L) {
      value <- smoke_attribute(active[[1L]], "data-value")
      if (!is.na(value)) values[[id]] <- value
    }
  }

  for (tag in regmatches(html, gregexpr("<input[^>]*>", html))[[1L]]) {
    type <- smoke_attribute(tag, "type")
    id <- smoke_attribute(tag, "id")
    value <- smoke_attribute(tag, "value")
    if (identical(type, "radio") && grepl("\\schecked", tag) &&
        !is.na(smoke_attribute(tag, "name"))) {
      values[[smoke_attribute(tag, "name")]] <- value
    } else if (type %in% c("text", "number") && !is.na(id) && !is.na(value)) {
      values[[id]] <- if (identical(type, "number")) suppressWarnings(as.numeric(value)) else value
    } else if (identical(type, "checkbox") && !is.na(id)) {
      values[[id]] <- grepl("\\schecked", tag)
    }
  }

  values
}

smoke_output_ids <- function(html) {
  tags <- regmatches(html, gregexpr("<[^>]*shiny-[a-z-]*output[^>]*>", html))[[1L]]
  ids <- vapply(tags, smoke_attribute, character(1), name = "id", USE.NAMES = FALSE)
  unique(ids[!is.na(ids)])
}

smoke_text <- function(value) {
  if (is.list(value) && !is.null(value$html)) value <- value$html
  if (!is.character(value)) return("")
  smoke_decode(trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(value, collapse = " ")))))
}

# Controls filled in by the server (updateSelectInput() and friends) only
# change in a browser. While this test runs, these wrappers record the value
# each update would select, so the test can send it back as the browser
# would. The modules find them before shiny's own functions; they are
# removed at the end of this file.
smoke_updates <- new.env()

smoke_record <- function(session, inputId, value) {
  if (is.null(value) || length(value) == 0L) {
    return(invisible())
  }
  id <- if (!is.null(session) && is.function(session$ns)) session$ns(inputId) else inputId
  assign(id, value, envir = smoke_updates)
  invisible()
}

smoke_first_choice <- function(choices) {
  values <- unlist(choices, use.names = FALSE)
  if (length(values) > 0L) as.character(values[[1L]]) else NULL
}

updateSelectInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL,
                              choices = NULL, selected = NULL) {
  smoke_record(session, inputId,
    if (!is.null(selected)) as.character(selected) else smoke_first_choice(choices))
  shiny::updateSelectInput(session, inputId, label, choices, selected)
}
updateSelectizeInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL,
                                 choices = NULL, selected = NULL, options = list(), server = FALSE) {
  smoke_record(session, inputId, if (!is.null(selected)) as.character(selected))
  shiny::updateSelectizeInput(session, inputId, label, choices, selected, options, server)
}
updateRadioButtons <- function(session = getDefaultReactiveDomain(), inputId, label = NULL,
                               choices = NULL, selected = NULL, inline = FALSE,
                               choiceNames = NULL, choiceValues = NULL) {
  smoke_record(session, inputId,
    if (!is.null(selected)) as.character(selected) else smoke_first_choice(c(choices, choiceValues)))
  shiny::updateRadioButtons(session, inputId, label, choices, selected, inline, choiceNames, choiceValues)
}
updateCheckboxGroupInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL,
                                     choices = NULL, selected = NULL, inline = FALSE,
                                     choiceNames = NULL, choiceValues = NULL) {
  smoke_record(session, inputId, if (!is.null(selected)) as.character(selected))
  shiny::updateCheckboxGroupInput(session, inputId, label, choices, selected, inline, choiceNames, choiceValues)
}
updateNumericInput <- function(session = getDefaultReactiveDomain(), inputId, label = NULL,
                               value = NULL, min = NULL, max = NULL, step = NULL) {
  smoke_record(session, inputId, value)
  shiny::updateNumericInput(session, inputId, label, value, min, max, step)
}

smoke_ui <- paste(as.character(smoke_app$ui), collapse = "")
smoke_static_outputs <- smoke_output_ids(smoke_ui)
smoke_static_inputs <- smoke_default_inputs(smoke_ui)
stopifnot(
  length(smoke_static_outputs) > 50L,
  !is.null(smoke_static_inputs[["overview-to_year"]]),
  !is.null(smoke_static_inputs[["executive_dashboard-year"]])
)

smoke_upload <- function(paths) {
  data.frame(
    name = basename(paths),
    size = file.info(paths)$size,
    type = "application/octet-stream",
    datapath = paths,
    stringsAsFactors = FALSE
  )
}

# Runs the app with the given uploads, opens every control at its default,
# renders every output (including outputs and controls that appear inside
# rendered UI) and checks the headline figures of the loaded report types.
smoke_check <- function(label, steps = list()) {
  rm(list = ls(smoke_updates), envir = smoke_updates)
  shiny::testServer(smoke_app$server, {
    session$flushReact()
    for (step in steps) {
      do.call(session$setInputs, step)
      session$flushReact()
    }

    uploads <- dashboard_data()$uploads
    failed <- uploads[uploads$status != "ready", , drop = FALSE]
    if (nrow(failed) > 0L) {
      stop(label, ": ", nrow(failed), " file(s) not ready: ",
        paste(failed$original_name, failed$error_message, sep = " - ", collapse = "; "),
        call. = FALSE)
    }

    render_all <- function(ids) {
      lapply(stats::setNames(ids, ids), function(id) {
        tryCatch(
          list(value = output[[id]]),
          shiny.silent.error = function(error) list(silent = TRUE),
          error = function(error) list(error = conditionMessage(error))
        )
      })
    }

    # Pass 1: page controls at their defaults. Pass 2: controls and outputs
    # created by rendered UI (for example the Site Detail filters).
    # Updates sent by the server are applied every pass, as in a browser.
    set_inputs <- character()
    outputs <- smoke_static_outputs
    pending <- smoke_static_inputs
    for (pass in 1:4) {
      pending <- pending[!names(pending) %in% set_inputs]
      updates <- as.list(smoke_updates)
      rm(list = ls(smoke_updates), envir = smoke_updates)
      pending[names(updates)] <- updates
      if (length(pending) > 0L) {
        do.call(session$setInputs, pending)
        session$flushReact()
        set_inputs <- unique(c(set_inputs, names(pending)))
      }
      rendered <- render_all(outputs)
      html <- paste(vapply(rendered, function(r) {
        if (is.list(r$value) && is.character(r$value$html)) r$value$html else ""
      }, character(1)), collapse = "")
      outputs <- unique(c(outputs, smoke_output_ids(html)))
      pending <- smoke_default_inputs(html)
    }
    rendered <- render_all(outputs)

    errors <- unlist(lapply(names(rendered), function(id) {
      if (!is.null(rendered[[id]]$error)) paste0(id, ": ", rendered[[id]]$error)
    }))
    if (length(errors) > 0L) {
      stop(label, ": ", length(errors), " output(s) failed:\n",
        paste(utils::head(errors, 20L), collapse = "\n"), call. = FALSE)
    }

    text <- function(id) if (is.null(rendered[[id]]$value)) "" else smoke_text(rendered[[id]]$value)
    shown <- function(id) grepl("[0-9]", text(id))
    types <- unique(uploads$report_type)
    checks <- c(
      if ("monthly_site" %in% types) c(
        "overview-income_value", "overview-net_value",
        "executive_dashboard-kpi_total_income", "executive_dashboard-kpi_net_result",
        "forecasting-period_label"
      ),
      if ("site_detail" %in% types) "site_detail-kpis",
      if ("venues_tracking" %in% types) "venues_operations-kpis"
    )
    missing <- checks[!vapply(checks, shown, logical(1))]
    if (length(missing) > 0L) {
      details <- vapply(missing, function(id) {
        prefix <- sub("-.*", "-", id)
        controls <- names(session$input)
        controls <- controls[startsWith(controls, prefix)]
        state <- if (!is.null(rendered[[id]]$silent)) "waiting for input" else
          paste0("showing \"", substr(text(id), 1L, 80L), "\"")
        paste0(id, " (", state, "; controls: ",
          paste(controls, vapply(controls, function(name) {
            paste(format(session$input[[name]]), collapse = "|")
          }, character(1)), sep = "=", collapse = ", "), ")")
      }, character(1))
      stop(label, ": no figures shown in\n  ", paste(details, collapse = "\n  "), call. = FALSE)
    }
    if ("monthly_site" %in% types &&
        !identical(text("overview-income_value"), text("executive_dashboard-kpi_total_income"))) {
      stop(label, ": Overview and Executive Dashboard show different income: ",
        text("overview-income_value"), " vs ", text("executive_dashboard-kpi_total_income"),
        call. = FALSE)
    }

    rendered_count <- sum(vapply(rendered, function(r) !is.null(r$value), logical(1)))
    cat(sprintf(
      "PASS app smoke: %s (%d files; %d of %d outputs rendered, %d controls set)\n",
      label, nrow(uploads), rendered_count, length(rendered), length(set_inputs)
    ))
  })
}

smoke_sample <- function(...) file.path("data", "sample", ...)
smoke_monthly <- smoke_sample("Monthly Site Report", "Monthly Site Report 2025 -December.xlsx")
smoke_venues <- smoke_sample("Business Unit Tracking Reports", "2025 Venues Tracking Report.xlsx")
smoke_site <- smoke_sample(
  "Business Unit Templates that feed master reports", "2025", "Aquatics Site Report 2025.xlsx"
)
stopifnot(file.exists(c(smoke_monthly, smoke_venues, smoke_site)))

smoke_check("no data")
smoke_check("Venues Tracking Report only",
  list(list(`data_upload-venues_reports` = smoke_upload(smoke_venues))))
smoke_check("one Detailed Site Report only",
  list(list(`data_upload-site_reports` = smoke_upload(smoke_site))))
smoke_check("one year of Monthly Site Report only",
  list(list(`data_upload-monthly_reports` = smoke_upload(smoke_monthly))))
smoke_check("one file of each type",
  list(list(`data_upload-all_reports` = smoke_upload(c(smoke_monthly, smoke_venues, smoke_site)))))

# Full report set (sample or real), then the same data loaded from a saved
# file in a fresh session. Runs with run_sample_tests.R / run_full_tests.R.
smoke_root <- Sys.getenv("MONASH_SPORT_REAL_DATA_ROOT", unset = "")
if (nzchar(smoke_root)) {
  smoke_all <- list.files(smoke_root, "\\.xlsx$", recursive = TRUE, full.names = TRUE)
  smoke_all <- smoke_all[!is.na(classify_report_upload(basename(smoke_all)))]
  smoke_saved <- tempfile(fileext = ".rds")
  smoke_check(
    "full report set",
    list(list(`data_upload-all_reports` = smoke_upload(smoke_all)))
  )
  shiny::testServer(mod_data_upload_server, {
    session$flushReact()
    session$setInputs(all_reports = smoke_upload(smoke_all))
    session$flushReact()
    saveRDS(saved_data_payload(), smoke_saved)
  })
  smoke_check(
    "full report set loaded from saved data",
    list(list(`data_upload-saved_data` = data.frame(
      name = "saved.rds",
      size = file.size(smoke_saved),
      type = "application/octet-stream",
      datapath = smoke_saved,
      stringsAsFactors = FALSE
    )))
  )
  unlink(smoke_saved)
} else {
  cat("SKIP app smoke: full report set (MONASH_SPORT_REAL_DATA_ROOT is not set)\n")
}

rm(updateSelectInput, updateSelectizeInput, updateRadioButtons,
  updateCheckboxGroupInput, updateNumericInput)
cat("PASS test_app_smoke.R\n")
