# -------------------------------------------------------------------------
# Basic Data Upload module
# -------------------------------------------------------------------------
# This deliberately provides session-only upload, parsing progress and file
# deletion. Persistent storage, user accounts, RDS caching and background
# workers belong to the later advanced-upload branch.

BASIC_UPLOAD_REPORT_TYPES <- c(
  monthly_site = "Monthly Site Reports",
  site_detail = "Detailed Site Reports",
  venues_tracking = "Venues Tracking Reports"
)


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
  list(
    monthly_site = function(path, source_name) {
      load_monthly_site_reports(
        paths = path,
        source_names = source_name,
        strict = FALSE
      )
    },
    site_detail = function(path, source_name) {
      load_business_unit_template_reports(
        paths = path,
        source_names = source_name,
        strict = FALSE
      )
    },
    venues_tracking = function(path, source_name) {
      load_venues_tracking_reports(
        paths = path,
        source_names = source_name,
        strict = FALSE
      )
    }
  )
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
  structure(combined, class = result_class)
}


basic_upload_card <- function(
  ns,
  title,
  input_id,
  heading,
  description,
  icon
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
            "Upload Excel reports, follow their parsing progress,",
            "and remove files from the current session."
          )
        )
      ),
      div(
        class = "basic-session-note",
        bs_icon("info-circle"),
        div(
          tags$strong("Session-only upload"),
          tags$small(
            paste(
              "Refreshing or closing the dashboard clears these uploads.",
              "Your original Excel files are never modified."
            )
          )
        )
      ),
      layout_columns(
        basic_upload_card(
          ns = ns,
          title = "Monthly Site Reports",
          input_id = "monthly_reports",
          heading = "Overview and site comparison",
          description = "Upload one or more annual Monthly Site Report workbooks.",
          icon = "cloud-arrow-up"
        ),
        basic_upload_card(
          ns = ns,
          title = "Detailed Site Reports",
          input_id = "site_reports",
          heading = "Site detail",
          description = "Upload Business Unit Template site or program reports.",
          icon = "building"
        ),
        basic_upload_card(
          ns = ns,
          title = "Venues Tracking Reports",
          input_id = "venues_reports",
          heading = "Venues operations",
          description = "Upload annual Venues Tracking Report workbooks.",
          icon = "activity"
        ),
        col_widths = c(4, 4, 4)
      ),
      br(),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("File parsing status"),
          card_body(uiOutput(ns("file_status")))
        ),
        card(
          card_header("Remove an uploaded file"),
          card_body(
            selectInput(
              ns("selected_file"),
              "Uploaded file",
              choices = character(),
              width = "100%"
            ),
            actionButton(
              ns("delete_file"),
              "Delete selected file",
              icon = bs_icon("trash"),
              class = "btn-outline-danger w-100"
            ),
            p(
              class = "text-secondary small mt-3 mb-0",
              paste(
                "This removes only the temporary Dashboard copy and its",
                "parsed data. The original file remains on your computer."
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
        registry(rows[!rows$file_id %in% file_ids, , drop = FALSE])
        invisible(TRUE)
      }

      process_upload_batch <- function(report_type, upload_info) {
        if (!is.data.frame(upload_info) || nrow(upload_info) == 0L) {
          return(invisible(NULL))
        }

        parser <- parsers[[report_type]]
        if (!is.function(parser)) {
          stop("No parser is configured for report type: ", report_type)
        }

        # Selecting a file with the same name replaces its previous temporary
        # session copy, preventing duplicate years from entering the charts.
        current_rows <- registry()
        duplicate_ids <- current_rows$file_id[
          current_rows$report_type == report_type &
            current_rows$original_name %in% upload_info$name
        ]
        remove_session_files(duplicate_ids)

        queued_ids <- character(nrow(upload_info))
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

          registry(rbind(
            registry(),
            data.frame(
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
          ))
        }

        progress <- Progress$new(
          session = session,
          min = 0,
          max = nrow(upload_info)
        )
        on.exit(progress$close(), add = TRUE)

        for (index in seq_len(nrow(upload_info))) {
          file_id <- queued_ids[[index]]
          current <- registry()
          row <- current[current$file_id == file_id, , drop = FALSE]

          if (nrow(row) == 0L || row$status[[1L]] == "failed") {
            next
          }

          progress$set(
            value = index - 0.85,
            message = paste0(
              "Parsing ",
              index,
              " of ",
              nrow(upload_info)
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
            message <- conditionMessage(parsed)
            update_registry_row(
              file_id,
              status = "failed",
              progress = 1,
              detail = "Parsing failed",
              error_message = message
            )
          } else {
            issues <- basic_upload_result_issues(parsed)

            if (issues$errors > 0L) {
              update_registry_row(
                file_id,
                status = "failed",
                progress = 1,
                detail = paste(issues$errors, "validation error(s)"),
                warning_count = issues$warnings,
                error_message = issues$first_error
              )
            } else {
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
            value = index,
            detail = paste(row$original_name[[1L]], "complete")
          )
        }

        invisible(NULL)
      }

      observeEvent(
        input$monthly_reports,
        process_upload_batch("monthly_site", input$monthly_reports),
        ignoreInit = TRUE
      )
      observeEvent(
        input$site_reports,
        process_upload_batch("site_detail", input$site_reports),
        ignoreInit = TRUE
      )
      observeEvent(
        input$venues_reports,
        process_upload_batch("venues_tracking", input$venues_reports),
        ignoreInit = TRUE
      )

      observeEvent(
        registry(),
        {
          rows <- registry()
          choices <- if (nrow(rows) == 0L) {
            character()
          } else {
            labels <- paste0(
              rows$original_name,
              " | ",
              unname(BASIC_UPLOAD_REPORT_TYPES[rows$report_type]),
              " | ",
              rows$status
            )
            stats::setNames(rows$file_id, labels)
          }

          current <- isolate(input$selected_file)
          selected <- if (!is.null(current) && current %in% rows$file_id) {
            current
          } else if (nrow(rows) > 0L) {
            rows$file_id[[1L]]
          } else {
            character()
          }

          updateSelectInput(
            session,
            "selected_file",
            choices = choices,
            selected = selected
          )
        },
        ignoreInit = FALSE
      )

      observeEvent(
        input$delete_file,
        {
          file_id <- input$selected_file

          if (is.null(file_id) || !nzchar(file_id)) {
            showNotification("Select an uploaded file first.", type = "warning")
            return()
          }

          rows <- registry()
          row <- rows[rows$file_id == file_id, , drop = FALSE]

          if (nrow(row) == 0L) {
            showNotification("That file is no longer available.", type = "warning")
            return()
          }

          remove_session_files(file_id)
          showNotification(
            paste(row$original_name[[1L]], "was removed from this session."),
            type = "message"
          )
        }
      )

      output$file_status <- renderUI({
        rows <- registry()

        if (nrow(rows) == 0L) {
          return(div(
            class = "basic-upload-empty",
            bs_icon("inbox", size = "28px"),
            p("No files have been uploaded in this session.")
          ))
        }

        tagList(lapply(
          seq_len(nrow(rows)),
          function(index) {
            row <- rows[index, , drop = FALSE]
            percent <- round(100 * row$progress[[1L]])
            error_text <- row$error_message[[1L]]

            div(
              class = "basic-upload-status-row",
              div(
                class = "basic-upload-status-heading",
                div(
                  class = "basic-upload-file-name",
                  row$original_name[[1L]],
                  tags$small(
                    unname(BASIC_UPLOAD_REPORT_TYPES[[row$report_type[[1L]]]])
                  )
                ),
                span(
                  class = paste("basic-status-badge", row$status[[1L]]),
                  row$status[[1L]]
                )
              ),
              tags$progress(value = percent, max = 100),
              div(
                class = "basic-upload-detail",
                paste0(percent, "% · ", row$detail[[1L]])
              ),
              if (!is.na(error_text) && nzchar(error_text)) {
                div(class = "basic-upload-error", error_text)
              }
            )
          }
        ))
      })

      reactive({
        rows <- registry()
        ready_rows <- rows[rows$status == "ready", , drop = FALSE]
        results <- parsed_results()

        select_objects <- function(report_type) {
          ids <- ready_rows$file_id[ready_rows$report_type == report_type]
          unname(results[ids])
        }

        monthly <- combine_basic_upload_results(
          select_objects("monthly_site"),
          "monthly_site_report_data"
        )
        sites <- combine_basic_upload_results(
          select_objects("site_detail"),
          "business_unit_template_report_data"
        )
        venues <- combine_basic_upload_results(
          select_objects("venues_tracking"),
          "venues_tracking_report_data"
        )

        list(
          monthly_reports = monthly,
          site_reports = sites,
          venues_reports = venues,
          monthly = monthly,
          sites = sites,
          venues = venues,
          uploads = rows
        )
      })
    }
  )
}
