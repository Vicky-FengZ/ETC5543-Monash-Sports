# Regression tests. Run from the project root:
#
#   source("tests/run_tests.R")
#
# Test files run in groups on several R processes at once (one per group,
# up to one less than the number of processor cores, at most 8). Set
# MONASH_SPORT_TEST_WORKERS=1 to run everything in this R session, one file
# after another, for example when debugging a failing test.

required <- c("shiny", "bslib", "bsicons", "readxl", "writexl")
missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]

if (length(missing) > 0L) {
  stop(
    paste("Run renv::restore() first. Missing:", paste(missing, collapse = ", ")),
    call. = FALSE
  )
}

r_files <- c(
  "app.R",
  list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
)
invisible(lapply(r_files, parse, keep.source = FALSE))
cat("PASS parse all R files\n")

# Code every test file can rely on.
load_test_setup <- function() {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(bsicons)
  })
  for (file in c(
    "R/data/load.R", "R/calculations.R", "R/plots.R", "R/forecasting.R",
    "R/forecast_plots.R", "R/config.R", "R/executive_visual_registry.R",
    "R/ui_helpers.R", "R/mod_overview.R", "R/mod_site_comparison.R",
    "R/mod_forecasting.R", "R/mod_executive_dashboard.R", "R/mod_site_detail.R",
    "R/mod_venues_operations.R", "R/mod_data_upload.R",
    # Shared fixtures for the Site Detail / Venues tests and plot smoke tests.
    "tests/fixtures_site_venues.R",
    # Several tests upload the same sample workbooks; parse each one once.
    "tests/helper_parse_cache.R"
  )) {
    source(file, local = globalenv())
  }
  install_test_parse_cache()
  invisible(TRUE)
}

# Groups run in order inside, and side by side with each other. A file that
# uses objects made by an earlier file stays in that file's group. `env`
# sets environment variables for the group only.
test_groups <- list(
  list(files = c(
    "test_data_pipeline_regressions.R", "test_venues_tracking_extensions.R",
    "test_basic_upload.R", "test_upload_warnings.R", "test_extensible_imports.R",
    "test_excel_templates.R", "test_cross_page_consistency.R"
  )),
  list(files = "test_upload_realworld.R"),
  list(files = c(
    "test_forecasting.R", "test_forecasting_simple.R", "test_forecasting_manual.R",
    "test_forecasting_layout.R", "test_forecasting_options.R"
  )),
  list(files = c(
    "test_overview_charts.R", "test_site_comparison.R", "test_executive_dashboard.R",
    "test_chart_and_ui_helpers.R", "test_site_detail.R", "test_venues_operations.R",
    "test_plot_smoke.R"
  )),
  # Whole app on every page. The full report set (and the real-workbook
  # tests) run only when MONASH_SPORT_REAL_DATA_ROOT is set, as
  # tests/run_sample_tests.R and tests/run_full_tests.R do.
  list(files = "test_app_smoke.R", env = c(MONASH_SPORT_SMOKE_PART = "single")),
  list(files = "test_app_smoke.R", env = c(MONASH_SPORT_SMOKE_PART = "combined")),
  list(files = "test_app_smoke.R", env = c(MONASH_SPORT_SMOKE_PART = "full")),
  list(files = c("test_real_upload_parsers.R", "test_key_numbers.R"))
)

# Runs one group and returns its printed output and any error.
run_test_group <- function(group) {
  old_env <- if (length(group$env) > 0L) Sys.getenv(names(group$env), unset = NA) else character()
  if (length(group$env) > 0L) do.call(Sys.setenv, as.list(group$env))
  on.exit({
    for (name in names(old_env)) {
      if (is.na(old_env[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, as.list(old_env[name]))
    }
  }, add = TRUE)

  started <- Sys.time()
  error <- NULL
  output <- utils::capture.output(
    tryCatch(
      for (file in group$files) source(file.path("tests", file), local = globalenv()),
      error = function(condition) error <<- conditionMessage(condition)
    ),
    type = "output"
  )
  list(
    files = group$files,
    output = output,
    error = error,
    seconds = as.numeric(difftime(Sys.time(), started, units = "secs"))
  )
}

test_workers <- suppressWarnings(as.integer(Sys.getenv("MONASH_SPORT_TEST_WORKERS", unset = "")))
if (is.na(test_workers)) {
  test_workers <- max(1L, min(8L, parallel::detectCores() - 1L, length(test_groups)), na.rm = TRUE)
}

if (test_workers > 1L) {
  cat("Running", length(test_groups), "test groups on", test_workers, "R processes\n")
  # Workers skip the project .Rprofile (renv start-up); they get this
  # session's package library below instead.
  cluster <- parallel::makePSOCKcluster(
    test_workers,
    rscript_args = c("--no-init-file", "--no-environ")
  )
  test_results <- tryCatch(
    {
      parallel::clusterCall(
        cluster,
        function(directory, libraries, variables) {
          setwd(directory)
          .libPaths(libraries)
          if (length(variables) > 0L) do.call(Sys.setenv, as.list(variables))
          NULL
        },
        getwd(),
        .libPaths(),
        Sys.getenv()[grepl("^MONASH_SPORT_", names(Sys.getenv()))]
      )
      parallel::clusterCall(cluster, load_test_setup)
      parallel::parLapplyLB(cluster, test_groups, run_test_group)
    },
    finally = parallel::stopCluster(cluster)
  )
} else {
  load_test_setup()
  test_results <- lapply(test_groups, run_test_group)
}

for (result in test_results) {
  if (length(result$output) > 0L) cat(result$output, sep = "\n")
  if (!is.null(result$error)) {
    cat("FAIL in ", paste(result$files, collapse = ", "), ":\n  ", result$error, "\n", sep = "")
  }
}

failed <- Filter(function(result) !is.null(result$error), test_results)
if (length(failed) > 0L) {
  stop(length(failed), " test group(s) failed; see FAIL above.", call. = FALSE)
}

cat(sprintf(
  "Slowest groups: %s\n",
  paste(vapply(utils::head(test_results[order(-vapply(test_results, `[[`, numeric(1), "seconds"))], 3L),
    function(result) sprintf("%s %.0fs", result$files[[1L]], result$seconds), character(1)), collapse = ", ")
))
cat("ALL TESTS PASSED\n")
