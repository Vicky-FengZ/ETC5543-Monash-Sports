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

source(file.path("R", "data", "load.R"))
source(file.path("R", "calculations.R"))
source(file.path("R", "plots.R"))
source(file.path("R", "config.R"))
source(file.path("R", "ui_helpers.R"))
source(file.path("R", "mod_overview.R"))
source(file.path("tests", "test_data_pipeline_regressions.R"))
source(file.path("tests", "test_venues_tracking_extensions.R"))
source(file.path("tests", "test_basic_upload.R"))
source(file.path("tests", "test_overview_charts.R"))
source(file.path("tests", "test_site_comparison.R"))
source(file.path("tests", "test_real_upload_parsers.R"))

cat("ALL TESTS PASSED\n")
