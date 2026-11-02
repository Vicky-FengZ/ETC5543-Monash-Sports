# Build a static Shinylive version of the dashboard.
#
# The app then runs entirely in the visitor's browser (webR): uploaded Excel
# files are read on their own computer and are never sent to a server.
# Only the app code is copied into the build; no report data is included.
#
# Run from the project root, outside renv:
#   Rscript --vanilla deploy/build_shinylive.R
# Output: _site/ (a static website; serve it or publish it as-is).

lib <- Sys.getenv("R_LIBS_USER")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
if (!requireNamespace("shinylive", quietly = TRUE)) {
  install.packages("shinylive", lib = lib, repos = "https://cloud.r-project.org")
}

project <- normalizePath(".", winslash = "/")
stage <- file.path(tempdir(), "monash-sport-app")
site <- file.path(project, "_site")
unlink(stage, recursive = TRUE)
unlink(site, recursive = TRUE)
dir.create(stage, recursive = TRUE)

# App code and static assets only: never data/, tests/ or renv/.
file.copy(file.path(project, "app.R"), stage)
for (folder in c("R", "www", "config")) {
  file.copy(file.path(project, folder), stage, recursive = TRUE)
}
stopifnot(!dir.exists(file.path(stage, "data")))

# Only code, styles, images, the metric definitions and the blank templates
# may be published. Stop if any other spreadsheet or data file (a report or
# saved dashboard data, say) has found its way into these folders.
staged <- list.files(stage, recursive = TRUE)
data_like <- grepl("\\.(xlsx|xlsm|xls|csv|rds|rda|rdata)$", staged, ignore.case = TRUE)
allowed <- grepl("^www/templates/[^/~]+\\.xlsx$", staged) |
  staged == "config/metric_definitions.csv"
if (any(data_like & !allowed)) {
  stop(
    "Data files found in the app folders; refusing to build: ",
    paste(staged[data_like & !allowed], collapse = ", ")
  )
}

shinylive::export(stage, site)

# Publish the rendered user guide next to the app (not inside it, so it does
# not add to the app's loading time). Render it first with
# `quarto render docs/user_guide.qmd`; the navbar links to user_guide.html.
guide <- file.path(project, "docs", c("user_guide.html", "user_guide.pdf"))
guide <- guide[file.exists(guide)]
if (length(guide)) {
  file.copy(guide, site, overwrite = TRUE)
  cat("User guide copied:", paste(basename(guide), collapse = ", "), "\n")
} else {
  warning("docs/user_guide.html not found; the navbar User guide link will not work.")
}
cat("\nFiles in build:", length(list.files(site, recursive = TRUE)), "\n")
cat("BUILD DONE\n")
