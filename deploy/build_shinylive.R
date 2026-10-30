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

shinylive::export(stage, site)
cat("\nFiles in build:", length(list.files(site, recursive = TRUE)), "\n")
cat("BUILD DONE\n")
