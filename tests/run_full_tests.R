# Full test run, including parsing every real workbook in data/raw.
# data/raw is not in version control, so this only works on a machine that
# has the reports. Parsing all workbooks takes several minutes; run it before
# a submission or after changing the import, cleaning or validation code.
#
#   source("tests/run_full_tests.R")

real_data_root <- normalizePath(file.path("data", "raw"), mustWork = FALSE)

if (!dir.exists(real_data_root)) {
  stop("data/raw was not found. Run this from the project root on a machine with the reports.",
       call. = FALSE)
}

Sys.setenv(MONASH_SPORT_REAL_DATA_ROOT = real_data_root)

tryCatch(
  source(file.path("tests", "run_tests.R")),
  finally = Sys.unsetenv("MONASH_SPORT_REAL_DATA_ROOT")
)
