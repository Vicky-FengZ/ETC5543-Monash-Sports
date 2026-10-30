# Full test run against the synthetic sample workbooks in data/sample.
# Use this to check the whole upload pipeline without the confidential
# Monash Sport reports. The sample values are random and carry no meaning.
#
#   source("tests/run_sample_tests.R")

sample_root <- normalizePath(file.path("data", "sample"), mustWork = FALSE)

if (!dir.exists(sample_root)) {
  stop("data/sample was not found. Run this from the project root.", call. = FALSE)
}

Sys.setenv(MONASH_SPORT_REAL_DATA_ROOT = sample_root)

tryCatch(
  source(file.path("tests", "run_tests.R")),
  finally = Sys.unsetenv("MONASH_SPORT_REAL_DATA_ROOT")
)
