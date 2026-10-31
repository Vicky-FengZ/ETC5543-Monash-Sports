# Test helper: reuse parse results within one test run.
#
# Several tests upload the same sample workbook, and parsing one takes a
# second or two. Parsing the same file contents under the same file name and
# report type always gives the same result, so after the first parse the
# tests reuse it. The cache lives only for this R session and never touches
# the app's own code. Call install_test_parse_cache() after
# R/mod_data_upload.R has been sourced (again).

test_parse_cache <- new.env()

install_test_parse_cache <- function() {
  original <- get("basic_upload_parsers", envir = globalenv())
  if (isTRUE(attr(original, "test_parse_cache"))) {
    return(invisible(FALSE))
  }

  cached <- function() {
    parsers <- original()
    lapply(stats::setNames(names(parsers), names(parsers)), function(report_type) {
      parser <- parsers[[report_type]]
      function(path, source_name) {
        key <- paste(report_type, source_name, unname(tools::md5sum(path)), sep = "|")
        if (!exists(key, envir = test_parse_cache, inherits = FALSE)) {
          assign(key, tryCatch(parser(path, source_name), error = identity), envir = test_parse_cache)
        }
        result <- get(key, envir = test_parse_cache, inherits = FALSE)
        if (inherits(result, "error")) stop(result)
        result
      }
    })
  }
  attr(cached, "test_parse_cache") <- TRUE
  assign("basic_upload_parsers", cached, envir = globalenv())
  invisible(TRUE)
}
