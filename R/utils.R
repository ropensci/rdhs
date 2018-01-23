#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' converts response to json by first converting the response to text
response_to_json <- function(res) {
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"),
                     simplifyVector = FALSE)
}

#' checks if the response is json or not by looking at the responses headers
response_is_json <- function(x) {
  content_type <- httr::headers(x)[["Content-Type"]]
  dat <- httr::parse_media(content_type)
  dat$type == "application" && dat$subtype == "json"
}

#' unzips files without throwing warnings
unzip_warn_fails <- function (...){
  tryCatch({
    unzip(...)
  }, warning = function(w) stop(conditionMessage(w)))
}
