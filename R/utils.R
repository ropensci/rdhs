#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
NULL

#' converts response to json by first converting the response to text
#' @param x A response
response_to_json <- function(x) {
  jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"),
                     simplifyVector = FALSE)
}

#' checks if the response is json or not by looking at the responses headers
#' @param x A response
response_is_json <- function(x) {
  content_type <- httr::headers(x)[["Content-Type"]]
  dat <- httr::parse_media(content_type)
  dat$type == "application" && dat$subtype == "json"
}

#' unzips files without throwing warnings
#' @param ... arguments to pass to \code{unzip}
#' @importFrom utils unzip
unzip_warn_fails <- function (...){
  tryCatch({
    unzip(...)
  }, warning = function(w) stop(conditionMessage(w)))
}


#' open file outside
#' @param txt_path txt file path
#'
#'
sopen <- function(txt_path) system(paste0("open ","\"",txt_path,"\""))
