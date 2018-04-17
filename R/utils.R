#' Pipe operator
#'
#' See \code{\link[magrittr:pipe]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
#' @usage lhs \%>\% rhs
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

# unzips files without throwing warnings
unzip_warn_fails <- function (...){
  tryCatch({
    unzip(...)
  }, warning = function(w) stop(conditionMessage(w)))
}

# refresh client
client_refresh <- function(cli){

  cli$set_cache_date(last_api_update()-1)
  cli$save_client()
  root <- cli$get_root()
  if(!is.null(cli$.__enclos_env__$private$credentials_path)){
  if(file.exists(cli$.__enclos_env__$private$credentials_path)){
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",
                            credentials = cli$.__enclos_env__$private$credentials_path,
                            root = root)
  }} else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",root = root)
  }

  return(cli)

}

## convert list
type_convert_list_to_df <- function(l){

  l[lapply(l,is.character) %>% unlist] <- lapply(l[lapply(l,is.character) %>% unlist],type.convert,as.is=TRUE)

  return(l)
}

# type convert factor df to normal df
type_convert_df <- function(df){

  l <- lapply(df,as.character)
  df <- as.data.frame.list(type_convert_list_to_df(l),stringsAsFactors = FALSE)

}

# convert api mdy_hms character date times to posix
mdy_hms <- function(dates){
  strptime(dates,format = "%B, %d %Y %H:%M:%S")
}

# check if uppercase
# Credit: R package lettercase
is_uppercase <- function (string)
{
  if (!is.atomic(string))
    stop("String must be an atomic vector", call. = FALSE)
  if (!is.character(string))
    string <- as.character(string)
  !grepl("[a-z]", string, perl = TRUE)
}
