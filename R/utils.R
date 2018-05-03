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

#' implementation of data.tables rbindlist
#' @param x List of lists to be converted to a data.frame
rbind_list_base <- function(x) {
  x2 <- do.call(rbind.data.frame,
                c(x,stringsAsFactors = FALSE,make.row.names=FALSE))
  rownames(x2) <- 1:(dim(x2)[1])
  x2
}


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

#' Returns what the dataset file ending should be for a given filename
#'
#' @param file_format FileFormat for a file as taken from the API, e.g. \code{dhs_datasets(returnFields = "FileFormat")}
#'
#' @return One of "dat","dat","sas7bdat","sav" or "dta"
#'
#' @examples
#' file_format <- "Stata dataset (.dta)"
#' identical(rdhs:::file_dataset_format(file_format),"dta")
#'
file_dataset_format <- function(file_format){

  # hard coded here at the moment - when the package env client is finished replace this with
  # dats <- dhs_datasets(client = .rdhs$client,returnFields = "FileFormat")
  # dhs_file_formats <- unique(dats)

  dhs_file_formats <- c("Flat ASCII data (.dat)",
                        "Hierarchical ASCII data (.dat)",
                        "SAS dataset (.sas7bdat) ",
                        "SPSS dataset (.sav)",
                        "Stata dataset (.dta)")

  file_endings <- qdapRegex::ex_between(dhs_file_formats,left = "(.",right=")") %>% unlist
  file_endings[match(file_format,dhs_file_formats)]
}


# unzips files without throwing warnings
#' @noRd
unzip_warn_fails <- function (...){
  tryCatch({
    unzip(...)
  }, warning = function(w) stop(conditionMessage(w)))
}

# refresh client
#' @noRd
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
#' @noRd
type_convert_list_to_df <- function(l){

  l[lapply(l,is.character) %>% unlist] <- lapply(l[lapply(l,is.character) %>% unlist],type.convert,as.is=TRUE)

  return(l)
}

# type convert factor df to normal df
#' @noRd
type_convert_df <- function(df){

  l <- lapply(df,as.character)
  df <- as.data.frame.list(type_convert_list_to_df(l),stringsAsFactors = FALSE)

}

# convert api mdy_hms character date times to posix
#' @noRd
mdy_hms <- function(dates){
  strptime(dates,format = "%B, %d %Y %H:%M:%S")
}

# check if uppercase
# Credit: R package lettercase
#' @noRd
is_uppercase <- function (string)
{
  if (!is.atomic(string))
    stop("String must be an atomic vector", call. = FALSE)
  if (!is.character(string))
    string <- as.character(string)
  !grepl("[a-z]", string, perl = TRUE)
}
