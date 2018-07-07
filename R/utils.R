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
  x2 <- do.call(
    rbind.data.frame,
    c(x, stringsAsFactors = FALSE, make.row.names = FALSE)
  )
  rownames(x2) <- seq_len(dim(x2)[1])
  x2
}

#' collapse API response list
#' @param x List of lists from API to be collapsed
collapse_api_responses <- function(x) {
  rbind_list_base(do.call(c, lapply(x, function(y) y$Data)))
}


#' converts response to json by first converting the response to text
#' @param x A response
response_to_json <- function(x) {
  jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
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
#' @param file_format FileFormat for a file as taken from the API,
#'   e.g. \code{dhs_datasets(returnFields = "FileFormat")}
#'
#' @return One of "dat","dat","sas7bdat","sav" or "dta"
#'
#' @examples
#' file_format <- "Stata dataset (.dta)"
#' identical(rdhs:::file_dataset_format(file_format),"dta")
#'
file_dataset_format <- function(file_format) {

  dhs_file_formats <- c(
    "Flat ASCII data (.dat)",
    "Hierarchical ASCII data (.dat)",
    "SAS dataset (.sas7bdat) ",
    "SPSS dataset (.sav)",
    "Stata dataset (.dta)"
  )

  file_endings <- qdapRegex::ex_between(dhs_file_formats,
                                        left = "(.", right = ")") %>% unlist()
  file_endings[match(file_format, dhs_file_formats)]
}

# refresh client
#' @noRd
client_refresh <- function(cli) {

  cli$set_cache_date(last_api_update() - 1)
  cli$save_client()
  root <- cli$get_root()
  if (!is.null(cli$.__enclos_env__$private$credentials_path)) {
    if (file.exists(cli$.__enclos_env__$private$credentials_path)) {
      cli <- client_dhs(
        api_key = "ICLSPH-527168",
        credentials = cli$.__enclos_env__$private$credentials_path,
        root = root
      )
    }
  } else {
    cli <- client_dhs(api_key = "ICLSPH-527168", root = root)
  }

  return(cli)
}

## convert list
#' @noRd
type_convert_list_to_df <- function(l) {

  l[lapply(l, is.character) %>% unlist()] <-
    lapply(l[lapply(l, is.character) %>% unlist()], type.convert, as.is = TRUE)

  return(l)
}

# type convert factor df to normal df
#' @noRd
type_convert_df <- function(df) {

  l <- lapply(df, as.character)
  df <- as.data.frame.list(type_convert_list_to_df(l), stringsAsFactors = FALSE)
}

# convert api mdy_hms character date times to posix
#' @noRd
mdy_hms <- function(dates) {

  locale_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", locale_lc_time))
  
  Sys.setlocale("LC_TIME","C")
  
  strptime(dates, format = "%B, %d %Y %H:%M:%S")
}

# find Renviron file
#' @noRd

find_renviron <- function(){

  pathnames <- c(Sys.getenv("R_ENVIRON_USER"),
                 "./.Renviron",
                 "~/.Renviron")

  pathnames <- pathnames[file.exists(pathnames)]
  pathnames <- pathnames[!file.info(pathnames)$isdir]

  pathnames <- if (length(pathnames) == 0) {
    character(0L)
  } else {
    pathnames[1]
  }

  if (length(pathnames) == 0) {
    pathnames <- normalizePath("~/.Renviron", winslash = "/", mustWork = FALSE)
    file.create(pathnames)
  }

  file.path(normalizePath(pathnames, winslash = "/"))

}


# check if uppercase
# Credit: R package lettercase
#' @noRd
is_uppercase <- function(string) {

  if (!is.atomic(string)) {
    stop("String must be an atomic vector", call. = FALSE)
  }
  if (!is.character(string)) {
    string <- as.character(string)
  }
  !grepl("[a-z]", string, perl = TRUE)
}


assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  if (!(is.character(x) && length(x) == 1L && !is.na(x))) {
    stop(sprintf("'%s' must be a scalar character", name))
  }
  invisible(x)
}


assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
    stop(sprintf("'%s' must be a scalar logical", name))
  }
  invisible(x)
}


assert_scalar_numeric <- function(x, name = deparse(substitute(x))) {
  if (!(is.numeric(x) && length(x) == 1L && !is.na(x))) {
    stop(sprintf("'%s' must be a scalar numeric", name))
  }
  invisible(x)
}


is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\]|//|\\\\\\\\)", path)
}


squote <- function(x) {
  sprintf("'%s'", x)
}
