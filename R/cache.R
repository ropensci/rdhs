#' Pull last DHS API database update time
#' @param timeout Numeric for API timeout. Default = 30
last_api_update <- function(timeout = 30) {

  updates <- tryCatch(
    httr::GET(
      "https://api.dhsprogram.com/rest/dhs/dataupdates",
      httr::timeout(as.numeric(timeout))
    ),
    error = function(e) NULL
  )

  if (inherits(updates, "response") && updates$status_code == 200) {

    updates <- rbind_list_base(handle_api_response(updates)$Data)
    date <- updates$UpdateDate %>%
      mdy_hms() %>%
      max()

    # also check the datasets as this gets updated first, and updates is not
    # always up to date
    dats <- datasets_forced_no_client()
    datasets_date <- dats$FileDateLastModified %>%
      mdy_hms() %>%
      max(na.rm = TRUE)

    # take the greatest date
    date <- max(date, datasets_date)

  } else if (is.null(updates)) {

    date <- -0.5
    message("While rdhs was setting up your cache directory, it noticed \n",
            "the DHS API took longer than ", timeout, " seconds to respond.\n",
            "As a result some of the functionality of rdhs may not work.\n",
            "To check if the API is down please head to:\n",
            "https://api.dhsprogram.com/rest/dhs/dataupdates\n")

  } else if (inherits(updates, "response")) {

    date <- 0.5
    message("While rdhs was setting up your cache directory, it noticed \n",
            "the DHS API has failed and has returned the following error:")
    handle_api_response(updates, stop = FALSE)
    message("\nAs a result some of the functionality of rdhs may not work.\n",
            "To confirm if the API is down please head to:\n\n   -> ",
            "https://api.dhsprogram.com/rest/dhs/dataupdates\n")

  }

  date
}

# make a request for all DHS datasets without the client
#' @noRd
datasets_forced_no_client <- function(){

  l <- formals(dhs_datasets)[-c(14:16)]
  l$returnFields <- "FileDateLastModified"
  l$f <- "json"
  l$apiKey <- api_key_internal
  dats <- api_request("https://api.dhsprogram.com/rest/dhs/datasets",
                      query = l, all_results = TRUE, timeout = 30)
  return(dats)
}

#' Pull last cache date
#'
#' @param root Character for root path to where client,
#'   caches, surveys etc. will be stored.
#'
client_cache_date <- function(root) {

  # Grab cache directory for user
  cache_dir <- root

  # Does cache directory exist, as if not then return -1
  if (!dir.exists(cache_dir)) {

    # create the cache directory as will be needed
    dir.create(cache_dir, recursive = TRUE)
    return(-1)
  } else {

    # If somehow the cache object has been removed but the directory existed
    if (!file.exists(file.path(cache_dir, client_file_name()))) {
      return(-1)
    }

    # Read the client cache and return the cache date if it exists
    file <- file.path(cache_dir, client_file_name())
    if (file.exists(file)) {
      # return client cache date
      client_dhs <- readRDS(file.path(cache_dir, client_file_name()))
      return(client_dhs$get_cache_date())
    } else {
      return(-1)
    }
  }
}

## CACHE (overly DRY) functions for constants
# -------------------------------------------------------------------

# file name for where client is saved between sessions
#' @noRd
client_file_name <- function() "client_dhs.rds"

# rappdirs name
#' @noRd
rappdirs_rdhs <- function() rappdirs::user_cache_dir("rdhs", Sys.info()["user"])

# file name for config file
#' @noRd
config_file_name <- function() "rdhs.json"
