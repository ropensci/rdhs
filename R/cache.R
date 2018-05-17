#' Pull last DHS API database update time
last_api_update <- function() {

  if (Sys.getenv("rdhs_TIMEOUT") == "") {
    Sys.setenv("rdhs_TIMEOUT" = 30)
  }

  updates <- tryCatch(
    httr::GET(
      "https://api.dhsprogram.com/rest/dhs/dataupdates",
      httr::timeout(as.numeric(Sys.getenv("rdhs_TIMEOUT")))
    ),
    error = function(e) NULL
  )

  if (!is.null(updates) & updates$status_code == 200) {
  updates <- rbind_list_base(handle_api_response(updates)$Data)
  date <- updates$UpdateDate %>%
    strptime(format = "%B, %d %Y %H:%M:%S") %>%
    max()
  } else {

  date <- -0.5
  message("The DHS API took longer than ", Sys.getenv("rdhs_TIMEOUT"),
          " seconds to respond, or it returned an error.\n",
          "As a result some of the functionality of rdhs may not work.\n",
          "To check if the API is down please head to:\n",
          "https://api.dhsprogram.com/rest/dhs/dataupdates")
  }

  date
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

    # Read the client cache and return the chace date
    client_dhs <- readRDS(file.path(cache_dir, client_file_name()))

    # return client cache date
    return(client_dhs$get_cache_date())
  }
}

## CACHE (overly DRY) functions for constants
# -------------------------------------------------------------------

# file name for where client is saved between sessions
#' @noRd
client_file_name <- function() "client_dhs.rds"

# renv variable name for the credentials path
#' @noRd
renv_cred_path_name <- function() "rdhs_CREDENTIALS_PATH"

# renv variable name for the root path
#' @noRd
renv_root_path_name <- function() "rdhs_ROOT_PATH"
