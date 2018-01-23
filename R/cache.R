#' Pull last DHS database update time
dhs_last_update <- function(){

  date <- jsonlite::fromJSON("https://api.dhsprogram.com/rest/dhs/dataupdates?format=json")$Data$UpdateDate %>%
    strptime(format = "%B, %d %Y %H:%M:%S") %>%
    max

  date
}

#' Pull last cache date
#'
#' @param root Character for root path to where client, caches, surveys etc. will be stored.
#'
dhs_cache_date <- function(root){

  # Grab cache directory for user
  cache_dir <- root

  # Does cache directory exist, as if not then return -1
  if(!dir.exists(cache_dir)){

    # create the cache directory as will be needed
    dir.create(cache_dir,recursive = TRUE)
    dir.create(file.path(cache_dir,api_call_cache_directory_name()),recursive = TRUE)

    return(-1)

  } else {

    # If somehow the cache object has been removed but the directory existed
    if(!file.exists(file.path(cache_dir,client_file_name()))){

      # If we are doing error checking here then check the call cache directory is there
      if(!dir.exists(file.path(cache_dir,api_call_cache_directory_name()))){
        dir.create(file.path(cache_dir,api_call_cache_directory_name()),recursive = TRUE)
      }

      return(-1)
    }

    # Read the client cache and return the chace date
    dhs_client <- readRDS(file.path(cache_dir,client_file_name()))

    # return client cache date
    return(dhs_client$get_cache_date())

  }

}

## CACHE (overly DRY) functions for constants
## -------------------------------------------------------------------

#' file name for where client is saved between sessions
client_file_name <- function() "dhs_client.rds"

#' directory name for where calls are cached between sessions
api_call_cache_directory_name <- function() "dhs_api_call_storr"


