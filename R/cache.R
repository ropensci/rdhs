#' Pull last DHS database update time
dhs_last_update <- function(){

  date <- jsonlite::fromJSON("https://api.dhsprogram.com/rest/dhs/dataupdates?format=json")$Data$UpdateDate %>%
    strptime(format = "%B, %d %Y %H:%M:%S") %>%
    max

  date
}

#' Pull last cache date
dhs_cache_date <- function(){

  # Grab cache directory for user
  cache_dir <- dhs_cache_dir_path()

  # Does cache directory exist, as if not then return -1
  if(!dir.exists(cache_dir)){

    # create the cache directory as will be needed
    dir.create(cache_dir,recursive = TRUE)
    dir.create(dhs_api_call_cache_dir_path(),recursive = TRUE)

    return(-1)

  } else {

    # If somehow the cache object has been removed but the directory existed
    if(!file.exists(cached_dhs_client_path())){

      # If we are doing error checking here then check the call cache directory is there
      if(!dir.exists(dhs_api_call_cache_dir_path())){
        dir.create(dhs_api_call_cache_dir_path(),recursive = TRUE)
      }

      return(-1)
    }

    # Read the client cache and return the chace date
    dhs_client <- get_cached_dhs_client()

    # return client cache date
    return(dhs_client$get_cache_date())

  }

}

## CACHE (overly DRY) functions
## -------------------------------------------------------------------

#' file name for where api client is saved between sessions
api_client_file_name <- function() "dhs_api_client.rds"

#' directory name for where api calls are cached between sessions
api_call_cache_directory_name <- function() "dhs_api_call_cache.Rcache"

#' DHS api cache directory path
dhs_cache_dir_path <- function() rappdirs::user_cache_dir("rdhs",Sys.info()["user"])

#' DHS api call cache directory path
dhs_api_call_cache_dir_path <- function() file.path(dhs_cache_dir_path(),api_call_cache_directory_name())

#' DHS api client path
cached_dhs_client_path <- function() file.path(dhs_cache_dir_path(),api_client_file_name())

#' Get DHS api client from cache
get_cached_dhs_client <- function() readRDS(cached_dhs_client_path())



