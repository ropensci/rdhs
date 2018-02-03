##' @section Methods:
##'
##' \describe{
##' \item{\code{dhs_api_request}}{
##'   Makes a call to the DHS websites API. You canmake requests to any of their declared api endpoints (see \code{vignette(rdhs)} for more on these). API queries can be filtered by providing query terms, and you can control how many search results you want returned. The default paramters will return all of the results, and will format it nicely into a data.frame for you.
##'
##'   \emph{Usage:}
##'   \code{dhs_api_request(api_endpoint, query = list(), api_key = private$api_key,
##'       num_results = 100, just_results = TRUE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{api_endpoint}:   API endpoint. Must be one of the 12 possible endpoints.
##'     }
##'
##'     \item{\code{query}:   List of query filters. To see possible query filter terms for each endpoint then head to the DHS api website.
##'     }
##'
##'     \item{\code{api_key}:   DHS API key. Default will grab the key provided when the client was created.
##'     }
##'
##'     \item{\code{num_results}:   The Number of results to return. Default = "ALL" which will loop through all the api search results pages for you if there are more results than their API will allow you to fetch in one page. If you specify a number this many results will be returned (but probably best to just leave default).
##'     }
##'
##'     \item{\code{just_results}:   Boolean whetehr to return just the results or all the http API response. Default = TRUE (probably best again to leave as this.)
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Data.frame with search results if just_results=TRUE, otherwsie a nested list with all the API responses for each page required.
##' }
##' \item{\code{available_surveys}}{
##'   Searches the DHS website for all the surveys that you can donwloa. If the client has been created you will not have to pass in any parameters as these will be taken from your environment. The results of this function are cached.
##'
##'   \emph{Usage:}
##'   \code{available_surveys(your_email = Sys.getenv("rdhs.USER_EMAIL"), your_password = Sys.getenv("rdhs.USER_PASS"),
##'       your_project = Sys.getenv("rdhs.USER_PROJECT"), datasets_api_results = self$dhs_api_request("datasets",
##'           num_results = "ALL"))}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{your_email}:   DHS login email. Default = Sys.getenv("rdhs.USER_EMAIL")
##'     }
##'
##'     \item{\code{your_password}:   DHS login password. Default = Sys.getenv("rdhs.USER_PASS")
##'     }
##'
##'     \item{\code{your_project}:   DHS project. Default = Sys.getenv("rdhs.USER_PROJECT")
##'     }
##'
##'     \item{\code{datasets_api_results}:   The list of potential datasets as declared from their api. The default argument fetches these so don't worry too much about this. Default = self$dhs_api_request("datasets",num_results = "ALL")
##'     }
##'
##'     \item{\code{surveys_api_results}:   The list of potential surveys as declared from their api. The default argument fetches these so don't worry too much about this. Default = self$dhs_api_request("surveys",num_results = "ALL")
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Data.frame object with 14 variables that detail the surveys you can download, their url download links and the country, survey, year etc info for that link.
##' }
##' \item{\code{download_survey}}{
##'   Using the output from \code{avaialable_surveys} the client will download the surveys you have requested. The
##'
##'   \emph{Usage:}
##'   \code{download_survey(your_email = Sys.getenv("rdhs.USER_EMAIL"), your_password = Sys.getenv("rdhs.USER_PASS"),
##'       your_project = Sys.getenv("rdhs.USER_PROJECT"), output_dir_root = file.path(private$root,
##'           "surveys"), desired_survey, download_option = "both")}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{your_email}:   DHS login email. Default = Sys.getenv("rdhs.USER_EMAIL")
##'     }
##'
##'     \item{\code{your_password}:   DHS login password. Default = Sys.getenv("rdhs.USER_PASS")
##'     }
##'
##'     \item{\code{your_project}:   DHS project. Default = Sys.getenv("rdhs.USER_PROJECT")
##'     }
##'
##'     \item{\code{output_dir_root}:   Root directory where the surveys will be stored within. The default will download surveys to a subfolder of the client root called "surveys"
##'     }
##'
##'     \item{\code{desired_survey}:   The desired survey to be downloaded. This represents a row(s) from \code{avaialable_surveys}. If multiple rows are requested the function will handle that and give you back the results in a list, with elements named accordingly for the survey downloaded.
##'     }
##'
##'     \item{\code{download_option}:   Character specifying whether the survey should be just downloaded ("zip"), extracted ("ex"), imported and saved as an .rds object ("rds"), or both extract and rds ("both"). Conveniently you can just specify any letter from these options.
##'     }
##'
##'     \item{\code{reformat}:   Boolean concerning whether to reformat read in datasets. Default = TRUE.
##'     }
##'   }
##'
##'   \emph{Value}:
##'   Depends on the download_option requested, but ultimately it is a file path to where the survey was downloaded to, so that you can interact with it accordingly.
##' }
##' \item{\code{get_cache_date}}{
##'   Returns the private member variable cache-date, which is the date the client was last created/valiated against the DHS API.
##'
##'   \emph{Usage:}
##'   \code{get_cache_date()}
##'
##'   \emph{Value}:
##'   POSIXct and POSIXt time
##' }
##' \item{\code{set_cache_date}}{
##'   Sets the private member variable cache-date, which is the date the client was last created/valiated against the DHS API. This should never really be needed but is icluded to demonstrate the cache clearing properties of the client in the vignette.
##'
##'   \emph{Usage:}
##'   \code{set_cache_date(date)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{date}:   POSIXct and POSIXt time to update cache time to.
##'     }
##'   }
##' }
##' \item{\code{save_client}}{
##'   Internally save the client object as an .rds file within the root directory for the client.
##'
##'   \emph{Usage:}
##'   \code{save_client()}
##' }
##' \item{\code{clear_namespace}}{
##'   Clear the keys and values associated within a cache context. The dhs client caches a number of different tasks, and places these within specific contexts using the package \code{storr::storr_rds}.
##'
##'   \emph{Usage:}
##'   \code{clear_namespace(namespace)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{namespace}:   Character string for the namespace to be cleared.
##'     }
##'   }
##' }
##' }
