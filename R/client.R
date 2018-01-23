
##' Make a DHS API client
##'
##' @title Make a dhs client
##'
##' @param api_key Character for DHS API KEY
##' @param root Character for root directory to where client, caches, surveys etc. will be stored.
##' Default = \code{rappdirs::user_cache_dir("rdhs",Sys.info()["user"])}
##' @param ... Passed to \code{R6_dhs_client}
##'
##'
##' @export
##'
dhs_api_client <- function(api_key=NULL,
                           root = rappdirs::user_cache_dir("rdhs",Sys.info()["user"]),
                           ...) {

  # check rdhs against api last update time
  if(dhs_last_update() > dhs_cache_date(root=root)){

    return(R6_dhs_client$new(api_key,root,...))

    # if no api updates have occurred then get the cached api client
  } else {

    return(readRDS(file.path(root,client_file_name())))

  }
}

R6_dhs_client <- R6::R6Class(

  classname = "dhs_client",
  cloneable = FALSE,

  # PUBLIC METHODS
  # -------------------------------------------------------------------------------------------------------
  public = list(

    # INITIALISATION
    initialize = function(api_key = NULL, root = NULL){
      private$api_key <- api_key
      private$root <- root
      private$storr <- storr::storr_rds(path)
      saveRDS(self,dhs_client_path())
    },

    # API REQUESTS
    #' will either return your request as a parsed json (having cached the result), or will return an error
    dhs_api_request = function(indicators,
                               api_key = private$api_key,
                               to_json = (private$defaults$response_format=="json")){

      # create RESTful url request
      url <- paste0(BASE_URL, paste(indicators,collapse = ","))

      # first check against cache
      out <- tryCatch(private$storr$get(url,"api_calls"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if(!is.null(out)){ return(out) } else {

        # Get request
        resp <- httr::GET(url,httr::accept_json(),encode = "json")

        ## pass to response parse
        parsed_resp <- dhs_client_response(resp,to_json)
        if(resp$status_code >= 400 && resp$status_code < 600){
          return(parsed_resp)
        }

        # put some message or return to let the user know if the data returned is empty

        ## then cache the resp if we haven't stopped already and return the parsed resp
        private$storr$set(url,parsed_resp,"api_calls")
        return(parsed_resp)

      }

    },

    # DOWNLOADABLE SURVEYS
    #' Creates data.frame of avaialble surveys using \code{downloadable_surveys} and caches it (as takes ages)
    available_surveys = function(output_dir = file.path(private$root,"avaialable_surveys"),
                                 your_email,
                                 your_password,
                                 your_project,
                                 max_urls=NULL){

      # create call for survey function call
      survey_function_call <- match.call(available_surveys,
                                         call("available_surveys",
                                              your_email,your_password,your_project,output_dir,max_urls))


      # create key from this:
      key <- paste0(stringr::str_trim((deparse(survey_function_call))),collapse="")


      # first check against cache
      out <- tryCatch(private$storr$get(paste0(stringr::str_trim((deparse(key))),collapse=""),
                                        "available_survey_calls"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if(!is.null(out)){ return(out) } else {

        # Get downloadable surveys
        resp <- eval(survey_function_call)

        ## pass to response parse
        # TODO:
        parsed_resp <- resp

        ## then cache the resp if we haven't stopped already and return the parsed resp
        private$storr$set(key,parsed_resp,"available_survey_calls")
        return(parsed_resp)

      }

    },

    # GETTERS
    get_cache_date = function() private$cache_date

    # SETTERS

  ),

  private = list(api_key = NULL,
                 root = NULL,
                 cache_date = Sys.time(),
                 url = "https://api.dhsprogram.com/rest/dhs/data/",
                 defaults = list(cache_results = TRUE,
                                 response_format = "json"),
                 storr = NULL)


  ## not explicityl needed as only pulling so no need for all these

  # ## HTTP verbs
  # .get = function(...) {
  #   self$dhs_request(httr::GET, private$url, private$api_key, ...)
  # },
  # .put = function(...) {
  #   self$dhs_request(httr::PUT, private$url, private$api_key, ...)
  # },
  # .post = function(...) {
  #   self$dhs_request(httr::POST, private$url, private$api_key, ...)
  # },
  # .delete = function(...) {
  #   self$dhs_request(httr::DELETE, private$url, private$api_key, ...)
  # }
  # )


)

