
##' Make a DHS API client
##'
##' @title Make a dhs client
##'
##' @param api_key Character for DHS API KEY
##' @param ... Passed to \code{dhs_client}
##' @export
dhs_api_client <- function(api_key=NULL,...) {

  # check rdhs against api last update time
  if(dhs_last_update() > dhs_cache_date()){

    return(R6_dhs_client$new(api_key,...))

    # if no api updates have occurred then get the cached api client
  } else {

    return(get_cached_dhs_client())

  }
}

R6_dhs_client <- R6::R6Class(

  classname = "dhs_client",
  cloneable = FALSE,

  # public methods
  public = list(


    initialize = function(api_key = NULL){
      private$api_key <- api_key
      saveRDS(self,dhs_client_path())
    },

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


    # GETTERS
    get_cache_date = function() private$cache_date

    # SETTERS

  ),

  private = list(api_key = NULL,
                 cache_date = Sys.time(),
                 url = "https://api.dhsprogram.com/rest/dhs/data/",
                 defaults = list(cache_results = TRUE,
                                 response_format = "json"),
                 storr = storr::storr_rds(rdhs:::cache_dir_path())
  )

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

