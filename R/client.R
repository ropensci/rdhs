
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
dhs_client <- function(api_key=NULL,
                           root = rappdirs::user_cache_dir("rdhs",Sys.info()["user"]),
                           ...) {

  # check rdhs against api last update time
  if(dhs_last_update() > dhs_cache_date(root=root)){

    # create new client if DHS database has been updated
    client <- R6_dhs_client$new(api_key,root,...)

    # If there was already a client in your root (i.e. there was a DHS update)
    # the empty the api_call cache namespace
    # MAYBE change this - api calls are cheap so not an issue just clearing this
    # and then more manually checking dates for the actual surveys
    client$.__enclos_env__$private$storr$clear(namespace = "api_calls")
    client$.__enclos_env__$private$storr$gc()
    client$set_cache_date(Sys.time())

    return(client)

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
      private$storr <- storr::storr_rds(root)
      saveRDS(self,file.path(root,client_file_name()))
    },

    # API REQUESTS
    #' will either return your request as a parsed json (having cached the result), or will return an error
    dhs_api_request = function(api_endpoint,
                               query,
                               api_key = private$api_key){


      # Check api_endpoints first
      if(!is.element(api_endpoint,private$api_endpoints)){
        stop(paste("Requested api_endpoint is invalid. Must be one one of:",
                   paste(api_endpoints,collapse="\n"),
                   "For more information check the api website: https://api.dhsprogram.com/#/index.html",sep="\n"))
      }

      # Collapse query list
      query_param_lengths <- lapply(query,length) %>% unlist

      # collapse where lengths are greater than 1
      for(i in which(query_param_lengths>1)){
        query[[i]] <- paste0(query[[i]],collapse=",")
      }

      # add the api key if it exists
      query$apiKey <- private$api_key

      # Build url query and associated key
      url <- httr::modify_url(paste0(private$url,api_endpoint),query = query)
      key <- paste0(api_endpoint,"_",paste0(names(query),unlist(query),collapse=","))

      # first check against cache
      out <- tryCatch(private$storr$get(key,"api_calls"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if(!is.null(out)){ return(out) } else {

        # Get request
        resp <- httr::GET(url,httr::accept_json(),encode = "json")

        ## pass to response parse
        parsed_resp <- dhs_client_response(resp,TRUE)
        if(resp$status_code >= 400 && resp$status_code < 600){
          return(parsed_resp)
        }

        # put some message or return to let the user know if the data returned is empty
        # TODO: Loop for pages

        ## then cache the resp if we haven't stopped already and return the parsed resp
        private$storr$set(key,parsed_resp,"api_calls")
        return(parsed_resp)

      }

    },

    # AVAILABLE SURVEYS
    #' Creates data.frame of avaialble surveys using \code{downloadable_surveys} and caches it (as takes ages)
    available_surveys = function(output_dir = file.path(private$root,"avaialable_surveys"),
                                 your_email,
                                 your_password,
                                 your_project,
                                 max_urls=NULL){

      # create key for this
      key <- paste0(your_project,",",max_urls)

      # first check against cache
      out <- tryCatch(private$storr$get(key,"available_survey_calls"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if(!is.null(out)){ return(out) } else {

        # Get downloadable surveys
        resp <- available_surveys(your_email = your_email,
                                  your_password = your_password ,
                                  your_project = your_project,
                                  output_dir = output_dir,
                                  max_urls = NULL)

        ## then cache the resp if we haven't stopped already and return the parsed resp
        private$storr$set(key,resp,"available_survey_calls")
        return(parsed_resp)

      }

    },

    # DONWLOAD SURVEYS
    #' Creates data.frame of avaialble surveys using \code{downloadable_surveys} and caches it (as takes ages)
    download_survey = function(your_email,
                                 your_password,
                                 your_project,
                                desired_survey){


      # create key for this
      key <- paste0(httr::parse_url(desired_survey$full_url)$query$Filename)

      # first check against cache
      out <- tryCatch(private$storr$get(key,"downloaded_surveys"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if(!is.null(out)){ return(out) } else {

        # Download survey
        download_datasets(your_email=your_email,
                          your_password=your_password,
                          your_project=your_project,
                          desired_surveys=desired_survey)


        # read the dataset in
        #foreign::read.dta(desired_survey$output_folder)

        ## then cache the resp if we haven't stopped already and return the parsed resp
        private$storr$set(key,resp,"downloaded_surveys")
        return(parsed_resp)

      }

    },


    # GETTERS
    get_cache_date = function() private$cache_date,

    # SETTERS
    set_cache_date = function(date) private$cache_date = date,

    # SAVE CLIENT
    save_client = function() saveRDS(self,file.path(private$root,client_file_name()))

  ),

  private = list(api_key = NULL,
                 root = NULL,
                 cache_date = Sys.time(),
                 url = "https://api.dhsprogram.com/rest/dhs/",
                 api_endpoints = c("data","indicators","countries","surveys",
                                   "surveycharacteristics","publications","datasets",
                                   "geometry","tags","dataupdates","uiupdates","info"),
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

