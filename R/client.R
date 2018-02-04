
##' Make a DHS API client
##'
##' @title Make a dhs client
##'
##' @param api_key Character for DHS API KEY
##' @param root Character for root directory to where client, caches, surveys etc. will be stored.
##' Default = \code{rappdirs::user_cache_dir("rdhs",Sys.info()["user"])}
##' @param credentials File path to where log in credentials are stored (preferred as no
##' secrets are typed into an R session). File format should be (each bullet is a new line):
##' \itemize{
##'       \item email=dummy@gmail.com
##'       \item password=dummypass
##'       \item project=Dummy Project
##'       }
##' Could also be a list (less preffered), e.g. list("email"=dummy@gmail.com,"password"=dummy,"project"=Dummy Project)
##' @param ... Passed to \code{R6_dhs_client}
##'
##' @template dhs_client_methods
##' @export
##'
dhs_client <- function(api_key=NULL,
                       root = rappdirs::user_cache_dir("rdhs",Sys.info()["user"]),
                       credentials=NULL,
                       ...) {

  # handle credentials first
  if(is.null(credentials)){
    if (identical(Sys.getenv("rdhs.USER_PASS"), "") | identical(Sys.getenv("rdhs.USER_EMAIL"), "") | identical(Sys.getenv("rdhs.USER_PROJECT"), "")){
      stop("Credentials are not present in your system environment. Please provide credentials argument")
    }
  } else {
    set_environment_credentials(read_credentials(credentials))
  }

  # check rdhs against api last update time
  cache_date <- dhs_cache_date(root=root)
  if(dhs_last_update() > cache_date){

    # create new client if DHS database has been updated
    client <- R6_dhs_client$new(api_key,root,...)

    # If there was already a client in your root (i.e. there was a DHS update)
    # then empty the api_call cache namespace
    if(cache_date!=-1){

      message("DHS API has been updated since you last created a DHS client in this root directory.")
      message("Previous API / survey requests will subsequently be rerun in order to sure your results are up to date. :)")
      client$clear_namespace(namespace = "api_calls")
      client$clear_namespace(namespace = "available_survey_calls")

      ## clear any now old survey calls
      ## ----------------------------------------------------

      # fetch the dataupdates api endpoint
      updates <- client$dhs_api_request(api_endpoint = "dataupdates")

      # are any of the listed updates more recent than the cache date
      if(max(lubridate::mdy_hms(updates$UpdateDate))>client$get_cache_date()){

        # check which surveys have been downloaded in the past
        downloaded_survey_keys <-  client$.__enclos_env__$private$storr$list("downloaded_surveys")
        downloaded_surveyIds <- strsplit(downloaded_survey_keys,"_") %>% lapply(function(x) x[1]) %>% unlist
        surveys_to_clear <- which(downloaded_surveyIds %in% updates$SurveyId[lubridate::mdy_hms(updates$UpdateDate)>client$get_cache_date()])
        # do any of them match those that have been updated since the last cache_date
        if(length(surveys_to_clear) > 0){

          for(key in downloaded_survey_keys[surveys_to_clear]) {

            client$.__enclos_env__$private$storr$del(key,"downloaded_surveys")
            client$.__enclos_env__$private$storr$del(key,"downloaded_survey_code_descriptions")
          }

        }
      }
    }
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
      private$storr <- storr::storr_rds(file.path(root,"db"))
      private$cache_date <- Sys.time()
      saveRDS(self,file.path(root,client_file_name()))
    },

    # API REQUESTS
    #' will either return your request as a parsed json (having cached the result), or will return an error
    dhs_api_request = function(api_endpoint,
                               query = list(),
                               api_key = private$api_key,
                               num_results = 100,
                               just_results = TRUE){


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

      # then build in pages. If they have asked for all results catch this and standardise
      if(is.element(num_results,c("ALL","all","a","al","A","AL","ALl","All","AlL"))){
        query$perPage <- 100
        num_results <- "ALL"
      } else {
        query$perPage <- num_results
      }

      # Build url query and associated key
      url <- httr::modify_url(paste0(private$url,api_endpoint),query = query)

      # SLight hacking to deal eith page numbers etc now
      pp <- which(names(query)=="perPage")
      key <- paste0(api_endpoint,"_",paste0(names(query)[-pp],unlist(query)[-pp],collapse=","),",num_results",num_results,",just_results",just_results)

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
        if(parsed_resp$RecordsReturned==0) message("Records returned equal to 0. Most likely your query terms are too specific or there is a typo that does not trigger a 404 or 500 error")

        # Now let's address the num_results argument

        # if the first call has caught all the results then great
        if(parsed_resp$RecordsReturned == parsed_resp$RecordCount){
          if(just_results) parsed_resp <- data.table::rbindlist(parsed_resp$Data)
        } else {

          # if not then query with either max possible or their requested amount
          if(num_results=="ALL") query$perPage <- 5000 else query$perPage <- num_results

          # Create new request and parse this
          url <- httr::modify_url(paste0(private$url,api_endpoint),query = query)
          resp <- httr::GET(url,httr::accept_json(),encode = "json")
          parsed_resp <- dhs_client_response(resp,TRUE)

          # if this larger page query has returned all the results then return this else we will loop through
          if(parsed_resp$TotalPages == 1){
            if(just_results) parsed_resp <- data.table::rbindlist(parsed_resp$Data)
          } else {

            # save the resp as a temp and then make parsed_resp the list we will loop requests into
            temp_parsed_resp <- parsed_resp
            parsed_resp <- list(); length(parsed_resp) <- temp_parsed_resp$TotalPages
            parsed_resp[[1]] <- temp_parsed_resp
            for(i in 2:length(parsed_resp)){
              query$page <- i
              url <- httr::modify_url(paste0(private$url,api_endpoint),query = query)
              resp <- httr::GET(url,httr::accept_json(),encode = "json")
              temp_parsed_resp <- dhs_client_response(resp,TRUE)
              parsed_resp[[i]] <- temp_parsed_resp
            }

            # and now concatenate the results
            if(just_results) parsed_resp <- data.table::rbindlist(do.call(c,lapply(parsed_resp,function(x) x$Data)))
          }
        }
        ## then cache the resp if we haven't stopped already and return the parsed resp
        private$storr$set(key,parsed_resp,"api_calls")
        return(parsed_resp)

      }

    },

    # AVAILABLE SURVEYS
    #' Creates data.frame of avaialble surveys using \code{available_durveys} and caches it
    available_surveys = function(your_email=Sys.getenv("rdhs.USER_EMAIL"),
                                 your_password=Sys.getenv("rdhs.USER_PASS"),
                                 your_project=Sys.getenv("rdhs.USER_PROJECT"),
                                 datasets_api_results = self$dhs_api_request("datasets",num_results = "ALL"),
                                 surveys_api_results = self$dhs_api_request("surveys",num_results = "ALL")){

      # create key for this
      key <- paste0(your_project,",")

      # first check against cache
      out <- tryCatch(private$storr$get(key,"available_survey_calls"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if(!is.null(out)){ return(out) } else {

        # Get downloadable surveys
        resp <- available_surveys(your_email = your_email,
                                  your_password = your_password ,
                                  your_project = your_project,
                                  datasets_api_results = datasets_api_results,
                                  surveys_api_results = surveys_api_results)

        ## then cache the resp if we haven't stopped already and return the parsed resp
        private$storr$set(key,resp,"available_survey_calls")
        return(resp)

      }

    },

    # DONWLOAD SURVEYS
    #' Creates data.frame of avaialble surveys using \code{downloadable_surveys} and caches it (as takes ages)
    download_survey = function(output_dir_root=file.path(private$root,"surveys"),
                               desired_survey,
                               download_option="rds",
                               reformat=TRUE,
                               your_email=Sys.getenv("rdhs.USER_EMAIL"),
                               your_password=Sys.getenv("rdhs.USER_PASS"),
                               your_project=Sys.getenv("rdhs.USER_PROJECT")){


      # results storage
      res <- list()
      surveys <- desired_survey

      # possible download options:
      download_possibilities <- c("zip","ex","rds","both")
      download_option <- download_possibilities[grep(paste0(strsplit(download_option,"") %>% unlist,collapse="|"),download_possibilities)]
      if(!is.element(download_option,download_possibilities)) stop ("Download option provided is not valid")

      # handle for more than one survey specified
      download_iteration <- length(res) <- dim(surveys)[1]
      names(res) <- strsplit(surveys$FileName,".",fixed=T) %>% lapply(function(x)x[1]) %>% unlist

      # grab the api declared datasets list
      datasets_api_results <- self$dhs_api_request("datasets",num_results = "ALL")

      for(i in 1:download_iteration){

        # key from file name
        filename <- strsplit(surveys[i,]$FileName,".",fixed=TRUE)[[1]][1]

        # create key for this
        key <- paste0(surveys[i,]$SurveyId,"_",filename,"_",download_option,"_",reformat)

        # first check against cache
        out <- tryCatch(private$storr$get(key,"downloaded_surveys"),
                        KeyError = function(e) NULL)

        # check out agianst cache, if fine then return just that
        if(!is.null(out)){

          res[[i]] <- out

        } else {

          # Download survey
          resp <- download_datasets(your_email=your_email,
                                    your_password=your_password,
                                    your_project=your_project,
                                    desired_survey=surveys[i,],
                                    output_dir_root=output_dir_root,
                                    download_option=download_option,
                                    reformat=reformat)

          # if there were 2 results returned with these names
          if(identical(names(resp),c("Survey","Survey_Code_Descriptions"))){
            private$storr$set(key,resp$Survey,"downloaded_surveys")
            private$storr$set(key,resp$Survey_Code_Descriptions,"downloaded_survey_code_descriptions")
          } else {
            ## then cache the resp and store it in the results list
            private$storr$set(key,resp,"downloaded_surveys")
          }

          res[[i]] <- resp$Survey

        }
      }

      return(res)
    },


    # SURVEY_QUESTIONS
    #' Creates data.frame of all survey codes and descriptions, with an option to filter by search terms
    survey_questions = function(desired_survey,
                                search_terms = NULL,
                                regex = NULL,
                                output_dir_root=file.path(private$root,"surveys"),
                                your_email=Sys.getenv("rdhs.USER_EMAIL"),
                                your_password=Sys.getenv("rdhs.USER_PASS"),
                                your_project=Sys.getenv("rdhs.USER_PROJECT")){


      # results storage
      df <- data.frame("Code"= character(0),"Description"= character(0),"Survey"= character(0))
      res <- list()

      # shorter than desired_survey
      surveys <- desired_survey

      # handle the search terms
      if(is.null(regex) & is.null(search_terms)) stop ("One of search terms or regex must not be NULL")
      if(is.null(search_terms)){
        pattern <- regex
      } else {
        pattern <- paste0(search_terms,collapse="|")
        if(!is.null(regex)) message(paste0("Both regex and search_terms were provided.",
                                           "search_terms will be used.",
                                           "To use regex for searching, do not specify a search_terms argment"))
      }


      # possible download options:
      download_possibilities <- c("zip","ex","rds","both")
      download_option <- download_possibilities[grep(paste0(strsplit("rds","") %>% unlist,collapse="|"),download_possibilities)]
      if(!is.element(download_option,download_possibilities)) stop ("Download option provided is not valid")

      # handle for more than one survey specified
      download_iteration <- length(res) <- dim(surveys)[1]
      names(res) <- strsplit(surveys$FileName,".",fixed=T) %>% lapply(function(x)x[1]) %>% unlist

      # grab the api declared datasets list
      datasets_api_results <- self$dhs_api_request("datasets",num_results = "ALL")

      for(i in 1:download_iteration){

        # key from file name
        filename <- strsplit(surveys[i,]$FileName,".",fixed=TRUE)[[1]][1]

        # create key for this
        key <- paste0(surveys[i,]$SurveyId,"_",filename,"_",download_option,"_","TRUE")

        # first check against cache
        out <- tryCatch(private$storr$get(key,"downloaded_surveys"),
                        KeyError = function(e) NULL)

        out_descr <- tryCatch(private$storr$get(key,"downloaded_survey_code_descriptions"),
                        KeyError = function(e) NULL)

        # check out agianst cache, if not fine then download
        if(is.null(out) & is.null(out_descr)){

          # Download survey
          resp <- download_datasets(your_email=your_email,
                                    your_password=your_password,
                                    your_project=your_project,
                                    desired_survey=surveys[i,],
                                    output_dir_root=output_dir_root,
                                    download_option="rds",
                                    reformat="TRUE")

          # cache survey results and store them to the res list
          private$storr$set(key,resp$Survey,"downloaded_surveys")
          private$storr$set(key,resp$Survey_Code_Descriptions,"downloaded_survey_code_descriptions")

          out <- resp$Survey
          out_descr <- resp$Survey_Code_Descriptions

        }

          # add the survey file path to the res list
          res[[i]] <- out

          # match on search terms and remove questions that have na's
          matched_rows <- grep(pattern = paste0(search_terms,collapse="|"),out_descr$Description)
          matched_rows <- matched_rows[-grep("na -|na-",out_descr$Description[matched_rows],ignore.case = TRUE)]

          # only add if we have found any questions that match
          if(length(matched_rows)>0){

          # add the descriptions to the df object
          df <- rbind(df,data.frame("Code"=out_descr$Code[matched_rows],
                                    "Description"=out_descr$Description[matched_rows],
                                    "Survey"=rep(names(res[i]),length(matched_rows)),
                                    stringsAsFactors = FALSE
                                    )
                      )

          }


      }

      # combine this process and then return it (maybe cache the greps)
      results <- list("Surveys"=res,"Survey_Questions"=df)
      return(results)

    },

    # GETTERS
    get_cache_date = function() private$cache_date,
    get_root = function() private$root,

    # SETTERS
    set_cache_date = function(date) private$cache_date = date,

    # SAVE CLIENT
    save_client = function() saveRDS(self,file.path(private$root,client_file_name())),

    # CLEAR NAMESPACE
    clear_namespace = function(namespace){
      private$storr$clear(namespace = namespace)
      private$storr$gc()
    }

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

