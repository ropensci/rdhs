
##' Make a DHS API client
##'
##' @title Make a dhs client
##'
##' @param credentials File path to where log in credentials are stored (preferred as no
##' secrets are typed into an R session). File format should be (each bullet is a new line):
##' \itemize{
##'       \item email=dummy@gmail.com
##'       \item password=dummypass
##'       \item project=Dummy Project
##'       }
##' @param root Character for root directory to where client, caches, surveys etc. will be stored.
##' Default = \code{rappdirs::user_cache_dir("rdhs", Sys.info()["user"])}
##' @param api_key Character for DHS API KEY
##'
##' @template client_methods
##' @export
##'
dhs_client <- function(credentials=NULL,
                       root = rappdirs::user_cache_dir("rdhs",Sys.info()["user"]),
                       api_key="ICLSPH-527168"){

  # handle credentials first
  handle_credentials(credentials)

  # check rdhs against api last update time
  cache_date <- client_cache_date(root=root)
  if(last_api_update() > cache_date){

    # create new client if DHS database has been updated
    client <- R6_dhs_client$new(api_key,root,credentials)

    # If there was already a client in your root (i.e. there was a DHS update)
    # then empty the api_call cache namespace and check package version
    if(cache_date!=-1){

      message("DHS API has been updated since you last created a DHS client in this root directory.")
      message("Previous API / dataset requests will subsequently be rerun in order to ensure your results are up to date. :)")
      client$clear_namespace(namespace = "api_calls")
      client$clear_namespace(namespace = "available_datasets_calls")

      ## clear any now old dataset calls
      ## ----------------------------------------------------

      # fetch the dataupdates api endpoint
      updates <- client$dhs_api_request(api_endpoint = "dataupdates")

      # are any of the listed updates more recent than the cache date
      if(max(lubridate::mdy_hms(updates$UpdateDate))>client$get_cache_date()){

        # check which datasets have been downloaded in the past
        downloaded_dataset_keys <-  client$.__enclos_env__$private$storr$list("downloaded_datasets")
        downloaded_surveyIds <- strsplit(downloaded_dataset_keys,"_") %>% lapply(function(x) x[1]) %>% unlist
        datasets_to_clear <- which(downloaded_surveyIds %in% updates$SurveyId[lubridate::mdy_hms(updates$UpdateDate)>client$get_cache_date()])
        # do any of them match those that have been updated since the last cache_date
        if(length(datasets_to_clear) > 0){

          for(key in downloaded_dataset_keys[datasets_to_clear]) {

            client$.__enclos_env__$private$storr$del(key,"downloaded_datasets")
            client$.__enclos_env__$private$storr$del(key,"downloaded_datasets_variable_names")
          }

        }
      }

    }

    return(client)

    # if no api updates have occurred then get the cached api client
  } else {

    # load cached client
    client <- readRDS(file.path(root,client_file_name()))

    # check client against rdhs pacakge version
    # so that the R6 functions definitely work with any future pacakge version
    if(packageVersion("rdhs")!=client$.__enclos_env__$private$package_version){
      message("New version of rdhs detected. Your saved client will be updated.")
      client <- R6_dhs_client$new(api_key,root,credentials)
    }

    return(client)

  }
}

R6_dhs_client <- R6::R6Class(

  classname = "dhs_client",
  cloneable = FALSE,

  # PUBLIC METHODS
  # -------------------------------------------------------------------------------------------------------
  public = list(

    # INITIALISATION
    initialize = function(api_key = NULL, root = NULL, credentials=NULL){
      private$api_key <- api_key
      private$root <- root
      if(!is.null(credentials)) private$credentials_path <- normalizePath(credentials)
      private$storr <- storr::storr_rds(file.path(root,"db"))
      private$cache_date <- Sys.time()
      saveRDS(self,file.path(root,client_file_name()))
    },

    # API REQUESTS
    # will either return your request as a parsed json (having cached the result), or will return an error
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
      key <- digest::digest(key)

      # first check against cache
      out <- tryCatch(private$storr$get(key,"api_calls"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if(!is.null(out)){ return(out) } else {

        # Get request
        resp <- httr::GET(url,httr::accept_json(),httr::user_agent("https://github.com/OJWatson/rdhs"),encode = "json")

        ## pass to response parse
        parsed_resp <- handle_api_response(resp,TRUE)
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
          parsed_resp <- handle_api_response(resp,TRUE)

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
              temp_parsed_resp <- handle_api_response(resp,TRUE)
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

    # AVAILABLE DATASETS
    # Creates data.frame of avaialble datasets using \code{available_datasets} and caches it
    available_datasets = function(clear_cache_first = FALSE){

      # check credentials are good
      if(credentials_not_present()) handle_credentials(private$credentials_path)

      # clear the cache for this if set to do so. This is only included here if the user
      # has recently had a change to the datasets they have been allowed to access and want
      # to ensure they are accessing their new available datasets
      if(clear_cache_first) self$clear_namespace(namespace = "available_datasets_calls")

      # create key for this
      key <- digest::digest(paste0(Sys.getenv("rdhs_USER_PROJECT"),","))

      # first check against cache
      out <- tryCatch(private$storr$get(key,"available_datasets_calls"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if(!is.null(out)){ return(out) } else {

        # Get downloadable datasets
        resp <- available_datasets(your_email=Sys.getenv("rdhs_USER_EMAIL"),
                                   your_password=Sys.getenv("rdhs_USER_PASS"),
                                   your_project=Sys.getenv("rdhs_USER_PROJECT"),
                                   datasets_api_results = self$dhs_api_request("datasets",num_results = "ALL"),
                                   surveys_api_results = self$dhs_api_request("surveys",num_results = "ALL"))
        resp <- data.table::as.data.table(resp)

        ## then cache the resp if we haven't stopped already and return the parsed resp
        private$storr$set(key,resp,"available_datasets_calls")
        return(resp)

      }

    },

    # GET DATASETS
    # Gets datasets provided, either by downloading or retrieving from the cache
    get_datasets = function(dataset_filenames,
                                 download_option="rds",
                                 reformat=FALSE,
                                 all_lower=TRUE,
                                 output_dir_root=file.path(private$root,"datasets"),
                                 ...){

      # check credentials are good
      if(credentials_not_present()) handle_credentials(private$credentials_path)

      # fetch which datasets you can download from your login
      datasets <- private$check_available_datasets(dataset_filenames)

      # results storage
      res <- list()

      # possible download options:
      download_possibilities <- c("zip","rds","both")
      download_option <- download_possibilities[grep(paste0(strsplit(download_option,"") %>% unlist,collapse="|"),
                                                     download_possibilities,ignore.case = TRUE)]
      if(!is.element(download_option,download_possibilities)) stop ("Download option provided is not valid")

      # handle for more than one dataset specified
      download_iterations <- length(res) <- dim(datasets)[1]
      names(res) <- strsplit(datasets$FileName,".",fixed=T) %>% lapply(function(x)x[1]) %>% unlist

      # iterate through download requests
      for(i in 1:download_iterations){

        # key from file name
        filename <- strsplit(datasets[i,]$FileName,".",fixed=TRUE)[[1]][1]

        # create key for this
        key <- paste0(datasets[i,]$SurveyId,"_",filename,"_",download_option,"_",reformat)

        # first check against cache
        out <- tryCatch(private$storr$get(key,"downloaded_datasets"),
                        KeyError = function(e) NULL)

        # check out agianst cache, if fine then return just that
        if(!is.null(out)){

          res[[i]] <- out

        } else {

          # Download dataset
          resp <- download_datasets(your_email=Sys.getenv("rdhs_USER_EMAIL"),
                                    your_password=Sys.getenv("rdhs_USER_PASS"),
                                    your_project=Sys.getenv("rdhs_USER_PROJECT"),
                                    desired_dataset=datasets[i,],
                                    output_dir_root=output_dir_root,
                                    download_option=download_option,
                                    all_lower=all_lower,
                                    reformat=reformat,
                                    ...)

          # if there were 2 results returned with these names then we cache them into different namespace
          # the reason for this is it's really helpful to have the questions in each dataset quickly accessible without having
          # to load the dataset each time. And we cache the dataset path rather than the full dataset so that people can more
          # quickly jump and grab a dataset from the rds iin the datasets directory rather than having to go into the db directory

          if(identical(names(resp),c("dataset","variable_names"))){
            private$storr$set(key,resp$dataset,"downloaded_datasets")
            private$storr$set(key,resp$variable_names,"downloaded_dataset_variable_names")
            res[[i]] <- resp$dataset
          } else {
            ## then cache the resp and store it in the results list
            private$storr$set(key,resp,"downloaded_datasets")
            res[[i]] <- resp
          }

        }
      }

      # return vector of paths
      res <- unlist(res)
      
      # add the reformat as an attribute to make life easier is survey_questions/variables
      attr(res,which = "reformat") <- reformat
      return(res)
    },

    # SURVEY_QUESTIONS
    # Creates data.frame of all survey variables and descriptions, with an option to filter by search terms
    survey_questions = function(dataset_filenames,
                                search_terms = NULL,
                                essential_terms = NULL,
                                regex = NULL,
                                ...){

      # check credentials are good
      if(credentials_not_present()) handle_credentials(private$credentials_path)

      # fetch which datasets you can download from your login
      datasets <- private$check_available_datasets(dataset_filenames)

      # download any datasets that need to be downloaded
      download <- self$get_datasets(datasets$FileName,...)

      # handle the search terms
      if(is.null(regex) & is.null(search_terms)) stop ("One of search terms or regex must not be NULL")
      if(is.null(search_terms)){
        pattern <- paste0(regex,essential_terms,collapse="|")
      } else {
        pattern <- paste0(search_terms,essential_terms,collapse="|")
        if(!is.null(regex)) message(paste0("Both regex and search_terms were provided.",
                                           "search_terms will be used.",
                                           "To use regex for searching, do not specify a search_terms argment"))
      }

      # results storage
      df <- data.frame("code"= character(0),"description"= character(0),
                       "dataset_filename"= character(0), "dataset_path" = character(0),
                       "country_code"= character(0), "survey_year" = character(0))
      res <- list()
      download_iteration <- length(res) <- dim(datasets)[1]
      names(res) <- strsplit(datasets$FileName,".",fixed=T) %>% lapply(function(x)x[1]) %>% unlist

      # iterate through downloaded surveys
      for(i in 1:download_iteration){

        # key from file name
        filename <- strsplit(datasets[i,]$FileName,".",fixed=TRUE)[[1]][1]

        # create key for this
        key <- paste0(datasets[i,]$SurveyId,"_",filename,"_","rds",
                      "_",attr(download,which = "reformat"))

        # first check against cache
        out <- tryCatch(private$storr$get(key,"downloaded_datasets"),
                        KeyError = function(e) NULL)

        out_descr <- tryCatch(private$storr$get(key,"downloaded_dataset_variable_names"),
                              KeyError = function(e) NULL)

        # add the survey file path to the res list
        res[[i]] <- out

        # match on search terms and remove questions that have na's
        matched_rows <- grep(pattern = pattern,out_descr$description,ignore.case = TRUE)
        na_from_match <- grep(private$na_s,out_descr$description[matched_rows],ignore.case = TRUE)
        if(length(na_from_match)>0){
          matched_rows <- matched_rows[-grep(private$na_s,out_descr$description[matched_rows],ignore.case = TRUE)]
        }

        # only add if we have found any questions that match
        if(length(matched_rows)>0){

          # add the descriptions to the df object
          df <- rbind(df,data.frame("variable"=out_descr$variable[matched_rows],
                                    "description"=out_descr$description[matched_rows],
                                    "dataset_filename"=rep(names(res[i]),length(matched_rows)),
                                    "dataset_path" = rep(res[[i]],length(matched_rows)),
                                    "country_code" = rep(datasets[i,]$DHS_CountryCode,length(matched_rows)),
                                    "survey_year" = rep(datasets[i,]$SurveyYear,length(matched_rows)),
                                    stringsAsFactors = FALSE))

        }
      }

      # now remove datasets that do not have essential terms:
      if(!is.null(essential_terms)){
          if(sum(is.na(grep(essential_terms,df$description)))>0){
            df <- df[grepl(essential_terms,df$description),]
          }
      }


      # Return the questions, codes and surveys data.frame
      return(df)

    },

    # SURVEY_VARIABLES
    # Creates data.frame of wanted survey variables and descriptions
    survey_variables = function(dataset_filenames,
                            variables,
                            essential_variables = NULL,
                            ...){


      # check credentials are good
      if(credentials_not_present()) handle_credentials(private$credentials_path)

      # fetch which datasets you can download from your login
      datasets <- private$check_available_datasets(dataset_filenames)

      # first download any datasets needed
      download <- self$get_datasets(datasets$FileName,...)

      # results storage
      df <- data.frame("code"= character(0),"description"= character(0),
                       "dataset_filename"= character(0), "dataset_path" = character(0),
                       "country_code"= character(0), "survey_year" = character(0))
      res <- list()
      download_iteration <- length(res) <- dim(datasets)[1]
      names(res) <- strsplit(datasets$FileName,".",fixed=T) %>% lapply(function(x)x[1]) %>% unlist

      # iterate through datasets
      for(i in 1:download_iteration){

        # key from file name
        filename <- strsplit(datasets[i,]$FileName,".",fixed=TRUE)[[1]][1]

        # create key for this
        key <- paste0(datasets[i,]$SurveyId,"_",filename,"_","rds",
                      "_",attr(download,which = "reformat"))

        # Get description and dataset path and find the matched_rows for the requested variables
        out_descr <- private$storr$get(key,"downloaded_dataset_variable_names")
        res[[i]] <- private$storr$get(key,"downloaded_datasets")

        # handle for case mismatches - we'll do this rather than allow people to cache agianst the case
        # they have specified with all_lower as that is ridiculous memory wastage.

        # if the description first variable is upper then they all are and we'll force the variables and
        # essential variables to be the same for for matching. If not then all lower and do the same
        if(is_uppercase(out_descr$variable[1])){
          variables <- toupper(variables)
          if(!is.null(essential_variables)) essential_variables <- toupper(essential_variables)
        } else {
          variables <- tolower(variables)
          if(!is.null(essential_variables)) essential_variables <- tolower(essential_variables)
        }

        # no let's match
        matched_rows <- na.omit(match(variables,out_descr$variable))

        # remove na results
        na_from_match <- grep(private$na_s,out_descr$description[matched_rows],ignore.case = TRUE)
        if(length(na_from_match)>0){
          matched_rows <- matched_rows[-grep(private$na_s,out_descr$description[matched_rows],ignore.case = TRUE)]
        }

        # only add if we have found any questions that match
        if(length(matched_rows)>0){

          # add the descriptions to the df object
          df <- rbind(df,data.frame("variable"=out_descr$variable[matched_rows],
                                    "description"=out_descr$description[matched_rows],
                                    "dataset_filename"=rep(names(res[i]),length(matched_rows)),
                                    "dataset_path" = rep(res[[i]],length(matched_rows)),
                                    "country_code" = rep(datasets[i,]$DHS_CountryCode,length(matched_rows)),
                                    "survey_year" = rep(datasets[i,]$SurveyYear,length(matched_rows)),
                                    stringsAsFactors = FALSE))
        }
      }


      # now remove datasets that do not have essential codes:
      if(!is.null(essential_variables)){
        for(i in unique(df$dataset_filename)){
          if(sum(is.na(match(essential_variables,df$variable[df$dataset_filename==i])))>0){
            df <- df[-which(df$dataset_filename==i),]
          }
        }
      }

      # return the finished df
      return(df)

    },

    # EXTRACTION
    extract = function(questions,
                       add_geo=TRUE){

      # append the filename as survey to the datasets for easier matching later
      datasets <- self$available_datasets()
      datasets$Survey <- strsplit(datasets$FileName,".",fixed=T) %>% lapply(function(x) x[1]) %>% unlist

      ## get geo_surveys if needed
      if(add_geo){
        hhs_geo <- which(datasets$FileType %in% c("Geographic Data"))
        ge_match <- which(datasets$SurveyNum %in% datasets$SurveyNum[match(unique(questions$dataset_filename),datasets$Survey)] &
                            datasets$FileType=="Geographic Data")

        if(sum(!is.na(ge_match))>0){
          geo_surveys <- self$get_datasets(dataset_filenames = datasets$FileName[ge_match],
                                                download_option = "rds")
        }
      }

      ## fetch the results
      res <- extraction(questions,datasets,geo_surveys,add_geo)
      return(res)

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

  private = list(

    api_key = NULL,
    root = NULL,
    credentials_path = NULL,
    cache_date = Sys.time(),
    package_version = packageVersion("rdhs"),
    url = "https://api.dhsprogram.com/rest/dhs/",
    api_endpoints = c("data","indicators","countries","surveys",
                      "surveycharacteristics","publications","datasets",
                      "geometry","tags","dataupdates","uiupdates","info"),
    storr = NULL,
    na_s = na_s <- "^na -|^na-|.*-na$|.* - na$| \\{NA\\}$",


    # CHECK_AVAIALABLE_DATASETS
    check_available_datasets = function(filenames){

      # fetch which datasets you can download from your login
      available_datasets <- self$available_datasets()

      # check that the requested filenames are available
      found_datasets <- match(filenames, available_datasets$FileName)
      if(sum(is.na(found_datasets))>0) {
        message(paste0("These requested datasets are not available from your DHS login credentials:\n---\n",
                       paste0(filenames[which(is.na(found_datasets))],collapse="\n"),
                       "\n---\nPlease request permission for these datasets from the DHS website to be able to download them"))
      }

      # create the datasets data.frame that will then be used to download datasets
      datasets <- available_datasets[na.omit(found_datasets),]

      return(datasets)
    }


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

