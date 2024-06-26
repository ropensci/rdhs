#' Make a DHS API client
#'
#' @title Make a dhs client
#'
#' @param config config object, as created using \code{read_rdhs_config}
#' @param root Character for root directory to where client, caches,
#'   surveys etc. will be stored. Default = \code{rappdirs_rdhs()}
#' @param api_key Character for DHS API KEY. Default = NULL
#'
#' @template client_dhs_methods
#' @export
#'
#' @examples
#' \dontrun{
#' # create an rdhs config file at "rdhs.json"
#' conf <- set_rdhs_config(
#' config_path = "rdhs.json",global = FALSE, prompt = FALSE
#' )
#' td <- tempdir()
#' cli <- rdhs::client_dhs(api_key = "DEMO_1234", config = conf, root = td)
#' }
client_dhs <- function(config=NULL,
                       root=rappdirs_rdhs(),
                       api_key=NULL) {

  # if api_key is NULL then set it to the default
  if (is.null(api_key)) {
    api_key <- api_key_internal
  }

  # we need to have a config, so we  create the temp one if not provided
  if (is.null(config)) {
    .rdhs$internal_client_update <- FALSE
    config <- set_rdhs_config(prompt = FALSE)
    .rdhs$internal_client_update <- TRUE
  }

  # check rdhs against api last update time
  cache_date <- client_cache_date(root = root)
  if (last_api_update(config$timeout) > cache_date) {

    # create new client if DHS database has been updated
    client <- R6_client_dhs$new(config, api_key, root)

    # If there was already a client in your root (i.e. there was a DHS update)
    # then empty the api_call cache namespace and check package version
    if (cache_date > 0) {
      message("\nDHS API has been updated since you last set up a DHS client\n",
              "in this root directory.")
      message("Previous API / dataset requests will subsequently be rerun in\n",
              "order to ensure your results are up to date.\n")
      client$clear_namespace(namespace = "api_calls")
      client$clear_namespace(namespace = "available_datasets_calls")

      ## clear any now old dataset calls
      # ----------------------------------------------------

      # fetch the dataupdates api endpoint
      upd <- client$dhs_api_request(api_endpoint = "dataupdates")

      # are any of the listed updates more recent than the cache date
      if (max(mdy_hms(upd$UpdateDate)) > cache_date) {

        ## check which datasets have been downloaded in the past

        # first grab the private for readability
        private <- client$.__enclos_env__$private

        # get the surveyIds for all downloaded datasets
        downloaded_dataset_keys <- private$storr$list("downloaded_datasets")
        downloaded_surveyIds <- strsplit(downloaded_dataset_keys, "_") %>%
          lapply(function(x) x[1]) %>%
          unlist()

        # which of the updates have occured since our last client was created
        chge <- upd$SurveyId[mdy_hms(upd$UpdateDate) > client$get_cache_date()]
        datasets_to_clear <- which(downloaded_surveyIds %in% chge)

        # do any of them match those that have been updated since the
        # last cache_date
        if (length(datasets_to_clear) > 0) {
          for (key in downloaded_dataset_keys[datasets_to_clear]) {
            private$storr$del(key, "downloaded_datasets")
            private$storr$del(key, "downloaded_datasets_variable_names")
          }
        }

        # Now do the same for spatial boundaries
        dats <- client$dhs_api_request(api_endpoint = "dataupdates")
        downloaded_spatial_keys <- private$storr$list("spatial_boundaries")
        downloaded_surveyNums <- strsplit(downloaded_spatial_keys, "_") %>%
          lapply(function(x) x[1]) %>%
          unlist()

        # which of the updates have occured since our last client was created
        chge <- dats$SurveyNum[mdy_hms(dats$FileDateLastModified) > client$get_cache_date()]
        datasets_to_clear <- which(downloaded_spatial_keys %in% chge)

        # do any of them match those that have been updated since the
        # last cache_date
        if (length(datasets_to_clear) > 0) {
          for (key in downloaded_spatial_keys[datasets_to_clear]) {
            private$storr$del(key, "spatial_boundaries")
          }
        }

      }
    }

    return(client)

    # if no api updates have occurred then get the cached api client
  } else {

    # create client in the location rather than readRDS so that we
    # are using new config etc
    client <- R6_client_dhs$new(config, api_key, root)

    # load cached client
    private <- client$.__enclos_env__$private

    # spit out a message to say if the client is being updated from
    # a previous rdhs version
    if (packageVersion("rdhs") != private$package_version) {
      message("New version of rdhs detected.",
              "Your saved client will be updated.")
    }

    return(client)
  }
}

R6_client_dhs <- R6::R6Class(
  classname = "client_dhs",
  cloneable = FALSE,

  # PUBLIC METHODS
  public = list(

    # INITIALISATION
    initialize = function(config,
                          api_key,
                          root = rappdirs_rdhs()) {

      if (!inherits(config, "rdhs_config")) {
        stop ("config provided for client is not of class rdhs_config")
      }
      private$api_key <- api_key
      private$root <- root
      private$storr <- storr::storr_rds(file.path(root, "db"))
      private$cache_date <- Sys.time()
      saveRDS(self, file.path(root, client_file_name())) # save before config
      private$config <- config
    },

    # API REQUESTS
    # will either return your request as a parsed json (having cached the
    # result), or will return an error
    dhs_api_request = function(api_endpoint,
                               query = list(),
                               api_key = private$api_key,
                               num_results = 100,
                               just_results = TRUE) {


      # Check api_endpoints first
      if (!is.element(api_endpoint, private$api_endpoints)) {
        stop(paste("Requested api_endpoint is invalid. Must be one one of:",
                   paste(api_endpoints, collapse = "\n"),
                   "For more information check the api website:",
                   "https://api.dhsprogram.com/#/index.html",
                   sep = "\n"))
      }

      # Collapse query list
      query_param_lengths <- lapply(query, length) %>% unlist()

      # collapse where lengths are greater than 1
      for (i in which(query_param_lengths > 1)) {
        query[[i]] <- paste0(query[[i]], collapse = ",")
      }

      # add the api key if it exists
      query$apiKey <- private$api_key

      # then build in pages. If they have asked for all results
      # catch this and standardise
      if (is.element(num_results, c("ALL", "all", "a", "al", "aL", "Al",
                                    "A", "AL", "ALl", "All", "AlL", "aLL",
                                    "aLl", "alL"))) {
        query$perPage <- 100
        num_results <- "ALL"
      } else {
        query$perPage <- num_results
      }

      # Build url query and associated key
      url <- httr::modify_url(paste0(private$url, api_endpoint), query = query)

      # SLight hacking to deal eith page numbers etc now
      pp <- which(names(query) == "perPage")
      key <- paste0(api_endpoint, "_",
                    paste0(
                      names(query)[-pp], unlist(query)[-pp], collapse = ","
                    ),
                    ",num_results", num_results, ",just_results", just_results)

      key <- digest::digest(key)

      # first check against cache
      out <- tryCatch(private$storr$get(key, "api_calls"),
                      KeyError = function(e) NULL)

      # check out agianst cache, if fine then return just that
      if (!is.null(out)) {
        return(out)
      } else {

        # Get request
        resp <- httr::GET(url, httr::accept_json(),
                          httr::user_agent("https://github.com/ropensci/rdhs"),
                          encode = "json")

        ## pass to response parse
        parsed_resp <- handle_api_response(resp, TRUE)
        if (resp$status_code >= 400 && resp$status_code < 600) {
          return(parsed_resp)
        }

        # put some message or return to let the user know if the data
        # returned is empty
        if (parsed_resp$RecordsReturned == 0) {
          message("Records returned equal to 0. Most likely your query
                  terms are too specific or there is a typo that does not
                  trigger a 404 or 500 error")
        }

        # Now let's address the num_results argument

        # if the first call has caught all the results then great
        if (parsed_resp$RecordsReturned == parsed_resp$RecordCount) {
          if (just_results) {
            parsed_resp <- rbind_list_base(parsed_resp$Data)
          }
        } else {

          # if not then query with either max possible or their requested amount
          if (num_results == "ALL") {
            query$perPage <- 5000
          } else {
            query$perPage <- num_results
          }


          # Create new request and parse this
          url <- httr::modify_url(paste0(private$url, api_endpoint),
                                  query = query)

          resp <- httr::GET(
            url, httr::accept_json(),
            httr::user_agent("https://github.com/ropensci/rdhs"),
            encode = "json"
          )

          parsed_resp <- handle_api_response(resp, TRUE)

          # if this larger page query has returned all the results then
          # return this else we will loop through
          if (parsed_resp$TotalPages == 1) {
            if (just_results) {
              parsed_resp <- rbind_list_base(parsed_resp$Data)
            }
          } else {

            # save the resp as a temp and then make parsed_resp the list we
            # will loop requests into
            temp_parsed_resp <- parsed_resp
            parsed_resp <- list()
            length(parsed_resp) <- temp_parsed_resp$TotalPages
            parsed_resp[[1]] <- temp_parsed_resp

            for (i in 2:length(parsed_resp)) {
              query$page <- i
              url <- httr::modify_url(paste0(private$url, api_endpoint),
                                      query = query)

              resp <- httr::GET(
                url, httr::accept_json(),
                httr::user_agent("https://github.com/ropensci/rdhs"),
                encode = "json"
              )

              temp_parsed_resp <- handle_api_response(resp, TRUE)
              parsed_resp[[i]] <- temp_parsed_resp
            }

            # and now concatenate the results
            if (just_results) {
              parsed_resp <- collapse_api_responses(parsed_resp)
            }
          }
        }

        ## then cache the resp if we haven't stopped already and
        # return the parsed resp
        private$storr$set(key, parsed_resp, "api_calls")
        return(parsed_resp)
      }
    },

    # AVAILABLE DATASETS
    # Creates data.frame of available datasets using \code{available_datasets}
    # and caches it
    available_datasets = function(clear_cache_first = FALSE) {

      # check config are good
      if (config_not_present(private$config)) {
        handle_config(private$config$config_path)
      }

      # clear the cache for this if set to do so. This is only included
      # here if the user has recently had a change to the datasets
      # they have been allowed to access and want to ensure they
      # are accessing their new available datasets
      if (clear_cache_first) {
        self$clear_namespace(namespace = "available_datasets_calls")
      }

      # create key for this
      key <- digest::digest(paste0(private$config$project, ","))

      # first check against cache
      out <- tryCatch(private$storr$get(key, "available_datasets_calls"),
                      KeyError = function(e) NULL
      )

      # check out agianst cache, if fine then return just that
      if (!is.null(out)) {
        return(out)
      } else {

        # Get downloadable datasets
        resp <- available_datasets(
          config = private$config,
          datasets_api_results = self$dhs_api_request("datasets",
                                                      num_results = "ALL"),
          surveys_api_results = self$dhs_api_request("surveys",
                                                     num_results = "ALL")
        )

        ## then cache the resp if we haven't stopped already and return
        ## the parsed resp
        private$storr$set(key, resp, "available_datasets_calls")
        return(resp)
      }
    },

    # GET DATASETS
    # Gets datasets provided, either by downloading or retrieving from the cache
    get_datasets = function(dataset_filenames,
                            download_option="rds",
                            reformat=FALSE,
                            all_lower=TRUE,
                            output_dir_root=file.path(private$root, "datasets"),
                            clear_cache = FALSE,
                            ...) {

      # check config are good
      if (config_not_present(private$config)) {
        handle_config(private$config$config_path)
      }

      # if cache needs clearing
      if (clear_cache){
        avs <- self$available_datasets(TRUE)
      }

      # fetch which datasets you can download from your login
      datasets <- private$check_available_datasets(dataset_filenames,
                                                   download_option,
                                                   reformat)

      # results storage
      res <- list()

      # possible download options:
      download_possibilities <- c("zip", "rds", "both")
      dopt <- grep(paste0(strsplit(download_option, "") %>%
                            unlist(), collapse = "|"),
                   download_possibilities, ignore.case = TRUE)

      download_option <- download_possibilities[dopt]
      if (!is.element(download_option, download_possibilities)) {
        stop("Download option provided is not valid")
      }

      # handle for more than one dataset specified
      download_iterations <- length(res) <- dim(datasets)[1]
      names(res) <- datasets$file

      # iterate through download requests
      for (i in 1:download_iterations) {

          # create key for this
          key <- paste0(datasets[i, ]$SurveyId, "_", datasets[i, ]$FileName,
                        "_", download_option, "_", reformat)

          # first check against cache
          out <- tryCatch(private$storr$get(key, "downloaded_datasets"),
                          KeyError = function(e) NULL)

          # check out agianst cache, if fine then return just that
          if (!is.null(out)) {
            res[[i]] <- out
          } else {

        # if no url then place error message in results list
        if (is.na(datasets$URLS[i])) {
          res[[i]] <- "Dataset is not available with your DHS login credentials"
        } else {

            # Download dataset
            resp <- download_datasets(
              config = private$config,
              desired_dataset = datasets[i, ],
              output_dir_root = output_dir_root,
              download_option = download_option,
              all_lower = all_lower,
              reformat = reformat,
              ...
            )

            # if there were 2 results returned with these names then we cache
            # them into different namespace the reason for this is it's really
            # helpful to have the questions in each dataset quickly accessible
            # without having to load the dataset each time. And we cache the
            # dataset path rather than the full dataset so that people can more
            # quickly jump and grab a dataset from the rds iin the datasets
            # directory rather than having to go into the db directory

            if (identical(names(resp), c("dataset", "variable_names"))) {
              private$storr$set(key, resp$dataset,
                                "downloaded_datasets")
              private$storr$set(key, resp$variable_names,
                                "downloaded_dataset_variable_names")
              res[[i]] <- resp$dataset
            } else if (grepl("No support for reading in", resp)) {
              res[[i]] <- resp
            } else {
              ## then cache the resp and store it in the results list
              private$storr$set(key, resp, "downloaded_datasets")
              res[[i]] <- resp
            }
          }
        }
      }
      # add the reformat as an attribute to make life easier is
      # in survey questions/variables
      attr(res, which = "reformat") <- reformat
      return(res)
    },

    # SURVEY_QUESTIONS
    # Creates data.frame of all survey variables and descriptions, with an
    # option to filter by search terms
    survey_questions = function(dataset_filenames,
                                search_terms = NULL,
                                essential_terms = NULL,
                                regex = NULL,
                                rm_na = TRUE,
                                ...) {

      # check config are good
      if (config_not_present(private$config)) {
        handle_config(private$config$config_path)
      }

      # download any datasets that need to be downloaded
      download <- self$get_datasets(dataset_filenames, ...)

      # fetch which datasets you can download from your login
      datasets <- private$check_available_datasets(dataset_filenames, ...)
      datasets <- datasets[!is.na(datasets$URLS), ]

      # handle the search terms
      if (is.null(regex) & is.null(search_terms)) {
        stop("One of search terms or regex must not be NULL")
      }

      if (is.null(search_terms)) {
        pattern <- paste0(regex, essential_terms, collapse = "|")
      } else {
        pattern <- paste0(search_terms, essential_terms, collapse = "|")
        if (!is.null(regex)) {
          message(paste0(
            "Both regex and search_terms were provided.",
            "search_terms will be used.",
            "To use regex for searching, do not specify search_terms"
          ))
        }
      }

      # results storage
      df <- data.frame("code" = character(0), "description" = character(0),
                       "dataset_filename" = character(0),
                       "dataset_path" = character(0),
                       "survey_id" = character(0))

      res <- list()
      download_iteration <- length(res) <- dim(datasets)[1]
      names(res) <- datasets$file

      # iterate through downloaded surveys
      for (i in 1:download_iteration) {

        # create key for this
        key <- paste0(datasets[i, ]$SurveyId, "_",
                      datasets[i, ]$FileName, "_",
                      "rds", "_",
                      attr(download, which = "reformat"))

        # first check against cache
        out <- tryCatch(
          private$storr$get(key, "downloaded_datasets"),
          KeyError = function(e) NULL
        )

        out_desc <- tryCatch(
          private$storr$get(key, "downloaded_dataset_variable_names"),
          KeyError = function(e) NULL
        )

        # add the survey file path to the res list
        res[[i]] <- out

        # match on search terms and remove questions that have na's
        matched_rows <- grep(pattern = pattern, out_desc$description,
                             ignore.case = TRUE)

        if (rm_na) {
          na_from_match <- grep(private$na_s,
                                out_desc$description[matched_rows],
                                ignore.case = TRUE)

          if (length(na_from_match) > 0) {
            matched_rows <- matched_rows[-grep(
              private$na_s,
              out_desc$description[matched_rows],
              ignore.case = TRUE
            )]
          }

        }

        # only add if we have found any questions that match
        if (length(matched_rows) > 0) {

          # add the descriptions to the df object
          df <- rbind(df, data.frame(
            "variable" = out_desc$variable[matched_rows],
            "description" = out_desc$description[matched_rows],
            "dataset_filename" = rep(names(res[i]), length(matched_rows)),
            "dataset_path" = rep(res[[i]], length(matched_rows)),
            "survey_id" = rep(datasets[i, ]$SurveyId, length(matched_rows)),
            stringsAsFactors = FALSE
          ))
        }
      }

      # now remove datasets that do not have essential terms:
      if (!is.null(essential_terms)) {
        if (sum(is.na(grep(essential_terms, df$description))) > 0) {
          df <- df[grepl(essential_terms, df$description), ]
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
                                rm_na = TRUE,
                                ...) {


      # check config are good
      if (config_not_present(private$config)) {
        handle_config(private$config$config_path)
      }

      # first download any datasets needed
      download <- self$get_datasets(dataset_filenames, ...)

      # fetch which datasets you can download from your login
      datasets <- private$check_available_datasets(dataset_filenames, ...)
      datasets <- datasets[!is.na(datasets$URLS), ]

      # results storage
      df <- data.frame(
        "code" = character(0), "description" = character(0),
        "dataset_filename" = character(0), "dataset_path" = character(0),
        "survey_id" = character(0)
      )
      res <- list()
      download_iteration <- length(res) <- dim(datasets)[1]
      names(res) <- datasets$file

      # iterate through datasets
      for (i in 1:download_iteration) {

        # create key for this
        key <- paste0(datasets[i, ]$SurveyId, "_",
                      datasets[i, ]$FileName, "_",
                      "rds", "_",
                      attr(download, which = "reformat"))

        # Get description and dataset path and find the matched_rows for
        # the requested variables

        # first check against cache
        out <- tryCatch(
          private$storr$get(key, "downloaded_datasets"),
          KeyError = function(e) NULL
        )

        out_desc <- tryCatch(
          private$storr$get(key, "downloaded_dataset_variable_names"),
          KeyError = function(e) NULL
        )

        res[[i]] <- out

        if (!is.null(out_desc)) {
          # handle for case mismatches - we'll do this rather than allow people to
          # cache agianst the case they have specified with all_lower as that is
          # ridiculous memory wastage.

          # if the description first variable is upper then they all are and we'll
          # force the variables and essential variables to be the same for for
          # matching. If not then all lower and do the same
          if (is_uppercase(out_desc$variable[1])) {
            variables <- toupper(variables)
            if (!is.null(essential_variables)) {
              essential_variables <- toupper(essential_variables)
            }
          } else {
            variables <- tolower(variables)
            if (!is.null(essential_variables)) {
              essential_variables <- tolower(essential_variables)
            }
          }

          # now let's match
          matched_rows <- na.omit(match(variables, out_desc$variable))

          if (rm_na) {

            # remove na results
            na_from_match <- grep(private$na_s,
                                  out_desc$description[matched_rows],
                                  ignore.case = TRUE)

            if (length(na_from_match) > 0) {
              matched_rows <- matched_rows[-grep(
                private$na_s,
                out_desc$description[matched_rows],
                ignore.case = TRUE
              )]
            }

          }

          # only add if we have found any questions that match
          if (length(matched_rows) > 0) {

            # add the descriptions to the df object
            df <- rbind(df, data.frame(
              "variable" = out_desc$variable[matched_rows],
              "description" = out_desc$description[matched_rows],
              "dataset_filename" = rep(names(res[i]), length(matched_rows)),
              "dataset_path" = rep(res[[i]], length(matched_rows)),
              "survey_id" = rep(datasets[i, ]$SurveyId, length(matched_rows)),
              stringsAsFactors = FALSE
            ))
          }
        }
      }

      # now remove datasets that do not have essential codes:
      if (!is.null(essential_variables)) {
        for (i in unique(df$dataset_filename)) {
          if (sum(is.na(match(essential_variables,
                              df$variable[df$dataset_filename == i]))) > 0) {
            df <- df[-which(df$dataset_filename == i), ]
          }
        }
      }

      # return the finished df
      return(df)
    },

    # EXTRACTION
    extract = function(questions, add_geo=FALSE) {
      if (dim(questions)[1] == 0) {
        stop("questions argument is empty - check your
             survey_questions/variables terms?")
      }

      # are the questions relating to the model datasets
      if (all(substr(unique(questions$dataset_filename), 1, 2) == "zz")) {
        datasets <- rdhs::model_datasets
      } else {
        datasets <- self$available_datasets()
      }

      # append the filename as survey to the datasets for easier matching later
      datasets$Survey <- strsplit(datasets$FileName, ".", fixed = TRUE) %>%
        lapply(function(x) x[1]) %>%
        unlist()

      ## get geo_surveys if needed
      if (add_geo) {
        hhs_geo <- which(datasets$FileType %in% c("Geographic Data"))
        snm <- match(unique(questions$dataset_filename), datasets$Survey)
        ge_match <- which(datasets$SurveyNum %in% datasets$SurveyNum[snm] &
                            datasets$FileType == "Geographic Data")

        if (sum(!is.na(ge_match)) > 0) {
          geo_surveys <- self$get_datasets(
            dataset_filenames = datasets$FileName[ge_match],
            download_option = "rds"
          )
        }
      }

      ## fetch the results
      res <- extraction(questions, datasets, geo_surveys, add_geo)
      return(res)
    },


    # GETTERS
    get_cache_date = function() private$cache_date,
    get_root = function() private$root,
    get_config = function() private$config,

    # get a dataset's var labels
    get_variable_labels = function(dataset_filenames=NULL,
                                   dataset_paths=NULL,
                                   rm_na = FALSE) {

      # catch if both null
      if (is.null(dataset_filenames) && is.null(dataset_paths)) {
        stop("One of dataset_filenames or dataset_paths must not be null")
      }

      # catch if both provided
      if (!is.null(dataset_filenames) && !is.null(dataset_paths)) {
        message("Both of dataset_filenames and dataset_paths are provided.
                The filenames will be used")
        dataset_paths <- NULL
      }

      # grab these now
      filenames <- dhs_datasets(client = self)$FileName
      filenames <- c(filenames, rdhs::model_datasets$FileName)

      # get vars from dataset_paths
      if (!is.null(dataset_paths)) {

        # stop if all poor file paths
        if (all(!file.exists(dataset_paths))) {
          stop(
            "All dataset file paths were not found:\n   ",
            paste0(dataset_paths[!file.exists(dataset_paths)], sep = "\n ")
          )
        }

        # message any poor file paths first
        if (any(!file.exists(dataset_paths))) {
          message(
            "Following dataset file paths were not found:\n   ",
            paste0(dataset_paths[!file.exists(dataset_paths)], sep = "\n ")
          )
        }

        # what have we downloaded
        downs <- self$get_downloaded_datasets()

        # which file paths are these
        mats <- match(dataset_paths[file.exists(dataset_paths)], downs)

        # what keys do these belong to and what were the downloaded options
        # (so we don't download extra files)
        keys <- private$storr$list("downloaded_datasets")[mats]
        options <- strsplit(keys, "_") %>% lapply(function(x) x[c(2, 4)])
        options <- lapply(options, function(x) {
          c(grep(x[1], filenames, value = TRUE), x[2])
        })

        vars <- lapply(options, function(x) {
          self$survey_questions(dataset_filenames = x[1],
                                search_terms = "",
                                reformat = x[2],
                                rm_na = FALSE)
        })
        vars <- rbind_labelled(vars)
      }

      if (!is.null(dataset_filenames)) {

        # just get the ones that exist
        names_matched <- filenames[match(dataset_filenames, filenames)]

        # stop if all poor file names
        if (all(is.na(names_matched))) {
          stop("All dataset file names are not valid:\n   ",
               paste0(dataset_filenames[is.na(names_matched)], sep = "\n "))
        }

        # message any poor file names
        if (any(is.na(names_matched))) {
          message("Following dataset file names are not valid:\n   ",
                  paste0(dataset_filenames[is.na(names_matched)], sep = "\n "))
        }

        # grab the variables using a catch all variables term
        vars <- self$survey_questions(
          dataset_filenames = names_matched[!is.na(names_matched)],
          search_terms = "",
          rm_na = FALSE
        )
      }

      return(vars)
    },


    ## GET_DOWNLOADED_DATASETS
    # Grab all downloaded datasets
    get_downloaded_datasets = function() {

      # grab the keys within the namespace for this
      keys <- private$storr$list("downloaded_datasets")

      # download paths
      downloads <- private$storr$mget(keys, namespace = "downloaded_datasets")
      names(downloads) <- strsplit(
        basename(unlist(downloads)), ".rds", fixed = TRUE
      ) %>% lapply(function(x) x[1]) %>% unlist()

      return(downloads)
    },

    # SETTERS
    set_cache_date = function(date) private$cache_date <- date,

    # SAVE CLIENT
    save_client = function() saveRDS(self, file.path(
      private$root,
      client_file_name()
    )),

    # CLEAR NAMESPACE
    clear_namespace = function(namespace) {
      private$storr$clear(namespace = namespace)
      private$storr$gc()
    }
  ),

  private = list(
    api_key = NULL,
    root = NULL,
    user_declared_root = NULL,
    config = NULL,
    cache_date = Sys.time(),
    package_version = packageVersion("rdhs"),
    url = "https://api.dhsprogram.com/rest/dhs/",
    api_endpoints = c(
      "data", "indicators", "countries", "surveys",
      "surveycharacteristics", "publications", "datasets",
      "geometry", "tags", "dataupdates", "uiupdates", "info"
    ),
    storr = NULL,
    na_s = "^na -|^na-|.*-na$|.* - na$| \\{NA\\}$|.* NA$|.*NA$",


    # CHECK_AVAIALABLE_DATASETS
    check_available_datasets = function(filenames,
                                        download_option="rds",
                                        reformat=FALSE) {

      # catch of the filenames requested are with or without the zip
      if (any(grepl("zip", filenames, ignore.case = TRUE))) {
        nm_type <- "FileName"
      } else {
        nm_type <- "file"
      }

      # ammend our model_datasets first
      model_datasets_ammend <- create_new_filenames(rdhs::model_datasets)

      # if they have only asked for model datasets then return those
      if (all(filenames %in% model_datasets_ammend[[nm_type]])){
        return(model_datasets_ammend[match(filenames, model_datasets_ammend[[nm_type]]), ])
      }

      # fetch which datasets you can download from your login
      avs <- self$available_datasets()
      avs <- create_new_filenames(avs)
      avs <- rbind(avs, model_datasets_ammend)

      # fetch all the datasets so we can catch for the India matches by
      # using the country code catch
      datasets <- dhs_datasets(client = self)
      datasets <-  rbind(datasets, model_datasets_ammend[, -c(14:15)])

      # create new filename argument that takes into account the india
      # difficiulties where needed
      datasets <- create_new_filenames(datasets)

      # look up logic assumes it will be a data frame so change as needed
      if (inherits(datasets, "tbl")) {
        datasets <- as.data.frame(datasets)
      }

      # find all the duplicate filenames and what datasets they belong to
      duplicates <- datasets[duplicated(datasets$FileName), nm_type]
      duplicate_data <- datasets[which(datasets[,nm_type] %in% duplicates), ]

      # because there are duplicate filenames in the API we allow/recommend
      # users to provide as the datasets argument the output of dhs_datasets
      # so that we have the full info about the dataset they want. As such we
      # mow may have filenames that are filenames or the entire API output so
      # let's check this
      if (is.vector(filenames)) {

        # do their requested filenames include any of the duplicates
        duplicates_fnd <- match(toupper(duplicates), toupper(filenames))

        # if there are no duplicates matched then perfect
        if (sum(duplicates_fnd, na.rm = TRUE) == 0) {

          # what is the full set of datasets they have asked for
          potential <- datasets[match(toupper(filenames),
                                      toupper(datasets[,nm_type])), ]

          # now match the requested filenames are available
          found_datasets <- match(toupper(filenames), toupper(avs[,nm_type]))
        } else {

          # let the user know there are duplicate matches and suggest that
          # they clarify using dhs_datasets()
          message(paste0(
            "The following requested dataset file names are used
            by more than one dataset:\n---\n",
            paste0(duplicates[which(!is.na(duplicates_fnd))], collapse = "\n"),
            "\n---\nBy default the above datasets will be downloaded according",
            "to the country code indicated by the first 2 letters of these",
            "datasets. If you wished for the the above datatasets to be",
            "downloaded not based on just their first 2 letters then please",
            "provide the desired rows from the output of dhs_datasets() for",
            "the datasets argument.",
            "See introductory vignette for more info about this issue.",
            collapse = "\n"
          ))

          # unique match strings
          fil_match <- paste0(toupper(substr(filenames, 1, 2)),
                              toupper(filenames))
          dat_match <- paste0(toupper(datasets$DHS_CountryCode),
                              toupper(datasets[,nm_type]))
          avs_match <- paste0(toupper(avs$DHS_CountryCode),
                              toupper(avs[,nm_type]))

          # what is the full set of datasets they have asked for based on
          # the countrycode assumpotion
          potential <- datasets[match(fil_match, dat_match), ]

          # if there are duplicates what we will do is assume that they want
          # the country versions
          found_datasets <- match(fil_match, avs_match)
        }
      } else {

        # unique match strings
        fil_match <- paste0(toupper(filenames$DHS_CountryCode),
                            toupper(filenames[,nm_type]))
        dat_match <- paste0(toupper(datasets$DHS_CountryCode),
                            toupper(datasets[,nm_type]))
        avs_match <- paste0(toupper(avs$DHS_CountryCode),
                            toupper(avs[,nm_type]))

        # what is the full set of datasets they have asked for
        potential <- datasets[match(fil_match, dat_match), ]

        # if they gave the full output then we can match with the
        # provided country code
        found_datasets <- match(fil_match, avs_match)

      }

      # create the datasets data.frame that will
      # then be used to download datasets
      potential$URLS <- avs$URLS[found_datasets]

      # let them know about any datasets that they requested that aren't
      # avaialable for them to download also
      if (sum(is.na(found_datasets)) > 0) {

        # which filenames have failed
        fail_names <- filenames
        if (is.data.frame(fail_names)) {
          fail_names <- filenames[,nm_type]
        }

        # check if they are already downloaded
        key <- paste0(potential$SurveyId, "_", potential$FileName,
                      "_", download_option, "_", reformat)

        # first check against cache
        found <- private$storr$exists(key, "downloaded_datasets")
        fail_names <- fail_names[!found]

        message(
          paste0(
            "These requested datasets are not available from your ",
            "DHS login credentials:\n---\n",
            paste0(fail_names[which(is.na(found_datasets))], collapse = ", "),
            "\n---\nPlease request permission for these datasets from ",
            "the DHS website to be able to download them"
          ))
      }

      return(potential)
    }

  )
)
