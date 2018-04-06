query_creation <- function(query){

  # Collapse query list
  query_param_lengths <- lapply(query,length) %>% unlist

  # collapse where lengths are greater than 1
  for(i in which(query_param_lengths>1)){
    query[[i]] <- paste0(query[[i]],collapse=",")
  }

  # add the api key
  query$apiKey <- "ICLSPH-527168"

  # Return query list
  return(query)
}

handle_api_request <- function(endpoint, query, allResults, client){

  # first clear the query list of any not needed query args
  query$allResults <- NULL ; query$client <- NULL

  # create query and set format to json of not specified
  query <- query_creation(query)
  if(is.null(query$f)) query$f <- "json"

  # if there is no client then make request
  if(is.null(client)){

  # create generic request
  resp <- api_request(endpoint, query, allResults, client)

  } else {

    # create url for api request
    url <- httr::modify_url(endpoint,query = query)

    # create a client cache key for this
    key <- digest::digest(paste0(url,"allResults=",allResults,collapse=""))

    out <- tryCatch(client$.__enclos_env__$private$storr$get(key,"api_calls"),
                    KeyError = function(e) NULL)

    # check out agianst cache, if fine then return just that and if not make request
    if(!is.null(out)){ return(out) } else {

      # create generic request
      resp <- api_request(endpoint, query, allResults, client)

      ## then cache the resp if we haven't stopped already and return the parsed resp
      client$.__enclos_env__$private$storr$set(key,resp,"api_calls")

    }

  }

    return(resp)
}


api_request <- function(endpoint, query, allResults, client){


    # make url for request
    url <- httr::modify_url(endpoint,query = query)

    # if they have specified other than json
    if(query$f != "json"){

      # Make the request
      resp <- httr::GET(url,httr::user_agent("https://github.com/OJWatson/rdhs"))

      # if they have allResults still as TRUE then let them now that not all results will be returned
      if(allResults) message(paste0("Format specified is not equal to 'json'. ",
                                    "allResults will be ignored and only the ",
                                    "first response from the API will be returned. ",
                                    "Set 'f=\"json\"' to return all API results."))

      ## pass to response parse and then return
      parsed_resp <- dhs_client_response(resp,FALSE)
      return(parsed_resp)

    }

    # make the request
    resp <- httr::GET(url,httr::accept_json(),
                      httr::user_agent("https://github.com/OJWatson/rdhs"),
                      encode = "json")

    ## pass to response parse and if its json then grab the data
    parsed_resp <- dhs_client_response(resp,TRUE)
    if(resp$status_code >= 400 && resp$status_code < 600){
      return(parsed_resp)
    }

    # put some message or return to let the user know if the data returned is empty
    if(parsed_resp$RecordsReturned==0) {
      message(paste0("Records returned equal to 0. Most likely your",
                     "query terms are too specific or there is a typo",
                     "that does not trigger a 404 or 500 error"))
    }

    # Now let's address the num_results argument. If that was everything then great
    if(!allResults){
      parsed_resp <- data.table::rbindlist(parsed_resp$Data)
    } else {

      # if the first call has caught all the results then great
      if(parsed_resp$RecordsReturned == parsed_resp$RecordCount){
        parsed_resp <- data.table::rbindlist(parsed_resp$Data)
      } else {

        # if not then query with either max possible or their requested amount
        query$perPage <- 5000

        # Create new request and parse this
        url <- httr::modify_url(endpoint,query = query)
        resp <- httr::GET(url,httr::accept_json(),encode = "json")
        parsed_resp <- dhs_client_response(resp,TRUE)

        # if this larger page query has returned all the results then return this else we will loop through
        if(parsed_resp$TotalPages == 1){
          parsed_resp <- data.table::rbindlist(parsed_resp$Data)
        } else {

          # save the resp as a temp and then make parsed_resp the list we will loop requests into
          temp_parsed_resp <- parsed_resp
          parsed_resp <- list(); length(parsed_resp) <- temp_parsed_resp$TotalPages
          parsed_resp[[1]] <- temp_parsed_resp
          for(i in 2:length(parsed_resp)){
            query$page <- i
            url <- httr::modify_url(endpoint,query = query)
            resp <- httr::GET(url,httr::accept_json(),encode = "json")
            temp_parsed_resp <- dhs_client_response(resp,TRUE)
            parsed_resp[[i]] <- temp_parsed_resp
          }

          # and now concatenate the results
          parsed_resp <- data.table::rbindlist(do.call(c,lapply(parsed_resp,function(x) x$Data)))
        }
      }

    }

    return(parsed_resp)
}