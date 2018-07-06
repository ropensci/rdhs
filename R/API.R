#' @noRd
query_creation <- function(query) {

  # Collapse query list where lengths are greater than 1
  j <- lengths(query) > 1
  query[j] <- lapply(query[j], paste0, collapse = ",")

  # add the api key
  query$apiKey <- "ICLSPH-527168"

  return(query)
}

#' @noRd
handle_api_request <- function(endpoint, query, all_results, client,
                               force=FALSE) {

  # create query and set format to json of not specified
  query <- query_creation(query)
  if (is.null(query$f)) {
    query$f <- "json"
  }

  # if no client was provided we'll look for
  # the package environment client by default
  if (is.null(client)) {
    client <- if (!check_client(.rdhs$client)) NULL else .rdhs$client
  }

  # if there is no client then make request
  if (is.null(client)) {

    # create generic request
    resp <- api_request(endpoint, query, all_results)

  } else {

    # create url for api request
    url <- httr::modify_url(endpoint, query = query)

    # create a client cache key for this
    key <- digest::digest(
      paste0(url, "all_results=", all_results, collapse = "")
    )

    out <- tryCatch(client$.__enclos_env__$private$storr$get(key, "api_calls"),
                    KeyError = function(e) NULL
    )

    # check out agianst cache, if fine return that and if not make request
    if (!is.null(out) && !force) {
      resp <- out
    } else {

      # create generic request
      resp <- api_request(endpoint, query, all_results)

      ## then cache the resp and return the parsed resp
      client$.__enclos_env__$private$storr$set(key, resp, "api_calls")
    }
  }

  # for those (jeff) who want data.table with no package overhead for rdhs
  if (Sys.getenv("rdhs_DATA_TABLE") == TRUE) {
    resp <- eval(parse(text = "resp %>% data.table::as.data.table()"))
  }

  return(resp)
}

#' @noRd
api_request <- function(endpoint, query, all_results) {

  # if they have specified other than json
  if (!is.element(query$f, c("json", "geojson"))) {

    # make url for request
    url <- httr::modify_url(endpoint, query = query)

    # Make the request
    resp <- httr::GET(url, httr::user_agent("https://github.com/OJWatson/rdhs"))

    # if they have all_results still as TRUE then let them now
    # that not all results will be returned
    if (all_results) {
      message(paste0(
        "Format specified is not equal to 'json'. ",
        "all_results will be ignored and only the ",
        "first response from the API will be returned. ",
        "Set 'f=\"json\"' to return all API results."
      ))
    }

    ## pass to response parse and then return
    parsed_resp <- handle_api_response(resp, FALSE)

    return(parsed_resp)
  }

  # if not then let's make paginated requests
  if (query$f == "json") {
    parsed_resp <- handle_pagination_json(endpoint, query, all_results)
  } else if (query$f == "geojson") {
    parsed_resp <- handle_pagination_geojson(endpoint, query, all_results)
  }

  return(parsed_resp)
}

#' @noRd
handle_pagination_json <- function(endpoint, query, all_results) {


  # make url for request
  url <- httr::modify_url(endpoint, query = query)

  # make the request
  resp <- httr::GET(url, httr::accept_json(),
                    httr::user_agent("https://github.com/OJWatson/rdhs"),
                    encode = "json"
  )

  ## pass to response parse and if its json then grab the data
  parsed_resp <- handle_api_response(resp, TRUE)
  if (resp$status_code >= 400 && resp$status_code < 600) {
    return(parsed_resp)
  }

  # put some messages to let the user know if the data returned is empty
  if (parsed_resp$RecordsReturned == 0) {
    stop(
      paste0(
        "Records returned equal to 0. Most likely your ",
        "query terms are too specific or there is a typo ",
        "that does not trigger a 404 or 500 error"
      )
    )
  }

  # Now address the num_results argument. If that was everything then great
  if (!all_results) {
    parsed_resp <- rbind_list_base(parsed_resp$Data)
  } else {

    # if the first call has caught all the results then great
    if (parsed_resp$RecordsReturned == parsed_resp$RecordCount) {
      parsed_resp <- rbind_list_base(parsed_resp$Data)
    } else {

      # if not then query with either max possible or their requested amount
      query$perPage <- 5000

      # Create new request and parse this
      url <- httr::modify_url(endpoint, query = query)
      resp <- httr::GET(
        url, httr::user_agent("https://github.com/OJWatson/rdhs"),
        httr::accept_json(),
        encode = "json"
      )
      parsed_resp <- handle_api_response(resp, TRUE)

      # if this larger page query has returned all the results
      # then return this else we will loop through
      if (parsed_resp$TotalPages == 1) {
        parsed_resp <- rbind_list_base(parsed_resp$Data)
      } else {

        # save the resp as a temp and then make parsed_resp the list
        # we will loop requests into
        temp_parsed_resp <- parsed_resp
        parsed_resp <- list()
        length(parsed_resp) <- temp_parsed_resp$TotalPages
        parsed_resp[[1]] <- temp_parsed_resp
        for (i in 2:length(parsed_resp)) {
          query$page <- i
          url <- httr::modify_url(endpoint, query = query)
          resp <- httr::GET(
            url, httr::user_agent("https://github.com/OJWatson/rdhs"),
            httr::accept_json(),
            encode = "json"
          )
          temp_parsed_resp <- handle_api_response(resp, TRUE)
          parsed_resp[[i]] <- temp_parsed_resp
        }

        # and now concatenate the results
        parsed_resp <- collapse_api_responses(parsed_resp)
      }
    }
  }

  return(parsed_resp)

}

#' @noRd
handle_pagination_geojson <- function(endpoint, query, all_results) {

  # make url for request
  url <- httr::modify_url(endpoint, query = query)

  # make the request
  resp <- httr::GET(url, httr::accept_json(),
                    httr::user_agent("https://github.com/OJWatson/rdhs"),
                    encode = "json"
  )

  ## pass to response parse and if its json then grab the data
  parsed_resp <- handle_api_response(resp, TRUE)
  if (resp$status_code >= 400 && resp$status_code < 600) {
    return(parsed_resp)
  }

  # put some messages to let the user know if the data returned is empty
  rr <- parsed_resp$features[[1]]$properties$RecordsReturned
  if (rr == 0) {
    stop(
      paste0(
        "Records returned equal to 0. Most likely your ",
        "query terms are too specific or there is a typo ",
        "that does not trigger a 404 or 500 error"
      )
    )
  }

  # Now address the num_results argument. If that was everything then no need
  if (all_results) {

    # if the first call has not caught all the results then paginate
    if (rr != parsed_resp$features[[1]]$properties$RecordCount) {

      # if not then query with either max possible or their requested amount
      query$perPage <- 5000

      # Create new request and parse this
      url <- httr::modify_url(endpoint, query = query)
      resp <- httr::GET(
        url, httr::user_agent("https://github.com/OJWatson/rdhs"),
        httr::accept_json(),
        encode = "json"
      )
      parsed_resp <- handle_api_response(resp, TRUE)

      # if this larger page query has not returned all the results
      # then keep going
      tp <- parsed_resp$features[[1]]$properties$TotalPages
      if (tp > 1) {

        # save the resp as a temp and then make parsed_resp the list
        # we will loop requests into
        loop_resp <- list()
        length(loop_resp) <- tp
        loop_resp[[1]] <- parsed_resp$features
        for (i in 2:length(loop_resp)) {
          query$page <- i
          url <- httr::modify_url(endpoint, query = query)
          resp <- httr::GET(
            url, httr::user_agent("https://github.com/OJWatson/rdhs"),
            httr::accept_json(),
            encode = "json"
          )
          temp_parsed_resp <- handle_api_response(resp, TRUE)
          loop_resp[[i]] <- temp_parsed_resp$features
        }

        # and now concatenate the results
        loop_resp <- do.call(c, loop_resp)
        parsed_resp$features <- loop_resp
      }
    }
  }

  return(parsed_resp)

}


## This is something of an ugly hack to convert function arguments
## into a list appropriate for the api queries.  There are common
## arguments (in "drop") to api functions that are not actually query
## parameters
args_to_query <- function(env, drop = c("client", "force", "all_results")) {
  ret <- as.list(env)
  ret[setdiff(names(ret), drop)]
}
