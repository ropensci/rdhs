#' @noRd
handle_api_response <- function(res, to_json = TRUE) {
  code <- httr::status_code(res)
  if (code >= 400 && code < 600) {
    if (response_is_json(res)) {
      errors <- response_to_json(res)$errors
      text <- paste(errors, collapse = "\n")
    } else {
      errors <- NULL
      text <- trimws(httr::content(res, "text", encoding = "UTF-8"))
    }
    stop ((handle_api_error(code, text, errors)))
  }
  if (to_json) {
    res <- response_to_json(res)
  }
  res
}

#' @noRd
handle_api_error <- function(code, text, errors) {
  if (!nzchar(text)) {
    text <- httr::http_status(code)$message
  }

  parent <- c(sys.calls()[[1]] %>% as.character() %>%
                strsplit("\\(") %>% unlist())[1]

  type <- switch(as.character(code),
    "400" = "dhs_invalid_request",
    "401" = "dhs_unauthorized",
    "403" = "dhs_forbidden",
    "404" = "dhs_invalid_path - Likely incorrect query parameters",
    "429" = "dhs_rate_limit_exceeded",
    "500" = "dhs_internal_server_error",
    "501" = "dhs_not_initialized",
    "503" = "dhs_down",
    "dhs_unknown_error"
  )

  err <- (paste0("DHS API Request Failed [", code, "] Error Type: ", type))
  err
}
