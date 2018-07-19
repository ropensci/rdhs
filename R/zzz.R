# Package Load set up

.rdhs <- new.env(parent = emptyenv())

#' @noRd
rdhs_reset <- function() {
  rm(list = ls(.rdhs, all.names = TRUE), envir = .rdhs)
  options("rappdir_permission" = FALSE)
  .rdhs$internal_client_update <- TRUE
}

# setup message suprression. Will keep messages in as good for debugging
#' @noRd
rdhs_setup_message <- function(verbose = TRUE, ...) {
  if (verbose) {
    message(...)
  }
}




#' @noRd
rdhs_setup <- function() {

  # check for existing config path
  config_file <- find_rdhs_config()
  if (is.null(config_file)) {
    config <- set_rdhs_config(prompt = FALSE)
  } else {
    config <- read_rdhs_config_file(config_file)
    .rdhs$client <- client_dhs(config = config, root = config$cache_path)
  }


  return(invisible(.rdhs$client))

}

#' @noRd
.onLoad <- function(...) {

  # just in case clear the package environment
  rdhs_reset()

}

# ask for user permission to write to Renviron
#' @noRd
ask_user_permission <- function(){

  # while loop until they provide valid response
  int_check <- TRUE

  # loop ask for permission
  while (int_check) {
    pl <- readline(
      prompt = cat(
        "rdhs would like to write to files outisde of your R temporary",
        "directory. This is so that your datasets and API calls are cached ",
        "between R sessions. Do you confirm rdhs to write to files outside",
        "your R temporary directry? (Enter 1 or 2)\n",
        "1: Yes",
        "2: No\n",
        sep = "\n"
      )) %>% as.integer()

    if (is.element(pl, c(1, 2))) {
      int_check <- FALSE
    }

  }

  if (pl == 1) {
    options("rappdir_permission" = TRUE)
  } else {
    options("rappdir_permission" = FALSE)
    message("You have not given rdhs permission to write to files outside ",
            "your temporary directory. You will still be able to use rdhs, ",
            "but your API results and any downloaded datasets will not be",
            "saved after you close this R session")
  }

}
