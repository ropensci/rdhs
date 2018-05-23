# Package Load set up

.rdhs <- new.env(parent = emptyenv())

# startup message suprression. Will keep messages in as good for debugging
#' @noRd
rdhs_startup_message <- function(...) {
  if (Sys.getenv("rdhs_STARTUP_LOUD") == TRUE) {
    packageStartupMessage(...)
  }
}

#' @noRd
rdhs_reset <- function() {
  rm(list = ls(.rdhs, all.names = TRUE), envir = .rdhs)
}

#' @noRd
rdhs_setup <- function() {

  # check for existing credentials path
  cred_path <- Sys.getenv(renv_cred_path_name())
  root_path <- Sys.getenv(renv_root_path_name())

  # check for a client here
  if (root_path == "" || cred_path == "") {
    rdhs_startup_message(
      "\nFor help with rdhs and to report any issues ",
      "please head to the github repo:\n   -> ",
      "https://github.com/OJWatson/rdhs\n"
    )
  } else {

    # check the credentials file we have for them is still valid
    credentials <- normalizePath(cred_path, winslash = "/", mustWork = FALSE)

    if (!file.exists(credentials)) {
      rdhs_startup_message(
        "Your login credentials provided last time are no longer there!\n",
        "Please recreate your credentials file at ",
        "the following path and reload rdhs:\n   -> ",
        credentials, "\n",
        "Alternatively use set_dhs_credentials() to ",
        "specify a new credentials file."
      )
      return(invisible(credentials))
    }

    # and check if it is still valid
    out <- tryCatch(
      expr = read_credentials(credentials),
      error = function(e) {
        rdhs_startup_message(
          "Your login credentials provided last time are not valid!\n",
          "Please check your credentials file at ",
          "the following path and reload rdhs:\n   -> ",
          credentials, "\n",
          "Alternatively use set_dhs_credentials() to ",
          "specify a new credentials file."
        )
      }
    )

    # and return now if that errored
    if (is.null(out)) return(invisible(credentials))


    # if we have got to this point then hooray and let's
    # get their client and set it in the package environment
    .rdhs$client <- client_dhs(
      credentials = Sys.getenv(renv_cred_path_name()),
      root = Sys.getenv(renv_root_path_name())
    )

    rdhs_startup_message(
      "\nWelcome back :) \n",
      "-------------------------\n",
      "rdhs will be using the login credentials you set last time, ",
      "which it will read from:\n   -> ",
      .rdhs$client$.__enclos_env__$private$credentials_path, "\n",
      "It will also save any datasets you download in this directory:\n   -> ",
      .rdhs$client$get_root(), "\n",
      "If you wish to change your credentials or where your datasets ",
      "are saved, please use set_dhs_credentials()"
    )

    # and now let's hande the credentials from this client
    return(invisible(.rdhs$client))
  }

}

#' @noRd
.onAttach <- function(...) {

  # just in case clear the pacakge environment
  rdhs_reset()

  # check
  .rdhs$test <- 1

  # set up the default root
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs", Sys.info()["user"])

  # return the client invisibly
  return(invisible(rdhs_setup()))

}

# set .Renviron variable
#' @noRd
set_renviron <- function(variable, value, ask = TRUE) {

  # first do some checking:
  if (substr(variable, 1, 4) != "rdhs") {
    stop("renviron variable to be set does not begin \"rdhs\"")
  }

  # now remove any trailing " that may be in there
  value <- gsub("\"", "", value)
  variable <- gsub("\"", "", variable)

  # and set these within our current session
  args <- list(value)
  names(args) <- variable
  do.call(Sys.setenv, args)

  # ask user if okay to write
  if (Sys.getenv("rdhs_RENVIRON_PERMISSION") != 1 && ask) {
    ask_user_permission()
  }

  if (Sys.getenv("rdhs_RENVIRON_PERMISSION") == 1) {

    # next grab the current .Renviron if it exists
    if (file.exists(find_renviron())) {
      current <- readLines(file.path(find_renviron()), warn = FALSE)

      # check to see if the variable already exists
      current_vars <- strsplit(current, "=") %>%
        lapply(function(x) x[1]) %>% unlist()
      presets <- grepl(variable, current_vars)

      # remove any previous rdhs variables for the variable of interest
      current <- current[!presets]

      # add our new value always placing it in quotes
      new <- c(current, paste0(variable, " = ", "\"", value, "\""))
    } else {
      new <- paste0(variable, " = ", "\"", value, "\"")
    }

    writeLines(new, file.path(find_renviron()))

  }

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
        "rdhs would like to write to files outisde ",
        "of your R temporary directory. This is ",
        "so that your datasets and API calls are cached between ",
        "R sessions. Do you confirm rdhs to ",
        "write to files outside your R temporary directry? (Enter 1 or 2)\n",
        "1: Yes",
        "2: No\n",
        "Your choice will be remembered between R sessions.",
        sep = "\n"
      )) %>% as.integer()

    if (is.element(pl, c(1, 2))) {
      int_check <- FALSE
    }

  }

  if (pl == 1) {
    Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)
    set_renviron("rdhs_RENVIRON_PERMISSION", 1)
  } else {
    Sys.setenv("rdhs_RENVIRON_PERMISSION" = 0)

    message("You have not given rdhs permission to write to files outside ",
            "your temporary directory.. You will still be able to use rdhs, ",
            "but your API results and any downloaded datasets will not be",
            "saved after you close this R session")
  }

}

# set credentials env var
#' @noRd
set_rdhs_CREDENTIALS_PATH <- function(path, ask = TRUE) {

  # normalise credentials here for ease with no warnings
  # as we'll check it ourselves
  credentials <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(credentials)) {
    stop("credentials file does not exist. Please check:\n   -> ", credentials)
  }

  # and let's check their validity before going any further
  read_credentials(credentials)

  # if these are fine let's set these to the .Renviron
  set_renviron(variable = renv_cred_path_name(),
               value = credentials,
               ask = ask)

  invisible(Sys.getenv(renv_cred_path_name()))
}

# set root env var
#' @noRd
set_rdhs_ROOT_PATH <- function(path, ask = TRUE) {

  # normalise the root path and create the directory
  root <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  # and let's then set these too to the .Renviron
  set_renviron(renv_root_path_name(), root, ask)

  invisible(Sys.getenv(renv_root_path_name()))
}

# check client for validity
#' @noRd
check_client <- function(client) {

  # if it's null then return early
  if (!inherits(client, "client_dhs")) {
    return(FALSE)
  }

  cred_path <- client$.__enclos_env__$private$credentials_path
  root_path <- client$get_root()

  # check for a client here
  if (root_path == "" || cred_path == "") {
    return(FALSE)
  }

  # check the credentials file we have for them is still valid
  credentials <- normalizePath(cred_path, winslash = "/", mustWork = FALSE)

  # and check if it is still valid
  out <- tryCatch(expr = read_credentials(credentials), error = function(e) {
    NULL
  })

  # and return now if that errored
  if (is.null(out)) return(FALSE)

  ## if we have got this far then cool
  return(TRUE)
}
