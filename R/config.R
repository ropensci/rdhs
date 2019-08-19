#' @noRd
find_rdhs_config <- function() {

  file_local <- "rdhs.json"
  file_global <- "~/.rdhs.json"
  file_rapp <- file.path(rappdirs_rdhs(), "rdhs.json")
  file_temp <- file.path(tempdir_check(), "rdhs/rdhs.json")

  if (file.exists(file_local)) {
    return(file_local)
  } else if (file.exists(file_global)) {
    return(file_global)
  } else if (file.exists(file_rapp)) {
    return(file_rapp)
  } else if (file.exists(file_temp)) {
    return(file_temp)
  } else {
    NULL
  }
}

#' @noRd
config_not_present <- function(config) {

  return(is.null(config$email) ||
           is.null(config$project) ||
           is.null(config$password))
}

#' @noRd
handle_config <- function(config_path) {
  if (!is.null(config_path)) {
    have_config_path <- file.exists(config_path)
    if (!have_config_path) {
      stop(
        "Login credentials for the DHS website have not been found.",
        "Please uset set_dhs_config() and provide your DHS email and project."
      )
    } else {
      return(invisible(read_rdhs_config_file(config_path)))
    }
  } else {
    stop(
      "Login credentials for the DHS website have not been found.",
      "Please uset set_dhs_config() and provide your DHS email and project."
    )
  }
}



#' Set rdhs config
#'
#' Sets the configuration settings for using rdhs.
#'
#' @param email Character for email used to login to the DHS website.
#' @param project Character for the name of the DHS project from which
#'   datasets should be downloaded.
#' @param cache_path Character for directory path where datasets and API calls
#'   will be cached. If left bank, a suitable directory will be created within
#'   your user cache directory for your operating system (permission granting).
#' @param config_path Character for where the config file should be saved.
#'   For a global configuration, `config_path` must be '~/.rdhs.json'.
#'   For a local configuration, `config_path` must be 'rdhs.json'.
#'   If left bank, the config file will be stored within
#'   your user cache directory for your operating system (permission granting).
#' @param global Logical for the config_path to be interpreted as a global
#'   config path or a local one. Default = TRUE.
#' @param verbose_setup Logical for rdhs setup and messages to be printed.
#'   Default = TRUE.
#' @param verbose_download Logical for dataset download progress bars to be
#'   shown. Default = FALSE.
#' @param data_frame Function with which to convert API calls into. If left
#'   blank \code{data_frame} objects are returned. Must be passed as a
#'   character. Examples could be:
#'   \code{data.table::as.data.table}
#'   \code{tibble::as.tibble}
#' @param timeout Numeric for how long in seconds to wait for the DHS API to
#'   respond. Default = 30.
#' @param password_prompt Logical whether user is asked to type their password,
#'   even if they have previously set it. Default = FALSE. Set to TRUE if you
#'   have mistyped your password when using \code{set_rdhs_config}.
#' @param prompt Logical for whether the user should be prompted for
#'   permission to write to files. This should not need be
#'   changed by the user. Default = TRUE.
#'
#' @rdname set_rdhs_config
#'
#' @details Setting up a configuration will enable API results to be cached, as
#'   well as enabling datasets from the DHS website to be downloaded and also
#'   cached. To enable results to be cached you have to either provide a valid
#'   `cache_path` argument, or allow rdhs to write to the user cache directory
#'   for your operating system. To do the later, leave the `cache_path` argument
#'   blank and you will be explicitly prompted to give permission to `rdhs` to
#'   save your results in this directory. If you do not then your API calls and
#'   any downloaded datasets will be saved in the temp directory and deleted
#'   after your R session closes. To allow `rdhs` to download datasets from the
#'   DHS website, you have to provide both an `email` and `project` argument.
#'   You will then be prompted to type in your login password securely.
#'   Your provided config (email, project, password, cache_path etc) will be
#'   saved at the location provided by `config_path`. If no argument is provided
#'   `config_path` will be either set to within your user cache directory if you
#'   have given permission to do so, otherwise it will be placed within your
#'   temp directory.
#'
#'   When creating your config you also have the option to specify whether the
#'   `config_path` provided should be used as a local configuration or a global
#'   one. This is controlled using the `global` argument, which by default is
#'   set equal to `TRUE`. A global config is saved within your R root directory
#'   (the directory that a new R session will start in). If you set `global` to
#'   `FALSE` the config file will be saved within the current directory. This
#'   can be useful if you create a new DHS project for each new piece of work,
#'   and want to keep the datasets you download for this project separate to
#'   another. If you want to have your config file saved in a different
#'   directory, then you must create a file "rdhs.json" first in that directory
#'   before specifying the full path to it, as well as setting `global` equal to
#'   `FALSE`.
#'
#'   As an aside, it is useful for the DHS program to see how the surveys they
#'   conducted are being used, and thus it is helpful for them if you do create
#'   a new project for each new piece of work (e.g. a different publication).
#'   However, we would still recommend setting up a global config and using
#'   the same `cache_path` for different projects as this will save you time
#'   downloading the same datasets as you have downloaded before.
#'
#'   Lastly, you can decide how API calls from the DHS API are formatted by
#'   providing an argument for `data_frame`. If left blank API calls will be
#'   returned as `data.frame` objects, however, you could return API calls as
#'   `data.table` objects using `data.table::as.data.table`.
#' @return Invisibly returns the rdhs config object
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # normal set up we would prvide the email and project, and be prompted for
#' # the password. (not run as it requires a prompt)
#' set_rdhs_config(email = "blah@gmail.com", project = "Blahs",
#' config_path = "rdhs.json", global = FALSE)
#'
#'
#' # otherwise we can do this by specifying prompt to FALSE
#' set_rdhs_config(
#' config_path = "rdhs.json", global = FALSE, prompt = FALSE
#' )
#'
#' # you can look at what you have set these to using \code{get_rdhs_config}
#' config <- get_rdhs_config()
#' }
#'
#'
set_rdhs_config <- function(email = NULL,
                            project = NULL,
                            cache_path = NULL,
                            config_path = NULL,
                            global = TRUE,
                            verbose_download = FALSE,
                            verbose_setup = TRUE,
                            data_frame = NULL,
                            timeout = 30,
                            password_prompt = FALSE,
                            prompt = TRUE) {

  assert_null_and_func(email, assert_scalar_character)
  assert_null_and_func(project, assert_scalar_character)
  assert_null_and_func(config_path, assert_scalar_character)
  assert_null_and_func(cache_path, assert_scalar_character)
  assert_null_and_func(data_frame, assert_scalar_character)
  assert_scalar_logical(global)
  assert_scalar_logical(verbose_download)
  assert_scalar_logical(verbose_setup)
  assert_scalar_logical(prompt)
  assert_scalar_numeric(timeout)


  # This is in a hope to be compliant with the CRAN policy about not
  # writing to disk.  We need permission to write to a place that we
  # can reliably load the configuration.  I think that if we did not
  # use the 'config_path' arg here and just wrote to the expected location
  # that would fall foul of the policy.  This way the user provides
  # the location and is therefore not different to something like
  # 'pdf', which also writes to disk.  It's not clear that there is
  # _any_ really sensible way through that particular policy...
  # If they provide nothing we then ask if they are okay with us
  # using rappdirs to store these, and if not then it goes temp.
  if (!is.null(config_path)) {
    if (global) {
      if (config_path != "~/.rdhs.json") {
        stop("For a global configuration, 'config_path' must be '~/.rdhs.json'")
      }
    } else {
      if (config_path != "rdhs.json" &&
          (!file.exists(config_path) || basename(config_path) == "rdhs/json")) {
        stop("For a local configuration, 'config_path' must be 'rdhs.json' ",
             "or 'config_path' must already exist and end 'rdhs.json'")
      }
    }
  } else {
    if (!(options("rappdir_permission") == TRUE) && prompt){
      ask_user_permission()
      asked <- TRUE
    }
    if (!(options("rappdir_permission") == TRUE)){
      config_path <- file.path(tempdir_check(), "rdhs/rdhs.json")
      dir.create(file.path(tempdir_check(), "rdhs"),
                 showWarnings = FALSE, recursive = TRUE)
    } else {
      config_path <- file.path(rappdirs_rdhs(), "rdhs.json")
      dir.create(rappdirs_rdhs(), showWarnings = FALSE, recursive = TRUE)
    }
  }

  # if the cache_path for non global is relative then convert
  if (global && !is.null(cache_path) && !is_absolute_path(cache_path)) {
    cache_path <- normalizePath(cache_path, mustWork = TRUE, winslash = "/")
  }

  # if we have no cache_path then set this to temp
  if (is.null(cache_path)){
    if (!(options("rappdir_permission") == TRUE) && prompt && !asked){
      ask_user_permission()
    }
    if (!(options("rappdir_permission") == TRUE)){
      cache_path <- file.path(tempdir_check(), "rdhs")
      dir.create(cache_path, showWarnings = FALSE, recursive = TRUE)
    } else {
      cache_path <- rappdirs_rdhs()
      dir.create(cache_path, showWarnings = FALSE, recursive = TRUE)
    }
  }

  if (prompt) {
    password <- Sys.getenv("RDHS_USER_PASS", NA_character_)
    if (is.na(password) || password_prompt) {
      password <- trimws(getPass::getPass("DHS Password: ", TRUE, TRUE))
      Sys.setenv("RDHS_USER_PASS" = password)
    }
  } else {
    password <- NULL
  }

  dat <- list(email = email,
              project = project,
              password = password,
              cache_path = cache_path,
              config_path = config_path,
              global = global,
              verbose_download = verbose_download,
              verbose_setup = verbose_setup,
              timeout = timeout,
              data_frame = data_frame,
              project_choice = NULL)

  config <- write_rdhs_config_file(dat, config_path)

  # and then create the package internal client if we are meant to
  if (.rdhs$internal_client_update) {
    .rdhs$client <- client_dhs(config = config, root = config$cache_path)
  }

  invisible(config)
}


#' @noRd
write_rdhs_config_file <- function(dat, path) {

  str <- jsonlite::toJSON(dat, auto_unbox = TRUE, pretty = TRUE, null = "null")

  rdhs_setup_message(
    verbose = dat$verbose_setup,
    "Writing your configuration to:\n   -> ", path, "\n"
  )

  # and add this to gitignore if it is a non global config file
  if (!dat$global) {
    add_line(".gitignore", path)
  }

  writeLines(str, path)
  invisible(read_rdhs_config_file(path))

}

#' @noRd
write_rdhs_config_from_client_config <- function(client) {

  config <- client$get_config()
  config$data_frame <- config$data_frame_nice
  config$data_frame_nice <- NULL
  str <- jsonlite::toJSON(unclass(config), auto_unbox = TRUE,
                          pretty = TRUE, null = "null")
  rdhs_setup_message(
    verbose = config$verbose_setup,
    "Writing your configuration to:\n   -> ", config$config_path, "\n"
  )
  rdhs_setup_message(
    verbose = config$verbose_setup,
    "If you are using git, be sure to add this to your .gitignore\n"
  )
  writeLines(str, config$config_path)
  invisible(read_rdhs_config_file(config$config_path))

}



#' @noRd
read_rdhs_config_file <- function(config_path) {
  fsize <- file.size(config_path)
  dat <- jsonlite::fromJSON(readChar(config_path, fsize), FALSE)
  ## Expected fields:
  expected <- c("email",
                "project",
                "password",
                "cache_path",
                "config_path",
                "global",
                "verbose_download",
                "verbose_setup",
                "timeout",
                "project_choice")
  msg <- setdiff(expected, names(dat))
  if (length(msg) > 0L) {
    stop("Missing fields from configuration: ",
         paste(squote(msg), collapse = ", "))
  }

  if (is.null(dat$data_frame)) {
    dat$data_frame <- identity
    dat$data_frame_nice <- "as.data.frame"
  } else {
    dat$data_frame_nice <- dat$data_frame
    if (grepl("::", dat$data_frame)) {
      ns <- sub("::.*$", "", dat$data_frame)
      nm <- sub("^.*::", "", dat$data_frame)
      dat$data_frame <- getExportedValue(ns, nm)
    } else {
      dat$data_frame <- match.fun(dat$data_frame)
    }
  }

  class(dat) <- "rdhs_config"
  ## TODO: we should do validation here again but it's boring so that
  ## can be done later.  The other thing that's not handled here is
  ## the "choice" functionality
  dat
}

#' Update your current rdhs config
#'
#' @inheritParams set_rdhs_config
#' @param password Logical for updating your password securely. Default = FALSE
#' @param project_choice Numeric for project choice. See \code{authenticate_dhs}
#'   for more info.
#' @description
#' \code{update_rdhs_config} allows you to update elements of your
#' rdhs config, without having to set it completely via \code{set_rdhs_config}.
#' For each config element, provide the new changes required. To update your
#' password, set \code{password = TRUE} and you will be asked securely for your
#' new password.
#'
#' @export
#'
update_rdhs_config <- function(password = FALSE,
                               email = NULL,
                               project = NULL,
                               cache_path = NULL,
                               config_path = NULL,
                               global = NULL,
                               verbose_download = NULL,
                               verbose_setup = NULL,
                               timeout = NULL,
                               data_frame = NULL,
                               project_choice = NULL) {

  # get the arguments
  args <- as.list(environment())

  assert_null_and_func(email, assert_scalar_character)
  assert_null_and_func(project, assert_scalar_character)
  assert_null_and_func(config_path, assert_scalar_character)
  assert_null_and_func(cache_path, assert_scalar_character)
  assert_null_and_func(data_frame, assert_scalar_character)
  assert_null_and_func(timeout, assert_scalar_numeric)
  assert_null_and_func(global, assert_scalar_logical)
  assert_null_and_func(verbose_download, assert_scalar_logical)
  assert_null_and_func(verbose_setup, assert_scalar_logical)
  assert_null_and_func(project_choice, assert_scalar_numeric)
  assert_scalar_logical(password)

  # get current config
  config <- get_rdhs_config()
  if (is.null(config)) {
    stop("No config found. Please create one first using set_rdhs_config()")
  }

  # get password if required
  if (args$password) {
    args$password <- trimws(getPass::getPass("DHS Password: ", TRUE, TRUE))
    Sys.setenv("RDHS_USER_PASS" = args$password)
  } else {
    args$password <- NULL
  }

  # loop through config args
  for (m in seq_len(length(args))){
    if (!is.null(args[[m]])) {
      config[[names(args[m])]] <- args[[m]]
    }
  }

  # check that the config_path is not invalid
  if (config$config_path != "rdhs.json" &&
      (!file.exists(config$config_path) ||
       basename(config$config_path) == "rdhs/json")) {
    stop("For a local configuration, 'config_path' must be 'rdhs.json' ",
         "or 'config_path' must already exist and end 'rdhs.json'")
  }

  # make cache directory if needed
  if (!file.exists(config$cache_path)) {
    dir.create(config$cache_path, recursive = TRUE, showWarnings = FALSE)
  }

  # handle the function for data.frame and the class issue
  class(config) <- NULL
  if (is.null(data_frame)) {
  config$data_frame <- config$data_frame_nice
  }
  config$data_frame_nice <- NULL

  # now write this config out
  config <- write_rdhs_config_file(config, config$config_path)

  # and then create the package internal client if we are meant to
  if (.rdhs$internal_client_update) {
    .rdhs$client <- client_dhs(config = config, root = config$cache_path)
  }

}


#' @noRd
print_rdhs_config <- function(config, give.attr = FALSE) {

  ga <- give.attr
  config$data_frame <- config$data_frame_nice
  config$data_frame_nice <- NULL

  # if there is a password in the config then let's not print it
  if (!is.null(config$password)) {
  config$password <- paste0(rep("*", nchar(config$password)),collapse="")
  }
  message(paste0(capture.output(str(config, give.attr = ga)), collapse = "\n"))
  message("\n")

}
