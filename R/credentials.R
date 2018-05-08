
##' Set DHS login credentials
##'
##' @param credentials File path to where log in credentials are written.
##' File format should be (each bullet is a new line):
##' \itemize{
##'       \item email=dummy@gmail.com
##'       \item password=dummypass
##'       \item project=Dummy Project
##'       }
##' @param root Character for root directory to where client,
##'   caches, surveys etc. will be stored.
##' Default = \code{rappdirs::user_cache_dir("rdhs", Sys.info()["user"])}
##' @return Invisibly returns the rdhs package environment client
##' @export
set_dhs_credentials <- function(credentials, root=NULL) {

  # first deal with the credentials
  set_rdhs_CREDENTIALS_PATH(path = credentials)

  # set the root if provided
  if (is.null(root)) root <- .rdhs$default_root

  # if no permission was granted then we will need to change this root
  # to one in the tempdir before setting it
  if (Sys.getenv("rdhs_RENVIRON_PERMISSION") != 1) {
    root <- tempdir()
    message(
      "You have not granted permision to rdhs to write outside of ",
      "your temporary directory. As a result any provided root ",
      "argument has been set to tempdir(). To use your provided ",
      "root please allow rdhs permission to write to outside your ",
      "temporary directory."
    )
    set_rdhs_ROOT_PATH(root, ask = FALSE)
  } else {

    set_rdhs_ROOT_PATH(root)

  }

  # and then create the client here for them using these variabes to be sure
  .rdhs$client <- client_dhs(
    credentials = Sys.getenv(renv_cred_path_name()),
    root = Sys.getenv(renv_root_path_name())
  )

  invisible(.rdhs$client)
}


## Format is
# email=dummy@gmail.com
# password=dummypass
# project=Dummy Project
#' @noRd
read_credentials <- function(filename) {
  if (is.list(filename)) {
    check_credentials(filename)
  } else {
    dat <- strsplit(readLines(filename, warn = FALSE), "=")
    dat <- setNames(
      as.list(trimws(vapply(dat, "[[", character(1), 2L))),
      trimws(vapply(dat, "[[", character(1), 1L))
    )
    dat <- lapply(dat, function(x) gsub("\"", "", x))
    check_credentials(dat)
  }
}

#' @noRd
check_credentials <- function(credentials) {
  if (is.null(names(credentials))) {
    stop("Credentials must be named")
  }
  extra <- setdiff(names(credentials), c("email", "password", "project"))
  if (length(extra) > 0L) {
    stop("Unknown fields in credentials: ", paste(extra, collapse = ", "))
  }
  credentials # consider credentials[req]
}

#' @noRd
set_environment_credentials <- function(credentials) {
  Sys.setenv("rdhs_USER_EMAIL" = credentials$email)
  Sys.setenv("rdhs_USER_PASS" = credentials$password)
  Sys.setenv("rdhs_USER_PROJECT" = credentials$project)
}

#' @noRd
credentials_not_present <- function() {
  return(identical(Sys.getenv("rdhs_USER_PASS"), "") ||
           identical(Sys.getenv("rdhs_USER_EMAIL"), "") ||
           identical(Sys.getenv("rdhs_USER_PROJECT"), ""))
}


handle_credentials <- function(credentials) {
  if (!is.null(credentials)) {
    have_cred_path <- file.exists(credentials)
    if (!have_cred_path) {
      if (identical(Sys.getenv("rdhs_USER_PASS"), "") ||
          identical(Sys.getenv("rdhs_USER_EMAIL"), "") ||
          identical(Sys.getenv("rdhs_USER_PROJECT"), "")) {
        stop(
          "Credentials are not present in your system environment.",
          "Please provide credentials argument"
        )
      }
    } else {
      set_environment_credentials(read_credentials(credentials))
    }
  } else {
    if (identical(Sys.getenv("rdhs_USER_PASS"), "") ||
        identical(Sys.getenv("rdhs_USER_EMAIL"), "") ||
        identical(Sys.getenv("rdhs_USER_PROJECT"), "")) {
      stop(
        "Credentials are not present in your system environment.",
        "Please provide credentials argument"
      )
    }
  }
}
