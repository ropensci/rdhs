#' #' Set DHS login credentials
#' #'
#' #' @param credentials File path to where log in credentials are written.
#' #'   File format should be (each bullet is a new line):
#' #'   \itemize{
#' #'       \item email=dummy@gmail.com
#' #'       \item password=dummypass
#' #'       \item project=Dummy Project
#' #'       }
#' #' @param root Character for root directory to where client,
#' #'   caches, surveys etc. will be stored.
#' #' Default = \code{rappdirs::user_cache_dir("rdhs", Sys.info()["user"])}
#' #' @return Invisibly returns the rdhs package environment client
#' #' @export
#' set_dhs_credentials <- function(credentials, root=NULL) {
#'
#'   # first deal with the credentials
#'   set_rdhs_CREDENTIALS_PATH(path = credentials)
#'
#'   # set the root if provided
#'   if (is.null(root)) root <- .rdhs$default_root
#'
#'   # if no permission was granted then we will need to change this root
#'   # to one in the tempdir before setting it
#'   if (Sys.getenv("rdhs_RENVIRON_PERMISSION") != 1) {
#'     root <- tempdir()
#'     message(
#'       "You have not granted permision to rdhs to write outside of ",
#'       "your temporary directory. As a result any provided root ",
#'       "argument has been set to tempdir(). To use your provided ",
#'       "root please allow rdhs permission to write to outside your ",
#'       "temporary directory."
#'     )
#'     set_rdhs_ROOT_PATH(root, ask = FALSE)
#'   } else {
#'
#'     set_rdhs_ROOT_PATH(root)
#'
#'   }
#'
#'   # and then create the client here for them using these variabes to be sure
#'   .rdhs$client <- client_dhs(
#'     credentials = Sys.getenv(renv_cred_path_name()),
#'     root = Sys.getenv(renv_root_path_name())
#'   )
#'
#'   invisible(.rdhs$client)
#' }
#' #' @noRd
#' read_credentials <- function(filename) {
#'   if (is.list(filename)) {
#'     check_credentials(filename)
#'   } else {
#'     dat <- strsplit(readLines(filename, warn = FALSE), "=")
#'     dat <- dat[which(lapply(dat,length) %>% lapply(function(x) x>0) %>% unlist)]
#'     dat <- setNames(
#'       as.list(trimws(vapply(dat, "[[", character(1), 2L))),
#'       trimws(vapply(dat, "[[", character(1), 1L))
#'     )
#'     dat <- lapply(dat, function(x) gsub("\"", "", x))
#'     check_credentials(dat)
#'   }
#' }
#'
#' #' @noRd
#' check_credentials <- function(credentials) {
#'   if (is.null(names(credentials))) {
#'     stop("Credentials must be named")
#'   }
#'   extra <- setdiff(names(credentials), c("email", "password", "project"))
#'   if (length(extra) > 0L) {
#'     stop("Unknown fields in credentials: ", paste(extra, collapse = ", "))
#'   }
#'   credentials # consider credentials[req]
#' }
#'
#' #' @noRd
#' set_environment_credentials <- function(credentials) {
#'   Sys.setenv("rdhs_USER_EMAIL" = credentials$email)
#'   Sys.setenv("rdhs_USER_PASS" = credentials$password)
#'   Sys.setenv("rdhs_USER_PROJECT" = credentials$project)
#' }
#'
#' #' @noRd
#' credentials_not_present <- function() {
#'   return(identical(Sys.getenv("rdhs_USER_PASS"), "") ||
#'            identical(Sys.getenv("rdhs_USER_EMAIL"), "") ||
#'            identical(Sys.getenv("rdhs_USER_PROJECT"), ""))
#' }
#'
#' #' @noRd
#' handle_credentials <- function(credentials) {
#'   if (!is.null(credentials)) {
#'     have_cred_path <- file.exists(credentials)
#'     if (!have_cred_path) {
#'       if (identical(Sys.getenv("rdhs_USER_PASS"), "") ||
#'           identical(Sys.getenv("rdhs_USER_EMAIL"), "") ||
#'           identical(Sys.getenv("rdhs_USER_PROJECT"), "")) {
#'         stop(
#'           "Credentials are not present in your system environment.",
#'           "Please provide credentials argument"
#'         )
#'       }
#'     } else {
#'       set_environment_credentials(read_credentials(credentials))
#'     }
#'   } else {
#'     if (identical(Sys.getenv("rdhs_USER_PASS"), "") ||
#'         identical(Sys.getenv("rdhs_USER_EMAIL"), "") ||
#'         identical(Sys.getenv("rdhs_USER_PROJECT"), "")) {
#'       stop(
#'         "Credentials are not present in your system environment.",
#'         "Please provide credentials argument"
#'       )
#'     }
#'   }
#' }
#'
#' # set .Renviron variable
#' #' @noRd
#' set_renviron <- function(variable, value, ask = TRUE) {
#'
#'   # first do some checking:
#'   if (substr(variable, 1, 4) != "rdhs") {
#'     stop("renviron variable to be set does not begin \"rdhs\"")
#'   }
#'
#'   # now remove any trailing " that may be in there
#'   value <- gsub("\"", "", value)
#'   variable <- gsub("\"", "", variable)
#'
#'   # and set these within our current session
#'   args <- list(value)
#'   names(args) <- variable
#'   do.call(Sys.setenv, args)
#'
#'   # ask user if okay to write
#'   if (Sys.getenv("rdhs_RENVIRON_PERMISSION") != 1 && ask) {
#'     ask_user_permission()
#'   }
#'
#'   if (Sys.getenv("rdhs_RENVIRON_PERMISSION") == 1) {
#'
#'     # next grab the current .Renviron if it exists
#'     if (file.exists(find_renviron())) {
#'       current <- readLines(file.path(find_renviron()), warn = FALSE)
#'
#'       # check to see if the variable already exists
#'       current_vars <- strsplit(current, "=") %>%
#'         lapply(function(x) x[1]) %>% unlist()
#'       presets <- grepl(variable, current_vars)
#'
#'       # remove any previous rdhs variables for the variable of interest
#'       current <- current[!presets]
#'
#'       # add our new value always placing it in quotes
#'       new <- c(current, paste0(variable, " = ", "\"", value, "\""))
#'     } else {
#'       new <- paste0(variable, " = ", "\"", value, "\"")
#'     }
#'
#'     writeLines(new, file.path(find_renviron()))
#'
#'   }
#'
#' }
#'
#' # set credentials env var
#' #' @noRd
#' set_rdhs_CREDENTIALS_PATH <- function(path, ask = TRUE) {
#'
#'   # normalise credentials here for ease with no warnings
#'   # as we'll check it ourselves
#'   credentials <- normalizePath(path, winslash = "/", mustWork = FALSE)
#'   if (!file.exists(credentials)) {
#'     stop("credentials file does not exist. Please check:\n   -> ", credentials)
#'   }
#'
#'   # and let's check their validity before going any further
#'   read_credentials(credentials)
#'
#'   # if these are fine let's set these to the .Renviron
#'   set_renviron(variable = renv_cred_path_name(),
#'                value = credentials,
#'                ask = ask)
#'
#'   invisible(Sys.getenv(renv_cred_path_name()))
#' }
#'
#' # set root env var
#' #' @noRd
#' set_rdhs_ROOT_PATH <- function(path, ask = TRUE) {
#'
#'   # normalise the root path and create the directory
#'   root <- normalizePath(path, winslash = "/", mustWork = FALSE)
#'   dir.create(root, recursive = TRUE, showWarnings = FALSE)
#'
#'   # and let's then set these too to the .Renviron
#'   set_renviron(renv_root_path_name(), root, ask)
#'
#'   invisible(Sys.getenv(renv_root_path_name()))
#' }
#'


# context("Credentials")
#
# test_that("credentials", {
#
#   # first get the credentials
#   skip_if_no_auth()
#
#   # save them so this test doesn't nuke the others
#   old_envs <- save_current_envs()
#
#   # remove these credentials
#   Sys.setenv("rdhs_USER_EMAIL" = "")
#   Sys.setenv("rdhs_USER_PASS" = "")
#   Sys.setenv("rdhs_USER_PROJECT" = "")
#
#   # check for no credentials provided with nothing in the envrionment catch
#   expect_error(handle_credentials(credentials = NULL))
#
#   # check for no good credential path
#   expect_error(handle_credentials(credentials = "rubbish"))
#
#   # now lets make a credentials object
#   write("email=dummy@gmail.com\npassword=\"dummy\"\nproject=Dummy space",
#         file = "rubbish_no_more.txt"
#   )
#   handle_credentials(credentials = "rubbish_no_more.txt")
#
#   # check they are what they should be
#   expect_identical(Sys.getenv("rdhs_USER_EMAIL"), "dummy@gmail.com")
#   expect_identical(Sys.getenv("rdhs_USER_PASS"), "dummy")
#   expect_identical(Sys.getenv("rdhs_USER_PROJECT"), "Dummy space")
#
#   # reset our credentials
#   restore_current_envs(old_envs)
#
#   # check for unnamed list attempt
#   expect_error(read_credentials(list("humpty", "dumpty", "project")))
#
#   # check for too many args
#   expect_error(read_credentials(list(
#     "email" = "humpty",
#     "password" = "dumpty",
#     "project" = "project",
#     "huh" = 2
#   )))
#
#   # remove this
#   unlink("rubbish_no_more.txt")
# })
#
#
#
# test_that("set_dhs_credentials", {
#
#   Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)
#
#   # first let's grab the default client objct so we can rewrite it
#   old_client <- .rdhs$client
#   old_cred <- Sys.getenv(renv_cred_path_name())
#   old_root <- Sys.getenv(renv_root_path_name())
#
#   # save them so this test doesn't nuke the others
#   old_envs <- save_current_envs()
#   if (file.exists(file.path(normalizePath("~"), ".Renviron"))) {
#     old_renviron <- readLines(file.path(normalizePath("~"), ".Renviron"))
#   }
#
#   # lets make a credentials object
#   write("email=dummy@gmail.com\npassword=\"dummy\"\nproject=Dummy space",
#         file = "rubbish_no_more.txt"
#   )
#
#   set_renviron(variable = "rdhs_CREDENTIALS_PATH",
#                value = "rubbish_no_more.txt")
#
#   out <- set_dhs_credentials(credentials = "rubbish_no_more.txt")
#
#   # check that it returns the client invisibly
#   expect_identical(class(out), c("client_dhs", "R6"))
#
#   # check it's set the system variables
#   expect_identical(Sys.getenv("rdhs_USER_EMAIL"), "dummy@gmail.com")
#   expect_identical(Sys.getenv("rdhs_USER_PASS"), "dummy")
#   expect_identical(Sys.getenv("rdhs_USER_PROJECT"), "Dummy space")
#
#   expect_identical(
#     .rdhs$client$.__enclos_env__$private$credentials_path,
#     normalizePath("rubbish_no_more.txt", winslash = "/")
#   )
#
#   # now let's try it with a new root
#   out <- set_dhs_credentials(
#     credentials = "rubbish_no_more.txt",
#     root = file.path(getwd(), "dummy")
#   )
#
#   # the env client root should now be this new dummy
#   expect_identical(
#     .rdhs$client$get_root() %>% basename(),
#     "dummy"
#   )
#   expect_null(.rdhs$client$.__enclos_env__$private$user_declared_root)
#
#   # and the client at the default root should still be there
#   dcrc <- readRDS(file.path(.rdhs$default_root, client_file_name()))
#   expect_identical(
#     dcrc$get_root() %>% basename(),
#     rappdirs::user_cache_dir("rdhs", Sys.info()["user"]) %>% basename()
#   )
#
#   ## and if it's working we should get the same now after triggering an .onLoad
#
#   # so first let's clear the package environment to be doubly sure
#   rdhs_reset()
#   expect_null(.rdhs$test)
#
#   Sys.setenv("rdhs_STARTUP_LOUD" = TRUE)
#   expect_message(rdhs::.onAttach())
#   Sys.setenv("rdhs_STARTUP_LOUD" = FALSE)
#
#   # the env client root should now be this new dummy
#   expect_identical(
#     .rdhs$client$get_root() %>% basename(),
#     "dummy"
#   )
#   expect_null(.rdhs$client$.__enclos_env__$private$user_declared_root)
#
#   # and we should have these options saved on the .Renviron
#   environ <- readLines(file.path(normalizePath("~"), ".Renviron"))
#   expect_true(any(grepl(rdhs::renv_cred_path_name(), environ)))
#   expect_true(any(grepl(rdhs::renv_root_path_name(), environ)))
#
#   # remove this
#   unlink("rubbish_no_more.txt")
#   unlink("dummy", recursive = TRUE)
#
#   # reset our credentials and remove the rubbish client
#   restore_current_envs(old_envs)
#   .rdhs$client <- NULL
#
#   if (exists("old_renviron")) {
#     write(x = old_renviron, file.path(normalizePath("~"), ".Renviron"))
#   } else {
#     write(x = "", file.path(normalizePath("~"), ".Renviron"))
#   }
#
#   # and put the old client back in place and reset
#   # the renvirons if they existed before hand
#   if (!is.null(old_client)) {
#     saveRDS(old_client, file.path(old_client$get_root(), client_file_name()))
#     .rdhs$client <- old_client
#
#     expect_identical(
#       rdhs::set_rdhs_CREDENTIALS_PATH(
#         old_client$.__enclos_env__$private$credentials_path
#       ),
#       old_client$.__enclos_env__$private$credentials_path
#     )
#     expect_identical(
#       rdhs::set_rdhs_ROOT_PATH(old_client$get_root()),
#       normalizePath(old_client$get_root(), winslash = "/", mustWork = FALSE)
#     )
#   }
# })
#
#
# test_that("read_credentials with spaces", {
#
#   # lets make a credentials object with end lines
#   write("email=dummy@gmail.com\npassword=\"dummy\"\nproject=Dummy space\n\n\n",
#         file = "rubbish_no_more.txt"
#   )
#
#   expect_identical(
#     read_credentials(filename = "rubbish_no_more.txt"),
#     list("email"="dummy@gmail.com",
#          "password"="dummy",
#          "project"="Dummy space")
#   )
#
#   unlink("rubbish_no_more.txt")
#
# })


#' # renv variable name for the credentials path
#' #' @noRd
#' renv_cred_path_name <- function() "RDHS_CREDENTIALS_PATH"
#'
#' # renv variable name for the root path
#' #' @noRd
#' renv_root_path_name <- function() "RDHS_ROOT_PATH"
#'
