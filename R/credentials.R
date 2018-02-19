## Format is
# email=dummy@gmail.com
# password=dummypass
# project=Dummy Project
read_credentials <- function(filename) {
  if(is.list(filename)){
    check_credentials(filename)
  } else {
  dat <- strsplit(readLines(filename), "=")
  dat <- setNames(as.list(trimws(vapply(dat, "[[", character(1), 2L))),
                  trimws(vapply(dat, "[[", character(1), 1L)))
  check_credentials(dat)
  }
}

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

set_environment_credentials <- function(credentials){
  Sys.setenv("rdhs_USER_EMAIL"=credentials$email)
  Sys.setenv("rdhs_USER_PASS"=credentials$password)
  Sys.setenv("rdhs_USER_PROJECT"=credentials$project)
}
