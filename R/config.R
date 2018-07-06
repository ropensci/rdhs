get_rdhs_config <- function() {
  file_local <- "rdhs.json"
  file_global <- "~/.rdhs.json"
  if (file.exists(file_local)) {
    read_rdhs_config_file(file_local, FALSE)
  } else if (file.exists(file_global)) {
    read_rdhs_config_file(file_global, TRUE)
  } else {
    NULL
  }
}


write_rdhs_config <- function(path, email, project, cache_path,
                              verbose_download = FALSE,
                              verbose_startup = FALSE,
                              timeout = 30,
                              global = TRUE) {
  if (!is.null(path)) {
    assert_scalar_character(path)
  }
  assert_scalar_character(email)
  assert_scalar_character(project)
  assert_scalar_character(cache_path)
  assert_scalar_logical(verbose_download)
  assert_scalar_logical(verbose_startup)
  assert_scalar_logical(global)
  assert_scalar_numeric(timeout)

  ## This is in a hope to be compliant with the CRAN policy about not
  ## writing to disk.  We need permission to write to a place that we
  ## can reliably load the configuration.  I think that if we did not
  ## use the 'path' arg here and just wrote to the expected location
  ## that would fall foul of the policy.  This way the user provides
  ## the location and is therefore not different to something like
  ## 'pdf', which also writes to disk.  It's not clear that there is
  ## _any_ really sensible way through that particular policy...
  if (global) {
    if (path != "~/.rdhs.json") {
      stop("For a global configuration, 'path' must be '~/.rdhs.json'")
    } else {
      stop("For a global configuration, 'path' must be 'rdhs.json'")
    }
  }

  if (global && !is.null(cache_path) && !is_absolute_path(cache_path)) {
    stop(
      "If using a global configuration, cache path must be an absolute path")
  }

  password <- Sys.getenv("RDHS_USER_PASS", NA_character_)
  if (is.na(password)) {
    password <- trimws(getPass::getPass("DHS Password: ", TRUE, TRUE))
  }

  dat <- list(email = email,
              project = project,
              password = password,
              cache_path = cache_path,
              verbose_download = verbose_download,
              verbose_startup = verbose_startup,
              timeout = timeout)

  str <- jsonlite::toJSON(dat, auto_unbox = TRUE, pretty = TRUE)
  message("Writing your configuration to '%s'", path)
  message("If you are using git, be sure to add this to your .gitignore")
  writeLines(str, path)
  read_rdhs_config_file(path, global = global)
}


read_rdhs_config_file <- function(path, global) {
  dat <- jsonlite::fromJSON(readChar(path, file.size(path)), FALSE)
  ## Expected fields:
  expected <- c("email",
                "project",
                "password",
                "cache_path",
                "verbose_download",
                "verbose_startup",
                "timeout")
  msg <- setdiff(expected, names(dat))
  if (length(msg) > 0L) {
    stop("Missing fields from configuration: ",
         paste(squote(msg), collapse = ", "))
  }

  ## TODO: we should do validation here again but it's boring so that
  ## can be done later.  The other thing that's not handled here is
  ## the "choice" functionality

  dat$global <- global
  dat
}
