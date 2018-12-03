skip_if_no_auth <- function(config_path = "rdhs.json") {

  testthat::skip_on_cran()

  have_cred_path <- file.exists(config_path)
  if (!have_cred_path) {
    skip("No authentication available")
  }

  skip_if_slow_API()

}

skip_if_slow_API <- function() {

  if (suppressMessages(last_api_update()) == 0.5) {
    skip("API is down")
  }

}


api_timeout_safe_test <- function(expr, cli) {

  # one that has to paginate
  d <- tryCatch(eval(substitute(expr)), error = function(e) NULL )

  if (is.null(d)) {
    testthat::skip(paste0("Skipping test as API is too ",
                          "slow to return these objects."))
  }

  return(d)

}


new_rand_client <- function() {

  # Create new directory
  td <- file.path(tempdir(), as.integer(Sys.time()))

  # create auth using rdhs.json

  # to enable the test suite to work without the encoded rdhs.json we'll create
  if (!file.exists("rdhs.json")) {
    options("rappdir_permission" = TRUE)
    set_rdhs_config(config_path = "rdhs.json", global = FALSE, prompt = FALSE)
  }
    cli <- rdhs::client_dhs(
      api_key = api_key_internal,
      config = read_rdhs_config_file("rdhs.json"),
      root = td
    )

  return(cli)
}
