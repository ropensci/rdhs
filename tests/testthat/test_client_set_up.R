context("Client Setup")

test_that("save credentials", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(), as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if (file.exists("credentials")) {
    cli <- rdhs::client_dhs(
      api_key = "ICLSPH-527168", credentials = "credentials", root = td
    )
    testthat::expect_identical(
      normalizePath(file.path("credentials"), winslash = "/"),
      cli$.__enclos_env__$private$credentials_path
    )
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168", root = td)
  }

  # check for when/if (how) the client got deleted but not the root directory
  file.remove(file.path(cli$get_root(), client_file_name()))

  # create auth through whichever route is valid for the environment
  if (file.exists("credentials")) {
    cli <- rdhs::client_dhs(
      api_key = "ICLSPH-527168", credentials = "credentials", root = td
    )
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168", root = td)
  }

  # test client extra functions

  # date test
  cli$set_cache_date(Sys.time())

  # save test
  cli$save_client()

  # namespace clear test
  countries <- cli$dhs_api_request(api_endpoint = "countries")
  cli$clear_namespace(namespace = "api_calls")

  # check that you can refresh the client - i.e. set the cache date just
  # before the most recnet update to flush certain api and functioanlity
  cli <- client_refresh(cli)
  cli$.__enclos_env__$private$credentials_path <- NULL
  cli <- client_refresh(cli)

  unlink(td)
})

test_that("new updates are recognised", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(), as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if (file.exists("credentials")) {
    cli <- rdhs::client_dhs(
      api_key = "ICLSPH-527168", credentials = "credentials", root = td
    )
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168", root = td)
  }

  cli$.__enclos_env__$private$cache_date <- rdhs:::last_api_update() - 1
  saveRDS(cli, file.path(cli$get_root(), rdhs:::client_file_name()))

  # create auth through whichever route is valid for the environment
  if (file.exists("credentials")) {
    cli <- rdhs::client_dhs(
      api_key = "ICLSPH-527168", credentials = "credentials", root = td
    )
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168", root = td)
  }

  # this dataset has been updated in the past so if we
  # force the cache dat back and then refresh it should trig clearing this
  d <- cli$get_datasets("BUBR70SV.ZIP")
  cli$set_cache_date(1)
  cli$save_client()
  cli <- client_dhs(root = cli$get_root())

  unlink(td)
})
