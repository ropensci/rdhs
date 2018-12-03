context("Client Setup")

test_that("save credentials", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  # Create new client
  cli <- new_rand_client()
  cd <- cli$get_cache_date()

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
  expect_true(cd < cli$get_cache_date())
  unlink(cli$get_root())
})

test_that("new updates are recognised", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  # Create new client
  cli <- new_rand_client()

  cli$.__enclos_env__$private$cache_date <- rdhs:::last_api_update() - 1
  saveRDS(cli, file.path(cli$get_root(), rdhs:::client_file_name()))

  # create again
  expect_message(cli <- rdhs::client_dhs(api_key = api_key_internal,
                            root = cli$get_root(),
                            cli$get_config()),
                 "DHS API has been updated since")

  # this dataset has been updated in the past so if we
  # force the cache dat back and then refresh it should trig clearing this
  d <- cli$get_datasets("BUBR70SV.ZIP")
  cli$set_cache_date(1)
  cli$save_client()
  # create again
  expect_message(cli <- client_dhs(root = cli$get_root(),
                                   config = cli$get_config()),
                 "DHS API has been updated since")


  unlink(cli$get_root())
})
