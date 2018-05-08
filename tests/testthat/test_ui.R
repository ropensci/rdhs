context("user interface")

test_that("check_for_client", {

  Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)

  old <- .rdhs$client
  old_envs <- save_current_envs()

  # nuke the package client
  rdhs_reset()
  expect_error(check_for_client())

  # set up some credentials
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs", Sys.info()["user"])
  out <- set_dhs_credentials(
    "credentials",
    rappdirs::user_cache_dir("rdhs", Sys.info()["user"])
    )

  expect_true(check_for_client())

  restore_current_envs(old_envs)
})


test_that("check on the use of the default client for API caching in extract", {
  testthat::skip_on_cran()

  Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)

  # clear the api call cache
  .rdhs$client$clear_namespace("api_calls")

  # so let's make sure there is no rdhs client in the cache
  rdhs_reset()
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs", Sys.info()["user"])

  # and let's create a query
  dat <- dhs_countries(countryIds = "SN", all_results = FALSE)

  ## now recreate the key

  key <- digest::digest(
    paste0(
      "https://api.dhsprogram.com/rest/dhs/countries?countryIds=SN",
      "&f=json&apiKey=ICLSPH-527168",
    "all_results=", FALSE,
    collapse = ""
  ))

  # set up a package client
  create_correct_credentials("credentials.txt")
  on.exit(unlink("credentials.txt"))

  # this should errror now as there was no client before
  expect_error(
    .rdhs$client$.__enclos_env__$private$storr$get(
      key, namespace = "api_calls"
      )
    )

  # but if we do it again
  dat <- dhs_countries(countryIds = "SN", all_results = FALSE)
  expect_equal(
    dat, .rdhs$client$.__enclos_env__$private$storr$get(key,
                                                        namespace = "api_calls")
    )
})


test_that("get_datasets", {
  testthat::skip_on_cran()

  Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)

  rdhs_reset()
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs", Sys.info()["user"])

  # set up a package client
  create_correct_credentials("credentials.txt")
  on.exit(unlink("credentials.txt"))

  dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "zip")
  expect_identical(names(dat), "ZWHR31SV")
})

test_that("get_downloaded_datasets", {
  testthat::skip_on_cran()

  Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)

  rdhs_reset()
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs", Sys.info()["user"])

  # set up a package client
  create_correct_credentials("credentials.txt")
  on.exit(unlink("credentials.txt"))

  dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "zip")
  dat <- get_downloaded_datasets()
  expect_true(any(names(dat) %in% "ZWHR31SV.ZIP"))
})

test_that("get_available_datasets", {
  testthat::skip_on_cran()

  Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)

  # clear the available_datasets_calls call cache
  .rdhs$client$clear_namespace("available_datasets_calls")

  # so let's make sure there is no rdhs client in the cache
  rdhs_reset()
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs", Sys.info()["user"])

  # set up a package client
  create_correct_credentials("credentials.txt")
  on.exit(unlink("credentials.txt"))

  # and let's get this first sans using the internal function
  dat <- available_datasets(
    your_email = Sys.getenv("rdhs_USER_EMAIL"),
    your_password = Sys.getenv("rdhs_USER_PASS"),
    your_project = Sys.getenv("rdhs_USER_PROJECT")
  )

  ## now recreate the key
  key <- digest::digest(paste0(Sys.getenv("rdhs_USER_PROJECT"), ","))

  # this should errror now as there was no client before
  expect_error(
    .rdhs$client$.__enclos_env__$private$storr$get(
      key, namespace = "available_datasets_calls"
      )
    )

  # but if we do it using the ui one again
  dat2 <- get_available_datasets()
  expect_equal(
    dat2,
    .rdhs$client$.__enclos_env__$private$storr$get(
      key,
      namespace = "available_datasets_calls"
    )
  )
})

test_that("search_and_extract_dhs", {
  testthat::skip_on_cran()

  Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)

  rdhs_reset()
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs", Sys.info()["user"])

  # set up a package client
  create_correct_credentials("credentials.txt")
  on.exit(unlink("credentials.txt"))

  dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "rds")
  que <- search_variables(dataset_filenames = "ZWHR31SV.ZIP", "hv024")
  extract <- extract_dhs(que)
  expect_equal(extract$ZWHR31SV$SurveyId[1], "ZW1994DHS")
  que <- search_variable_labels(dataset_filenames = "ZWHR31SV.ZIP", "Cluster")
  extract <- extract_dhs(que)
  expect_equal(extract$ZWHR31SV$hv001[1], 1)
})



test_that("get_var_labels", {
  testthat::skip_on_cran()

  Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)

  rdhs_reset()
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs", Sys.info()["user"])

  # set up a package client
  create_correct_credentials("credentials.txt")
  on.exit(unlink("credentials.txt"))

  # let's then get one dataset and get its variables in 3 ways
  dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "rds")
  r <- readRDS(dat$ZWHR31SV)
  var1 <- get_var_labels(dat$ZWHR31SV)
  var2 <- get_var_labels(r)
  var3 <- get_var_labels("ZWHR31SV.ZIP")

  # 1 and 3 same
  expect_identical(var1, var3)

  # if just using the dataset we make no assumption about having a
  # client or cached info to add on the extra useful meta
  expect_identical(var1[1, 1:2], var2[1, ])

  # and reset to the original credentials file
  create_correct_credentials("credentials")
})

test_that("get_var_labels direct via client", {
  testthat::skip_on_cran()

  Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)

  cli <- new_rand_client()

  dat <- cli$get_datasets(dataset_filenames = "ZWHR31SV.ZIP")
  expect_message(cli$get_var_labels("ZWHR31SV.ZIP", dat$ZWHR31))
  expect_error(cli$get_var_labels())
  expect_message(cli$get_var_labels(dataset_paths = c(dat$ZWHR31SV,
                                                      "twaddle")))
  expect_error(cli$get_var_labels(dataset_paths = c("twaddle")))
  expect_message(cli$get_var_labels(dataset_filenames = c("twaddle",
                                                          "ZWHR31SV.ZIP")))
  expect_error(cli$get_var_labels(dataset_filenames = c("twaddle")))
})
