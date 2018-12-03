context("user interface")

test_that("check_for_client", {
  skip_if_no_auth()

  config <- read_rdhs_config_file("rdhs.json")
  cli <- new_rand_client()

  # nuke the package client
  rdhs_reset()
  file <- "test"
  while (!is.null(file)) {
  file <- find_rdhs_config()
  unlink(file)
  }

  expect_message(check_for_client(), "You have not granted")

  # nuke the package client
  rdhs_reset()
  file <- "test"
  while (!is.null(file)) {
    file <- find_rdhs_config()
    unlink(file)
  }

  # set up a client
  td <- file.path(tempdir(), as.integer(Sys.time()))
  .rdhs$client <- client_dhs(config = config, root = td)

  expect_true(identical(check_for_client(), .rdhs$client))
  write_rdhs_config_from_client_config(.rdhs$client)
})


test_that("check on the use of the default client for API caching in extract", {
  skip_if_no_auth()

  # clear the api call cache
  .rdhs$client$clear_namespace("api_calls")

  # so let's make sure there is no rdhs client in the cache
  rdhs_reset()

  # and let's create a query
  dat <- dhs_countries(countryIds = "SN", all_results = FALSE)

  ## recreate the key
  key <- digest::digest(
    paste0(
      "https://api.dhsprogram.com/rest/dhs/countries?countryIds=SN",
      "&f=json&apiKey=",api_key_internal,
    "all_results=", FALSE,
    collapse = ""
  ))

  # check that they are the same and thus cached
  expect_equal(
    dat, .rdhs$client$.__enclos_env__$private$storr$get(key,
                                                        namespace = "api_calls")
    )
})


test_that("get_datasets with no temp first", {
  skip_if_no_auth()

  config <- read_rdhs_config_file("rdhs.json")
  cli <- new_rand_client()

  rdhs_reset()
  file <- "test"
  while (!is.null(file)) {
    file <- find_rdhs_config()
    unlink(file)
  }

  # error becasue of no email
  expect_error(dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "zip"))

  rdhs_reset()
  file <- "test"
  while (!is.null(file)) {
    file <- find_rdhs_config()
    unlink(file)
  }
  write_rdhs_config_from_client_config(cli)
  dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "zip")
  expect_identical(names(dat), "ZWHR31SV")

})

test_that("get_model_datasets", {
  skip_if_no_auth()
  rdhs_reset()

  dat <- get_datasets(dataset_filenames = "zzar61.zip",
                      download_option = "zip")
  expect_identical(names(dat), "zzar61")
})


test_that("get_downloaded_datasets", {
  skip_if_no_auth()

  rdhs_reset()

  dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "zip")
  dat <- get_downloaded_datasets()
  expect_true(any(names(dat) %in% "ZWHR31SV.ZIP"))
})

test_that("get_available_datasets", {
  skip_if_no_auth()

  rdhs_reset()

  dat <- get_available_datasets()
  expect_true(any(dat$FileName %in% "ZWHR31SV.ZIP"))

})

test_that("search_and_extract_dhs", {
  skip_if_no_auth()

  rdhs_reset()

  # set up a package client
  dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "rds")
  que <- search_variables(dataset_filenames = "ZWHR31SV.ZIP", "hv024")
  extract <- extract_dhs(que)
  expect_equal(extract$ZWHR31SV$SurveyId[1], "ZW1994DHS")
  que <- search_variable_labels(dataset_filenames = "ZWHR31SV.ZIP", "Cluster")
  extract <- extract_dhs(que)
  expect_equal(extract$ZWHR31SV$hv001[1], 1)
})



test_that("get_variable_labels", {
  skip_if_no_auth()

  rdhs_reset()

  # let's then get one dataset and get its variables in 3 ways
  dat <- get_datasets(dataset_filenames = "ZWHR31SV.ZIP",
                      download_option = "rds")
  r <- readRDS(dat$ZWHR31SV)
  var1 <- get_variable_labels(dat$ZWHR31SV)
  var2 <- get_variable_labels(r)
  var3 <- get_variable_labels("ZWHR31SV.ZIP")

  # 1 and 3 same
  expect_identical(var1, var3)

  # if just using the dataset we make no assumption about having a
  # client or cached info to add on the extra useful meta
  expect_identical(var1[1, 1:2], var2[1, ])

})

test_that("get_variable_labels direct via client", {
  skip_if_no_auth()

  rdhs_reset()

  cli <- new_rand_client()

  dat <- cli$get_datasets(dataset_filenames = "ZWHR31SV.ZIP")
  expect_message(cli$get_variable_labels("ZWHR31SV.ZIP", dat$ZWHR31))
  expect_error(cli$get_variable_labels())
  expect_message(cli$get_variable_labels(dataset_paths = c(dat$ZWHR31SV,
                                                      "twaddle")))
  expect_error(cli$get_variable_labels(dataset_paths = c("twaddle")))
  expect_message(cli$get_variable_labels(dataset_filenames = c("twaddle",
                                                          "ZWHR31SV.ZIP")))
  expect_error(cli$get_variable_labels(dataset_filenames = c("twaddle")))
})
