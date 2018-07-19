context("Authentication")

test_that("authenticate_dhs works", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  config <- read_rdhs_config_file("rdhs.json")

  expect_equal(rdhs:::authenticate_dhs(config)$proj_id, "111616")

  # catch if your project has a short name that won't be ellipsis cocnerned
  proj <- config$project
  config$project <- paste0(strsplit(config$project, "")[[1]][1:10],
                           collapse = ""
                           )
  expect_equal(rdhs:::authenticate_dhs(config)$proj_id, "111616")

  config$project <- "twaddle_for_days"
  expect_error(rdhs:::authenticate_dhs(config))
})

test_that("available_surveys works", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  cli <- new_rand_client()

  # create availbale datasets
  survs <- cli$available_datasets()

  # do it without the client check for poor formed internal api_request
  config <- cli$get_config()
  survs <- available_datasets(config)

  # check the names
  expect_identical(names(survs), c(
    "FileFormat", "FileSize", "DatasetType", "SurveyNum", "SurveyId",
    "FileType", "FileDateLastModified", "SurveyYearLabel", "SurveyType",
    "SurveyYear", "DHS_CountryCode", "FileName", "CountryName", "URLS"
  ))

  # there should definitely be more than this many urls
  expect_true(dim(survs)[1] > 5000)

  unlink(cli$get_root())
})
