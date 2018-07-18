context("Authentication")

test_that("authenticate_dhs works", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  config <- read_rdhs_config_file("rdhs.json")

  expect_equal(rdhs::authenticate_dhs(
    your_email = config$email,
    your_password = config$password,
    your_project = config$project
  )$proj_id, "111616")

  # catch if your project has a short name that won't be ellipsis cocnerned
  expect_equal(rdhs:::authenticate_dhs(
    your_email = config$email,
    your_password = config$password,
    your_project = paste0(
      strsplit(config$project, "")[[1]][1:10],
      collapse = ""
    )
  )$proj_id, "111616")

  expect_error(rdhs::authenticate_dhs(
    your_email = config$email,
    your_password = config$password,
    your_project = "twaddle_for_days"
  ))
})

test_that("available_surveys works", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  cli <- new_rand_client()

  # create availbale datasets
  survs <- cli$available_datasets()

  # do it without the client check for poor formed internal api_request
  config <- cli$get_config()
  survs <- available_datasets(
    your_email = config$email,
    your_password = config$password,
    your_project = config$project
  )

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
