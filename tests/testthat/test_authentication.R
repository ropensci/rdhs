context("Authentication")

test_that("authenticate_dhs works", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  expect_equal(rdhs:::authenticate_dhs(
    your_email = Sys.getenv("rdhs_USER_EMAIL"),
    your_password = Sys.getenv("rdhs_USER_PASS"),
    your_project = Sys.getenv("rdhs_USER_PROJECT")
  )$proj_id, "111616")

  # catch if your project has a short name that won't be ellipsis cocnerned
  expect_equal(rdhs:::authenticate_dhs(
    your_email = Sys.getenv("rdhs_USER_EMAIL"),
    your_password = Sys.getenv("rdhs_USER_PASS"),
    your_project = paste0(
      strsplit(Sys.getenv("rdhs_USER_PROJECT"), "")[[1]][1:10],
      collapse = ""
    )
  )$proj_id, "111616")

  expect_error(rdhs:::authenticate_dhs(
    your_email = Sys.getenv("rdhs_USER_EMAIL"),
    your_password = Sys.getenv("rdhs_USER_PASS"),
    your_project = "twaddle_for_days"
  ))
})

test_that("available_surveys works", {
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

  # create availbale datasets
  survs <- cli$available_datasets()

  # do it without the client check for poor formed internal api_request
  survs <- available_datasets(
    your_email = Sys.getenv("rdhs_USER_EMAIL"),
    your_password = Sys.getenv("rdhs_USER_PASS"),
    your_project = Sys.getenv("rdhs_USER_PROJECT")
  )

  # check the names
  expect_identical(names(survs), c(
    "FileFormat", "FileSize", "DatasetType", "SurveyNum", "SurveyId",
    "FileType", "FileDateLastModified", "SurveyYearLabel", "SurveyType",
    "SurveyYear", "DHS_CountryCode", "FileName", "CountryName", "URLS"
  ))

  # there should definitely be more than this many urls
  expect_true(dim(survs)[1] > 5000)

  unlink(td)
})
