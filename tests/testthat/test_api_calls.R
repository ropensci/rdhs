context("API calls")

test_that("can request api through dhs_api_request via client", {
  skip_if_no_auth()
  testthat::skip_on_cran()

  # Create new directory
  td <- file.path(tempdir(), as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  cli <- new_rand_client()

  # make call
  req <- cli$dhs_api_request(
    api_endpoint = "surveys", query = list("indicatorIds" = "ML_AMLD_C_QNN")
  )

  # remake call to test cache
  req <- cli$dhs_api_request(
    api_endpoint = "surveys", query = list("indicatorIds" = "ML_AMLD_C_QNN")
  )

  # test more than id
  req <- cli$dhs_api_request(
    api_endpoint = "surveys",
    query = list("indicatorIds" = c("ML_AMLD_C_QNN", "FE_FRTR_W_A20"))
  )

  # test the error catch for wrong endpoint
  expect_error(
    cli$dhs_api_request(
      api_endpoint = "blargh",
      query = list("breakdowAndCry" = "subartyping")
    )
  )

  unlink(td)
})
