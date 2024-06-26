

test_that("Spatial Boundaries Download", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  # for some reason travis keeps failing to open the download URLs:
  # https://travis-ci.org/ropensci/rdhs/jobs/637246125#L4141
  testthat::skip_on_travis()

  cli <- new_rand_client()

  # check gc file

  # using the surveyNum
  res <- download_boundaries(surveyNum = 471, countryId = "AF")
  expect_true(res$sdr_subnational_boundaries$ISO[1] == "AF")
  expect_is(res$sdr_subnational_boundaries, "sf")

  # using the surveyId and no countryID
  res <- download_boundaries(surveyId = "AF2010OTH")
  expect_true(length(res) == 2)
  expect_true(res$sdr_subnational_boundaries2$DHSREGEN[2] == "Northern")

  # using sf
  res <- download_boundaries(surveyNum = 471, countryId = "AF", method = "sf")
  expect_true(res$sdr_subnational_boundaries$ISO[1] == "AF")
  expect_is(res$sdr_subnational_boundaries, "sf")

  # using rgdal as example of unsupported method
  expect_message(
    res <- download_boundaries(surveyNum = 471,
                               countryId = "AF",
                               method = "rgdal")
  )
  expect_is(res, "character")
  expect_true(any(grepl("\\.shp$", res)))

})






test_that("Timout Spatial Boundaries Test", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  testthat::skip_on_travis()

  cli <- new_rand_client()

  # check time differences are greater - not best test but simple
  t1 <- Sys.time()
  res <- download_boundaries(surveyNum = 471, countryId = "AF", server_sleep = 1)
  t2 <- Sys.time()

  t3 <- Sys.time()
  res <- download_boundaries(surveyNum = 471, countryId = "AF", server_sleep = 4)
  t4 <- Sys.time()

  expect_true(t2-t1 < t4-t3)

})


test_that("caching works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  cli <- new_rand_client()
  dat <- api_timeout_safe_test(
    download_boundaries(surveyNum = 471, countryId = "AF", client = cli), cli
  )
  expect_identical(cli$.__enclos_env__$private$storr$list("spatial_boundaries"), "471_sf")

  # check that no message is sent second time
  expect_no_message(dat <- api_timeout_safe_test(
    download_boundaries(surveyNum = 471, countryId = "AF", client = cli), cli
  ))
})
