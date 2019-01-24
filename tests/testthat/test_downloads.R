context("Downloads")

test_that("available surveys and download work", {
  skip_if_no_auth()
  testthat::skip_on_cran()

  # Create new directory
  cli <- new_rand_client()

  # create availbale surveys
  survs <- cli$available_datasets()

  # check zip only
  downloads <- cli$get_datasets(
    dataset_filenames = survs$FileName[1],
    download_option = "z"
  )

  # check for annoying nested datasets
  sen <- cli$get_datasets("SNGE71FL.ZIP", download_option = "z")

  ## for teh unpacking make sure to pick dta or spss
  # check rds only
  downloads <- cli$get_datasets(
    dataset_filenames = "AOPR51dt.zip",
    download_option = "r"
  )

  # check handle for dataset you don't have permission for
  downloads <- cli$get_datasets("CDGC61FL.ZIP")

  # check dta foreign only
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62DT.ZIP",
    download_option = "r", mode = "raw"
  )
  d <- readRDS(downloads$AOBR62DT)
  v <- get_variable_labels(d)

  # remove the cache and then check for the reformat
  cli$.__enclos_env__$private$storr$del(
    key = "AO2011MIS_AOBR62DT.ZIP_rds_TRUE", namespace = "downloaded_datasets"
  )
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62DT.ZIP", download_option = "r",
    mode = "raw", reformat = TRUE
  )

  # remove the cache and then check for the toupper no reformat
  cli$.__enclos_env__$private$storr$del(
    key = "AO2011MIS_AOBR62DT.ZIP_rds_TRUE", namespace = "downloaded_datasets"
  )
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62DT.ZIP", download_option = "r",
    mode = "raw", all_lower = FALSE
  )

  # remove the cache and then check for the reformat to upper
  cli$.__enclos_env__$private$storr$del(
    key = "AO2011MIS_AOBR62DT.ZIP_rds_TRUE", namespace = "downloaded_datasets"
  )
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62DT.ZIP", download_option = "r",
    mode = "raw", reformat = TRUE, all_lower = TRUE
  )

  # check FL
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62FL.ZIP", download_option = "r",
    all_lower = FALSE
  )
  cli$.__enclos_env__$private$storr$del(
    key = "AO2011MIS_AOBR62FL.ZIP_rds_TRUE", namespace = "downloaded_datasets"
  )

  # remove the cache and then check for the map parser
  cli$.__enclos_env__$private$storr$del(
    key = "AO2011MIS_AOBR62DT.ZIP_rds_TRUE", namespace = "downloaded_datasets"
  )
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62DT.ZIP", download_option = "r",
    mode = "map", reformat = TRUE
  )

  # check FL with allLowr default
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62FL.ZIP", download_option = "r"
  )

  # check the irritating shared filename case
  downloads <- cli$get_datasets("KEKR42FL.ZIP")
  datasets <- dhs_datasets()
  india_match <- which(datasets$FileName=="KEKR42FL.ZIP" &
                         datasets$CountryName == "India")
  downloads <- cli$get_datasets(datasets[india_match, ])

  # check both
  downloads <- cli$get_datasets(
    dataset_filenames = "AOPR51sv.zip",
    download_option = "b"
  )

  # check downloaded surveys
  d <- cli$get_downloaded_datasets()

  # check var label fetch
  v <- cli$get_variable_labels(dataset_filenames = "AOBR62FL.ZIP")
  downloads <- cli$get_datasets(
    dataset_filenames = c("AOBR62FL.ZIP", "AOBR62DT.ZIP"),
    download_option = "r"
  )

  v <- cli$get_variable_labels(dataset_paths = unlist(downloads))
  v <- cli$get_variable_labels(dataset_filenames = c(
    "AOBR62FL.ZIP",
    "AOBR62DT.ZIP"
  ))

  # and check the non client version on normal and fommatted
  v <- get_variable_labels(readRDS(downloads$AOBR62FL))
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62DT.ZIP", download_option = "r", reformat = TRUE
  )
  v <- get_variable_labels(readRDS(downloads$AOBR62DT))

  unlink(cli$get_root())
})

test_that("ETAR71FL.ZIP test", {
  skip("DHS has removed this file (possibly because of what it tested")
  skip_if_no_auth()
  testthat::skip_on_cran()
  cli <- new_rand_client()

  dat <- cli$get_datasets("ETAR71FL.ZIP")
  r <- readRDS(dat[[1]])
  expect_identical(class(r), c("data.frame", "dhs_dataset"))
})

test_that("ugir41fl.zip test", {
  skip_if_no_auth()
  testthat::skip_on_cran()
  cli <- new_rand_client()

  dat <- cli$get_datasets("ugir41fl.zip")
  r <- readRDS(dat[[1]])
  expect_equal(r %>% dim(), c(7246, 3862))
})

test_that("zip file ending test", {
  skip_if_no_auth()
  testthat::skip_on_cran()
  cli <- new_rand_client()

  dat <- cli$get_datasets("ETAR61FL.ZIP", download_option = "zip")
  extension <- strsplit(dat$ETAR61FL, ".", fixed = TRUE) %>%
    lapply(function(x) x[length(x)]) %>%
    unlist()
  expect_identical(toupper(extension), "ZIP")
})

test_that("Hierarchal and sas7bdat dataset test", {
  skip_if_no_auth()
  testthat::skip_on_cran()
  cli <- new_rand_client()


  # check hierarchal catch and sas7bdat
  downloads <- cli$get_datasets(
    dataset_filenames = "AOIR62.ZIP", download_option = "r", mode = "raw"
  )
  expect_identical(downloads[[1]], "No support for importing hierarchal .dat")

  downloads <- cli$get_datasets(
    dataset_filenames = "AOHR51sd.zip", download_option = "r", mode = "raw"
  )
  expect_identical(downloads[[1]], "No support for importing .sas7bdat")

  expect_error(cli$.__enclos_env__$private$storr$get(
    "AO2011MIS_AOHR51sd_rds_FALSE", "downloaded_datsets"
  ))
})

test_that("Geo dataset test", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  cli <- new_rand_client()

  # create availbale surveys
  survs <- cli$available_datasets()

  # grab just the geographic data
  hhs_geo <- which(survs$FileType %in% c("Geographic Data"))

  # check rds only
  downloads <- cli$get_datasets("AOGE52FL.zip",
    download_option = "r"
  )

  unlink(cli$get_root())
})

test_that("Geospatial coviarates", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  cli <- new_rand_client()

  # check gc file
  downloads <- cli$get_datasets("CDGC62FL.ZIP")
  d <- readRDS(downloads$CDGC62FL)

  expect_true(d$DHSCLUST[1] == 1)
  unlink(cli$get_root())

})

