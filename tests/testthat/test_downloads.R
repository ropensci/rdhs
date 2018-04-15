context("Downloads")

test_that("avaialble surveys and download work", {

  skip_if_no_auth()
  testthat::skip_on_cran()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale surveys
  survs <- cli$available_datasets()

  # check zip only
  downloads <- sapply(1:2,function(x) cli$get_datasets(dataset_filenames = survs$FileName[x],download_option = "z"))

  # check for annoying nested datasets
  sen <- cli$get_datasets("SNGE71FL.ZIP",download_option = "z")

  ## for teh unpacking make sure to pick dta or spss
  hhs_dta <- which(survs$FileFormat %in% c("Stata dataset (.dta)","SPSS dataset (.sav)") & survs$FileType %in% c("Household Member Recode"))

  # check rds only for 1
  sample_survs <- sample(length(hhs_dta),1,replace=FALSE)

  # check rds only
  downloads <- cli$get_datasets(dataset_filenames = survs[hhs_dta[sample_survs],]$FileName,download_option = "r")

  # check handle for dataset you don't have permission for
  downloads <- cli$get_datasets("CDGC61FL.ZIP")

  # check dta foreign only
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r",mode="raw")

  # remove the cache and then check for the reformat
  cli$.__enclos_env__$private$storr$del(key="AO2011MIS_AOBR62DT_rds_TRUE",namespace = "downloaded_datasets")
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r",mode="raw",reformat=TRUE)

  # remove the cache and then check for the toupper no reformat
  cli$.__enclos_env__$private$storr$del(key="AO2011MIS_AOBR62DT_rds_TRUE",namespace = "downloaded_datasets")
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r",mode="raw",all_lower=FALSE)

  # remove the cache and then check for the reformat to upper
  cli$.__enclos_env__$private$storr$del(key="AO2011MIS_AOBR62DT_rds_TRUE",namespace = "downloaded_datasets")
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r",mode="raw",reformat=TRUE,all_lower=TRUE)

  # check FL
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62FL.ZIP",download_option = "r",all_lower = FALSE)
  cli$.__enclos_env__$private$storr$del(key="AO2011MIS_AOBR62FL_rds_TRUE",namespace = "downloaded_datasets")

  # remove the cache and then check for the map parser
  cli$.__enclos_env__$private$storr$del(key="AO2011MIS_AOBR62DT_rds_TRUE",namespace = "downloaded_datasets")
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r",mode="map",reformat=TRUE)


  # check FL with allLowr default
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62FL.ZIP",download_option = "r")

  # check the irritating shared filename case
  downloads <- cli$get_datasets("KEKR42FL.ZIP")
  datasets <- dhs_datasets()
  downloads <- cli$get_datasets(datasets[6047,])

  # check both
  downloads <- cli$get_datasets(dataset_filenames = survs[hhs_dta[sample_survs],]$FileName,download_option = "b")

  # check downloaded surveys
  d <- cli$get_downloaded_datasets()

  # check var label fetch
  v <- cli$get_var_labels(dataset_filenames = "AOBR62FL.ZIP")
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62FL.ZIP",download_option = "r")
  v <- cli$get_var_labels(dataset = readRDS(downloads$AOBR62FL))
  v <- cli$get_var_labels(dataset_filenames = "AOBR62FL.ZIP",dataset = readRDS(downloads$AOBR62FL))

  # and check the non client version on normal and fommatted
  v <- get_var_labels(readRDS(downloads$AOBR62FL))
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r",reformat=TRUE)
  v <- get_var_labels(readRDS(downloads$AOBR62DT))

  unlink(td)

})


test_that("Hierarchal and sas7bdat dataset test",{

  testthat::skip_on_cran()
  cli <- new_rand_client()


  # check hierarchal catch and sas7bdat
  downloads <- cli$get_datasets(dataset_filenames = "AOIR62.ZIP",download_option = "r",mode="raw")
  expect_identical(downloads[[1]],"No support for importing hierarchal .dat")

  downloads <- cli$get_datasets(dataset_filenames = "AOHR51sd.zip",download_option = "r",mode="raw")
  expect_identical(downloads[[1]],"No support for importing .sas7bdat")

  expect_error(cli$.__enclos_env__$private$storr$get("AO2011MIS_AOHR51sd_rds_FALSE","downloaded_datsets"))

})

test_that("Geo dataset test", {

  testthat::skip_on_cran()
  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale surveys
  survs <- cli$available_datasets()

  # grab just the geographic data
  hhs_geo <- which(survs$FileType %in% c("Geographic Data"))

  # check rds only
  downloads <- cli$get_datasets(dataset_filenames = survs[sample(length(hhs_geo),2,replace=FALSE),]$FileName,download_option = "r")

  unlink(td)


})
