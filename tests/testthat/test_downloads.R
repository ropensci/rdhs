context("Downloads")

test_that("avaialble surveys and download work", {

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

  # check FL with upper foreign only
  downloads <- cli$get_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r",mode="raw")

  # check hierarchal catch and sas7bdat
  downloads <- cli$get_datasets(dataset_filenames = "AOIR62.ZIP",download_option = "r",mode="raw")
  expect_identical(downloads[[1]],"No support for importing hierarchal .dat")

  downloads <- cli$get_datasets(dataset_filenames = "AOHR51sd.zip",download_option = "r",mode="raw")
  expect_identical(downloads[[1]],"No support for importing .sas7bdat")

  # check both
  downloads <- cli$get_datasets(dataset_filenames = survs[hhs_dta[sample_survs],]$FileName,download_option = "b")

  unlink(td)

})


test_that("Geo dataset test", {

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
