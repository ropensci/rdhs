context("Downloads")

test_that("avaialble surveys and download work", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale surveys
  survs <- cli$available_datasets()

  # check zip only
  downloads <- sapply(1:2,function(x) cli$download_datasets(dataset_filenames = survs$FileName[x],download_option = "z"))

  ## for teh unpacking make sure to pick dta or spss
  hhs_dta <- which(survs$FileFormat %in% c("Stata dataset (.dta)","SPSS dataset (.sav)") & survs$FileType %in% c("Household Member Recode"))

  # check rds only for 1
  sample_survs <- sample(length(hhs_dta),1,replace=FALSE)

  # check rds only
  downloads <- cli$download_datasets(dataset_filenames = survs[hhs_dta[sample_survs],]$FileName,download_option = "r")

  # check both
  downloads <- cli$download_datasets(dataset_filenames = survs[hhs_dta[sample_survs],]$FileName,download_option = "b")

  unlink(td)

})


test_that("Geo dataset test", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale surveys
  survs <- cli$available_datasets()

  # grab just the geographic data
  hhs_geo <- which(survs$FileType %in% c("Geographic Data"))

  # check rds only
  downloads <- cli$download_datasets(dataset_filenames = survs[sample(length(hhs_geo),2,replace=FALSE),]$FileName,download_option = "r")

  unlink(td)


})
