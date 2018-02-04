context("Downloads")

test_that("avaialble surveys and download work", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir())

  # create auth through whichever route is valid for the environment
  if(file.exists(".credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = ".credentials",root = td)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale surveys
  survs <- cli$available_surveys()

  # check zip only
  downloads <- sapply(1:4,function(x) cli$download_survey(desired_survey = survs[x,],download_option = "z"))

  # check extract only
  downloads <- sapply(5:8,function(x) cli$download_survey(desired_survey = survs[x,],download_option = "e"))

  # check rds only
  downloads <- sapply(9:12,function(x) cli$download_survey(desired_survey = survs[x,],download_option = "r"))

  # check both
  downloads <- sapply(13:16,function(x) cli$download_survey(desired_survey = survs[x,],download_option = "b"))

  unlink(td)

})


test_that("Geo dataset test", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir())

  # create auth through whichever route is valid for the environment
  if(file.exists(".credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = ".credentials",root = td)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale surveys
  survs <- cli$available_surveys()

  # grab just the geographic data
  hhs_geo <- which(survs$FileType %in% c("Geographic Data"))

  # check rds only
  downloads <- sapply(hhs_geo[sample(length(hhs_geo),4,replace=FALSE)],function(x) cli$download_survey(desired_survey = survs[x,],download_option = "r"))

  unlink(td)


})
