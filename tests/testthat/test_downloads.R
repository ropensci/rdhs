context("Downloads")

test_that("avaialble surveys and download work", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir())

  # create auth through whichever route is valid for the environment
  if(file.exists(".credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = ".credentials",root = td,force_initialise = TRUE)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td,force_initialise = TRUE)
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
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = ".credentials",root = td,force_initialise = TRUE)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td,force_initialise = TRUE)
  }

  # create availbale surveys
  survs <- cli$available_surveys()

  # grab some geo datasets
  geo_surveys <- subsetted_surveys[subsetted_surveys$FileType =="Geographic Data",]

  # check geo
  #downloads <- sapply(sample(dim(geo_surveys)[1],size = 4),function(x) cli$download_survey(desired_survey = survs[x,],download_option = "z"))

  unlink(td)

})
