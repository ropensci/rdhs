context("Extraction")

test_that("query codes having downloaded surveys", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale surveys
  survs <- cli$available_surveys()

  # grab survey types needed
  hhs_dta <- which(survs$FileFormat == "Stata dataset (.dta)" & survs$FileType %in% c("Household Member Recode"))

  # check rds only for 1
  sample_survs <- sample(length(hhs_dta),1,replace=FALSE)
  downloads <-  cli$download_survey(desired_survey = survs[hhs_dta[sample_survs],],download_option = "r")

  # create questions
  quest <- cli$survey_questions(survs[hhs_dta[sample_survs],],search_terms = c("fever","malaria","test"))

  unlink(td)
})

