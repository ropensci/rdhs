context("Extraction")

test_that("query codes having downloaded surveys", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(),as.numeric(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale surveys
  survs <- cli$available_surveys()

  # grab survey types needed
  hhs_dta <- which(survs$FileFormat == "Stata dataset (.dta)" & survs$FileType %in% c("Household Recode","Household Member Raw"))
  hhs_geo <- which(survs$FileType %in% c("Geographic Data"))
  wanted <- unique(c(hhs_dta,hhs_geo))

  # check rds only for 4
  sample_survs <- sample(length(wanted),4,replace=FALSE)
  downloads <- sapply(wanted[sample_survs],function(x){
    message(which(wanted==x)/length(wanted))
    cli$download_survey(desired_survey = survs[x,],download_option = "r")
    })

  # create questions
  quest <- cli$survey_questions(survs[sample_survs,],search_terms = c("fever","malaria","test"))

  unlink(td)
})

