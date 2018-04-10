context("Extraction")

test_that("query codes having downloaded surveys", {

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

  # check rds only for one survey
  downloads <-  cli$download_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r")

  # create questions
  quest <- cli$survey_questions(dataset_filenames = "AOBR62DT.ZIP",search_terms = c("fever","malaria","test"))

  # extract the data
  extract <- cli$extract(quest,add_geo = T)

  # extract the qeustions
  extract_neat <- rdhs:::extract_codes_to_descriptions(extract,quest)

  unlink(td)
})

