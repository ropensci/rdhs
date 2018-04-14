context("Extraction")

test_that("query codes having downloaded surveys", {

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

  # check rds only for one survey
  downloads <-  cli$get_datasets(dataset_filenames = "AOBR62DT.ZIP",download_option = "r")
  downloads <-  cli$get_datasets(dataset_filenames = "AOBR62SV.ZIP",download_option = "r",all_lower = FALSE,reformat = TRUE)

  ## QUESTIONS AND VARIABLE TESTS

  # create questions
  quest <- cli$survey_questions(dataset_filenames = "AOBR62DT.ZIP",search_terms = c("fever","malaria","test"))

  # check the regeex option
  quest <- cli$survey_questions(dataset_filenames = "AOBR62DT.ZIP",search_terms = c("fever|test"))

  # check the essetial temrs option
  quest <- cli$survey_questions(dataset_filenames = "AOBR62DT.ZIP",search_terms = c("fever|test"),essential_terms = "malaria")


  # check the same with an uppercase survey, one variable that is na and essential
  quest <- cli$survey_variables(dataset_filenames = "AOBR62SV.ZIP",variables =  "hml32",essential_variables = c("hml35","h32n"),
                                reformat=TRUE)

  # check variable
  quest <- cli$survey_questions(dataset_filenames = "AOBR62DT.ZIP",search_terms = c("fever|test"))


  ####

  # extract the data
  extract <- cli$extract(quest,add_geo = T)

  # extract the qeustions
  extract_neat <- rdhs:::extract_codes_to_descriptions(extract,quest)

  ## and repeat for sruveys that have no geo

  # check rds only for one survey
  downloads <-  cli$get_datasets(dataset_filenames = "ZWHR31SV.ZIP",download_option = "r")
  r <- readRDS(downloads$ZWHR31SV)
  r <- data_and_labels(r)

  # create questions
  quest <- cli$survey_questions(dataset_filenames = "ZWHR31SV.ZIP",search_terms = c("Has refrigerator"))

  # extract the data
  extract <- cli$extract(quest,add_geo = T)
  expect_identical(extract$ZWHR31SV$LATNUM[1],NA)

  # and repreat for reformatted ones
  quest <- cli$survey_questions(dataset_filenames = "AOBR62DT.ZIP",search_terms =  "malaria",reformat = TRUE)
  extract <- cli$extract(quest,add_geo = T)
  unlink(td)
})


test_that("rbind_labelled", {

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

  d <- cli$get_datasets(c("AOBR62FL.ZIP","BJBR41FL.ZIP"))

  quest <- cli$survey_variables(c("AOBR62FL.ZIP","BJBR41FL.ZIP"),variables = c("v024","v130"))

  extract <- cli$extract(quest,add_geo = TRUE)

  expect_warning(rbind_labelled(extract))

  dat <- rbind_labelled(extract,labels=list("v024"="concatenate","v130"= "concatenate"))

  dat <- rbind_labelled(extract,labels=list("v024"="concatenate"),warn = FALSE)

  })

