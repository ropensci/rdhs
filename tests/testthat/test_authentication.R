context("Authentication")

test_that("dhs_authenticate works", {

  skip_if_no_auth()

  expect_equal(rdhs:::dhs_authenticate(your_email=Sys.getenv("rdhs_USER_EMAIL"),
                                       your_password=Sys.getenv("rdhs_USER_PASS"),
                                       your_project=Sys.getenv("rdhs_USER_PROJECT"))$proj_id, "111616")
})

test_that("available_surveys works", {

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

  # check the names
  expect_identical(names(survs),c("FileFormat","FileSize","DatasetType","SurveyNum","SurveyId",
                                  "FileType","FileDateLastModified","SurveyYearLabel","SurveyType",
                                  "SurveyYear","DHS_CountryCode","FileName","CountryName","URLS"))

  # there should definitely be more than this many urls
  expect_true(dim(survs)[1]>5000)

  unlink(td)



})
