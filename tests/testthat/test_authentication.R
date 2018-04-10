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
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client(api_key = "ICLSPH-527168",root = td)
  }

  # create availbale datasets
  survs <- cli$available_datasets()

  # check the names
  expect_identical(names(survs),c("FileFormat","FileSize","DatasetType","SurveyNum","SurveyId",
                                  "FileType","FileDateLastModified","SurveyYearLabel","SurveyType",
                                  "SurveyYear","DHS_CountryCode","FileName","CountryName","URLS"))

  # there should definitely be more than this many urls
  expect_true(dim(survs)[1]>5000)

  unlink(td)



})
