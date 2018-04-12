context("API calls")

test_that("can request api through dhs_api_request via client", {

  skip_if_no_auth()
  testthat::skip_on_cran()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client(api_key = "ICLSPH-527168",root = td)
  }

  req <- cli$dhs_api_request(api_endpoint = "surveys",query = list("indicatorIds"="ML_AMLD_C_QNN"))

  # test the error catch for response http codes
  req <- cli$dhs_api_request(api_endpoint = "surveys",query = list("breakdowAndCry"="subartyping"))

  unlink(td)

})
