context("API calls")

test_that("can request api", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists(".credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = ".credentials",root = td)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td)
  }

  cli$dhs_api_request(api_endpoint = "surveys",query = list("indicatorIds"="ML_AMLD_C_QNN"))

  unlink(td)

})
