context("Client Setup")

test_that("save credentials", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
    testthat::expect_identical(normalizePath(file.path("credentials")),
                               cli$.__enclos_env__$private$credentials_path)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td)
  }

  unlink(td)

})
