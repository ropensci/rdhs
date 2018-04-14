context("Client Setup")

test_that("save credentials", {

  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
    testthat::expect_identical(normalizePath(file.path("credentials")),
                               cli$.__enclos_env__$private$credentials_path)
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",root = td)
  }

  # check for when/if (how) the client got deleted but not the root directory
  file.remove(file.path(cli$get_root(),client_file_name()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",root = td)
  }

  unlink(td)

})
