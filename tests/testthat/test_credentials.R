context("Credentials")

test_that("credentials", {

  # first get the credentials
  skip_if_no_auth()

  # save them so this test doesn't nuke the others
  em <- Sys.getenv("rdhs_USER_EMAIL")
  pass <- Sys.getenv("rdhs_USER_PASS")
  proj <- Sys.getenv("rdhs_USER_PROJECT")

  # remove these credentials
  Sys.setenv("rdhs_USER_EMAIL"="")
  Sys.setenv("rdhs_USER_PASS"="")
  Sys.setenv("rdhs_USER_PROJECT"="")

  # check for no credentials provided with nothing in the envrionment catch
  expect_error(handle_credentials(credentials=NULL))

  # check for no good credential path
  expect_error(handle_credentials(credentials="rubbish"))

  # now lets make a credentials object
  write("email=dummy@gmail.com\npassword=\"dummy\"\nproject=Dummy space",
        file = "rubbish_no_more.txt")
  handle_credentials(credentials="rubbish_no_more.txt")

  # check they are what they should be
  expect_identical(Sys.getenv("rdhs_USER_EMAIL"),"dummy@gmail.com")
  expect_identical(Sys.getenv("rdhs_USER_PASS"),"dummy")
  expect_identical(Sys.getenv("rdhs_USER_PROJECT"),"Dummy space")

  # reset our credentials
  Sys.setenv("rdhs_USER_EMAIL"=em)
  Sys.setenv("rdhs_USER_PASS"=pass)
  Sys.setenv("rdhs_USER_PROJECT"=proj)

  # check for unnamed list attempt
  expect_error(read_credentials(list("humpty","dumpty","project")))

  # check for too many args
  expect_error(read_credentials(list("email"="humpty","password"="dumpty","project"="project","huh"=2)))

  # remove this
  unlink("rubbish_no_more.txt")

})



test_that("set_dhs_credentials", {

  # first let's grab the default client objct so we can rewrite it
  old_client <- .rdhs$client

  # lets make a credentials object
  write("email=dummy@gmail.com\npassword=\"dummy\"\nproject=Dummy space",
        file = "rubbish_no_more.txt")
  out <- set_dhs_credentials(credentials="rubbish_no_more.txt")

  # check that it returns the client invisibly
  expect_identical(class(out),c("client_dhs", "R6"))

  # check it's set the system variables
  expect_identical(Sys.getenv("rdhs_USER_EMAIL"),"dummy@gmail.com")
  expect_identical(Sys.getenv("rdhs_USER_PASS"),"dummy")
  expect_identical(Sys.getenv("rdhs_USER_PROJECT"),"Dummy space")

  expect_identical(.rdhs$client$.__enclos_env__$private$credentials_path,
                   normalizePath("rubbish_no_more.txt"))

  # now let's try it with a new root
  out <- set_dhs_credentials(credentials="rubbish_no_more.txt",
                             root=file.path(getwd(),"dummy"))

  # the env client root should now be this new dummy
  expect_identical(.rdhs$client$.__enclos_env__$private$user_declared_root %>% basename,
                   "dummy")
  expect_null(.rdhs$client$.__enclos_env__$private$user_declared_root)

  # and the client at the default root should have dummy as the udr
  dcrc <- readRDS(file.path(.rdhs$default_root,client_file_name()))
  expect_identical(dcrc$.__enclos_env__$private$user_declared_root %>% basename,
                   "dummy")

  ## and if it's working we should get the same now after triggering an .onLoad

  # so first let's clear the package environment to be doubly sure
  rdhs_reset()
  expect_null(.rdhs$test)

  rdhs:::.onLoad()

  # the env client root should now be this new dummy
  expect_identical(.rdhs$client$get_root() %>% basename,
                   "dummy")
  expect_null(.rdhs$client$.__enclos_env__$private$user_declared_root)

  # and the client at the default root should have dummyas the udr
  dcrc <- readRDS(file.path(.rdhs$default_root,client_file_name()))
  expect_identical(dcrc$.__enclos_env__$private$user_declared_root %>% basename,
                   "dummy")

  # remove this
  unlink("rubbish_no_more.txt")
  unlink("dummy")

  # and put the old client back in place
  saveRDS(old_client,file.path(old_client$get_root(),client_file_name()))

})
