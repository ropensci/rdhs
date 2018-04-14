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
