context("Credentials")

test_that("credentials", {

  # first get the credentials
  skip_if_no_auth()

  # save them so this test doesn't nuke the others
  old_envs <- save_current_envs()

  # remove these credentials
  Sys.setenv("rdhs_USER_EMAIL" = "")
  Sys.setenv("rdhs_USER_PASS" = "")
  Sys.setenv("rdhs_USER_PROJECT" = "")

  # check for no credentials provided with nothing in the envrionment catch
  expect_error(handle_credentials(credentials = NULL))

  # check for no good credential path
  expect_error(handle_credentials(credentials = "rubbish"))

  # now lets make a credentials object
  write("email=dummy@gmail.com\npassword=\"dummy\"\nproject=Dummy space",
    file = "rubbish_no_more.txt"
  )
  handle_credentials(credentials = "rubbish_no_more.txt")

  # check they are what they should be
  expect_identical(Sys.getenv("rdhs_USER_EMAIL"), "dummy@gmail.com")
  expect_identical(Sys.getenv("rdhs_USER_PASS"), "dummy")
  expect_identical(Sys.getenv("rdhs_USER_PROJECT"), "Dummy space")

  # reset our credentials
  restore_current_envs(old_envs)

  # check for unnamed list attempt
  expect_error(read_credentials(list("humpty", "dumpty", "project")))

  # check for too many args
  expect_error(read_credentials(list(
    "email" = "humpty",
    "password" = "dumpty",
    "project" = "project",
    "huh" = 2
  )))

  # remove this
  unlink("rubbish_no_more.txt")
})



test_that("set_dhs_credentials", {

  Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)

  # first let's grab the default client objct so we can rewrite it
  old_client <- .rdhs$client
  old_cred <- Sys.getenv(renv_cred_path_name())
  old_root <- Sys.getenv(renv_root_path_name())

  # save them so this test doesn't nuke the others
  old_envs <- save_current_envs()
  if (file.exists(file.path(normalizePath("~"), ".Renviron"))) {
    old_renviron <- readLines(file.path(normalizePath("~"), ".Renviron"))
  }

  # lets make a credentials object
  write("email=dummy@gmail.com\npassword=\"dummy\"\nproject=Dummy space",
    file = "rubbish_no_more.txt"
  )

  set_renviron(variable = renv_cred_path_name(), value = "rubbish_no_more.txt",
               ask = TRUE)

  out <- set_dhs_credentials(credentials = "rubbish_no_more.txt")

  # check that it returns the client invisibly
  expect_identical(class(out), c("client_dhs", "R6"))

  # check it's set the system variables
  expect_identical(Sys.getenv("rdhs_USER_EMAIL"), "dummy@gmail.com")
  expect_identical(Sys.getenv("rdhs_USER_PASS"), "dummy")
  expect_identical(Sys.getenv("rdhs_USER_PROJECT"), "Dummy space")

  expect_identical(
    .rdhs$client$.__enclos_env__$private$credentials_path,
    normalizePath("rubbish_no_more.txt", winslash = "/")
  )

  # now let's try it with a new root
  out <- set_dhs_credentials(
    credentials = "rubbish_no_more.txt",
    root = file.path(getwd(), "dummy")
  )

  # the env client root should now be this new dummy
  expect_identical(
    .rdhs$client$get_root() %>% basename(),
    "dummy"
  )
  expect_null(.rdhs$client$.__enclos_env__$private$user_declared_root)

  # and the client at the default root should still be there
  dcrc <- readRDS(file.path(.rdhs$default_root, client_file_name()))
  expect_identical(
    dcrc$get_root() %>% basename(),
    rappdirs::user_cache_dir("rdhs", Sys.info()["user"]) %>% basename()
  )

  ## and if it's working we should get the same now after triggering an .onLoad

  # so first let's clear the package environment to be doubly sure
  rdhs_reset()
  expect_null(.rdhs$test)

  Sys.setenv("rdhs_STARTUP_LOUD" = TRUE)
  expect_message(rdhs:::.onAttach())
  Sys.setenv("rdhs_STARTUP_LOUD" = FALSE)

  # the env client root should now be this new dummy
  expect_identical(
    .rdhs$client$get_root() %>% basename(),
    "dummy"
  )
  expect_null(.rdhs$client$.__enclos_env__$private$user_declared_root)

  # and we should have these options saved on the .Renviron
  environ <- readLines(file.path(normalizePath("~"), ".Renviron"))
  expect_true(any(grepl(rdhs:::renv_cred_path_name(), environ)))
  expect_true(any(grepl(rdhs:::renv_root_path_name(), environ)))

  # remove this
  unlink("rubbish_no_more.txt")
  unlink("dummy", recursive = TRUE)

  # reset our credentials
  restore_current_envs(old_envs)
  if (exists("old_renviron")) {
    write(x = old_renviron, file.path(normalizePath("~"), ".Renviron"))
  } else {
    write(x = "", file.path(normalizePath("~"), ".Renviron"))
  }

  # and put the old client back in place and reset
  # the renvirons if they existed before hand
  if (!is.null(old_client)) {
    saveRDS(old_client, file.path(old_client$get_root(), client_file_name()))
    .rdhs$client <- old_client

    expect_identical(
      rdhs:::set_rdhs_CREDENTIALS_PATH(
        old_client$.__enclos_env__$private$credentials_path
      ),
      old_client$.__enclos_env__$private$credentials_path
    )
    expect_identical(
      rdhs:::set_rdhs_ROOT_PATH(old_client$get_root()),
      normalizePath(old_client$get_root(), winslash = "/", mustWork = FALSE)
    )
  }
})
