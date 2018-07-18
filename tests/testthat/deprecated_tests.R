# # deprecated_tests
#
# context("zzz")
#
# test_that("check for poor errors in creds etc", {
#
#
#   # first let's grab the default client objct so we can rewrite it
#   old_client <- .rdhs$client
#   old_cred <- Sys.getenv(renv_cred_path_name())
#   old_root <- Sys.getenv(renv_root_path_name())
#
#   # save them so this test doesn't nuke the others
#   old_envs <- save_current_envs()
#   if (file.exists(file.path(normalizePath("~"), ".Renviron"))) {
#     old_renviron <- readLines(file.path(normalizePath("~"), ".Renviron"))
#   }
#
#   # check for no root and cred
#   Sys.setenv("rdhs_CREDENTIALS_PATH" = "")
#   Sys.setenv("rdhs_ROOT_PATH" = "")
#   Sys.setenv("rdhs_STARTUP_LOUD" = TRUE)
#   expect_message(rdhs:::.onAttach(), "For help with rdhs")
#
#   # lets make a failing cred
#   write("twaddle", file = "rubbish_no_more.txt")
#   Sys.setenv("rdhs_CREDENTIALS_PATH" = "rubbish_no_more.txt")
#   Sys.setenv("rdhs_ROOT_PATH" = "rubbish_no_more.txt")
#   expect_message(rdhs:::.onAttach(), "last time are not valid")
#
#   file.remove("rubbish_no_more.txt")
#   expect_message(rdhs:::.onAttach(), "no longer there")
#
#   # remove this
#   unlink("rubbish_no_more.txt")
#   unlink("dummy", recursive = TRUE)
#
#   # reset our credentials
#   restore_current_envs(old_envs)
#   create_correct_credentials(old_cred)
#
#   if (exists("old_renviron")) {
#     write(x = old_renviron, file.path(normalizePath("~"), ".Renviron"))
#   } else {
#     write(x = "", file.path(normalizePath("~"), ".Renviron"))
#   }
#
#   Sys.setenv("rdhs_STARTUP_LOUD" = FALSE)
# })
#
#
# test_that("non rdhs env set", {
#   expect_error(set_renviron("twaddle", 7))
# })
