context("zzz")

test_that("check for poor errors in creds etc",{


  # first let's grab the default client objct so we can rewrite it
  old_client <- .rdhs$client
  old_cred <- Sys.getenv(renv_cred_path_name())
  old_root <- Sys.getenv(renv_root_path_name())

  # save them so this test doesn't nuke the others
  old_envs <- save_current_envs()
  if(file.exists(file.path(normalizePath("~"),".Renviron"))){
    old_renviron <- readLines(file.path(normalizePath("~"),".Renviron"))
  }

  # check for no root and cred
  Sys.setenv("rdhs_CREDENTIALS_PATH"="")
  Sys.setenv("rdhs_ROOT_PATH"="")
  expect_message(rdhs:::.onAttach(),"For help with rdhs")

  # lets make a failing cred
  write("twaddle",file = "rubbish_no_more.txt")
  Sys.setenv("rdhs_CREDENTIALS_PATH"="rubbish_no_more.txt")
  Sys.setenv("rdhs_ROOT_PATH"="rubbish_no_more.txt")
  expect_message(rdhs:::.onAttach(),"last time are not valid")

  # and now just a failing root
  write("email=dummy@gmail.com",file = "rubbish_no_more.txt")
  expect_message(rdhs:::.onAttach(),"does not appear to be here anymore")

  # remove this
  unlink("rubbish_no_more.txt")
  unlink("dummy",recursive = TRUE)

  # reset our credentials
  restore_current_envs(old_envs)
  if(exists("old_renviron")){
    write(x = old_renviron,file.path(normalizePath("~"),".Renviron"))
  } else {
    write(x = "",file.path(normalizePath("~"),".Renviron"))
  }

  # and put the old client back in place and reset the renvirons if they existed before hand
  if(!is.null(old_client)){

    saveRDS(old_client,file.path(old_client$get_root(),client_file_name()))
    .rdhs$client <- old_client

    expect_identical(rdhs:::set_rdhs_CREDENTIALS_PATH(old_client$.__enclos_env__$private$credentials_path),
                     old_client$.__enclos_env__$private$credentials_path)
    expect_identical(rdhs:::set_rdhs_ROOT_PATH(old_client$get_root()),
                     normalizePath(old_client$get_root(),winslash="/", mustWork = FALSE))

  }


})


test_that("non rdhs env set",{ expect_error(set_renviron("twaddle",7)) })

test_that("type_convert_df",{

  df <- data.frame("huh"=c("apple","apple","orange"))
  df <- type_convert_df(df)
  expect_null(levels(df$huh))

})

test_that("client_check",{


  expect_false(check_client("thing"))

  cli <- new_rand_client()
  cli$.__enclos_env__$private$credentials_path <- ""
  cli$.__enclos_env__$private$root <- ""
  expect_false(check_client(cli))

  cli <- new_rand_client()
  write("twaddle",file = cli$.__enclos_env__$private$credentials_path)
  expect_false(check_client(cli))

  })
