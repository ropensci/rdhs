.rdhs <- new.env(parent = emptyenv())

rdhs_reset <- function() {
  rm(list = ls(.rdhs, all.names = TRUE), envir = .rdhs)
}

.onLoad <- function(...) {

  # just in case clear the pacakge environment
  rdhs_reset()

  # check
  .rdhs$test <- 1

  # set up the default root
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs",Sys.info()["user"])

  # check for a client here
  if(file.exists(file.path(.rdhs$default_root,client_file_name()))){

    # read in this client
    .rdhs$client <- client_dhs(root = .rdhs$default_root)

    # if the user has previously specified a different root then grab it and then
    # set the environment client to that
    udr <- .rdhs$client$.__enclos_env__$private$user_declared_root
    if(!is.null(udr)){
      if(file.exists(file.path(udr,client_file_name()))){
      .rdhs$client <- client_dhs(root = udr)
    }
    }

    packageStartupMessage("\nWelcome back :) \n",
            "-------------------------\n",
            "rdhs will be using the login credentials you set last time, which it will read from:\n   -> ",
            .rdhs$client$.__enclos_env__$private$credentials_path,"\n",
            "It will also save any datasets you download inside this directory:\n   -> ",
            .rdhs$client$get_root())

    # and now let's hande the credentials from this client
    handle_credentials(.rdhs$client$.__enclos_env__$private$credentials_path)


  }

}

set_rdhs_client_user_declared_root <- function(path){

  old <- .rdhs$client$.__enclos_env__$private$user_declared_root

  # assign the new user_declared_root
  .rdhs$client$.__enclos_env__$private$user_declared_root <- normalizePath(path)
  invisible(old)
}

set_rdhs_client_credentials <- function(credentials){

  old <- .rdhs$client$.__enclos_env__$private$credentials_path

  # assign the new user_declared_root
  .rdhs$client$.__enclos_env__$private$credentials_path <- normalizePath(credentials)
  invisible(old)
}

set_test <- function(value){
  old <- .rdhs$test
  .rdhs$test <- value
  invisible(old)
}
