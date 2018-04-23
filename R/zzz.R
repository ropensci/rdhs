# Package Load set up

.rdhs <- new.env(parent = emptyenv())

rdhs_reset <- function() {
  rm(list = ls(.rdhs, all.names = TRUE), envir = .rdhs)
}

.onLoad <- function(...) suppressPackageStartupMessages(.onAttach())

.onAttach <- function(...) {

  # just in case clear the pacakge environment
  rdhs_reset()

  # check
  .rdhs$test <- 1

  # set up the default root
  .rdhs$default_root <- rappdirs::user_cache_dir("rdhs",Sys.info()["user"])

  # check for existing credentials path
  cred_path <- Sys.getenv(renv_cred_path_name())
  root_path <- Sys.getenv(renv_root_path_name())

  # check for a client here
  if(root_path == "" | cred_path == ""){
    packageStartupMessage("\nFor help with rdhs and to report any issues please head to the github repo:\n   -> ",
                          "https://github.com/OJWatson/rdhs\n")
  } else {

    # check the credentials file we have for them is still valid
    credentials <- normalizePath(cred_path,winslash="/", mustWork = FALSE)

    if(!file.exists(credentials)){
      packageStartupMessage("Your login credentials provided last time are no longer there!\n",
                            "Please recreate your credentials file at the following path and reload rdhs:\n   -> ",
                            credentials,"\n",
                            "Alternatively use set_dhs_credentials() to specify a new credentials file.")
      return(invisible(credentials))
    }

    # and check if it is still valid
    out <- tryCatch(expr = read_credentials(credentials),
                    error=function(e) {
                      packageStartupMessage("Your login credentials provided last time are not valid!\n",
                                            "Please check your credentials file at the following path and reload rdhs:\n   -> ",
                                            credentials,"\n",
                                            "Alternatively use set_dhs_credentials() to specify a new credentials file.")
                    })

    # and return now if that errored
    if(is.null(out)) return(invisible(credentials))


    # if we have got to this point then hooray and let's get their client and set it in the package environment
    .rdhs$client <- client_dhs(credentials = Sys.getenv(renv_cred_path_name()),
                               root = Sys.getenv(renv_root_path_name()))

    packageStartupMessage("\nWelcome back :) \n",
                          "-------------------------\n",
                          "rdhs will be using the login credentials you set last time, which it will read from:\n   -> ",
                          .rdhs$client$.__enclos_env__$private$credentials_path,"\n",
                          "It will also save any datasets you download inside this directory:\n   -> ",
                          .rdhs$client$get_root(),"\n",
                          "If you wish to change your credentials or where your datasets are saved, please use set_dhs_credentials()")

    # and now let's hande the credentials from this client
    return(invisible(.rdhs$client))

  }

}

# set .Renviron variable
set_renviron <- function(variable,value){

  # first do some checking:
  if(substr(variable,1,4) != "rdhs") stop("renviron variable to be set does not begin \"rdhs\"")

  # now remove any trailing " that may be in there
  value <- gsub("\"","",value)
  variable <- gsub("\"","",variable)

  # next grab the current .Renviron if it exists
  if(file.exists(file.path(normalizePath("~",winslash="/"),".Renviron"))){

  current <- readLines(file.path(normalizePath("~",winslash="/"),".Renviron"))

  # check to see if the variable already exists
  current_vars <- strsplit(current,"=") %>% lapply(function(x) x[1]) %>% unlist
  presets <- grepl(variable,current_vars)

  # remove any previous rdhs variables for the variable of interest
  current <- current[!presets]

  # add our new value always placing it in quotes
  new <- c(current, paste0(variable," = ","\"",value,"\""))

  } else {
    new <- paste0(variable," = ","\"",value,"\"")
  }

  writeLines(new,file.path(normalizePath("~",winslash="/"),".Renviron"))

  # and set it within our current session as well
  args <- list(value) ; names(args) <- variable
  do.call(Sys.setenv,args)

}

set_rdhs_CREDENTIALS_PATH <- function(path){

  # normalise credentials here for ease with no warnings as we'll check it ourselves
  credentials <- normalizePath(path,winslash="/", mustWork = FALSE)
  if(!file.exists(credentials)) stop("credentials file does not exist. Please check:\n   -> ",credentials)

  # and let's check their validity before going any further
  out <- read_credentials(credentials)

  # if these are fine let's set these to the .Renviron
  set_renviron(renv_cred_path_name(),credentials)

  invisible(Sys.getenv(renv_cred_path_name()))
}

set_rdhs_ROOT_PATH <- function(path){

  # normalise the root path and create the directory
  root <- normalizePath(path,winslash="/", mustWork = FALSE)
  dir.create(root,recursive = TRUE,showWarnings = FALSE)

  # and let's then set these too to the .Renviron
  set_renviron(renv_root_path_name(),root)

  invisible(Sys.getenv(renv_root_path_name()))
}

check_client <- function(client){

  # if it's null then return early
  if(is.null(client)) return(FALSE)

  # and if this is somehow not a client object we know return FALSE
  if(!identical(class(client),c("client_dhs","R6"))) return(FALSE)

  cred_path <- client$.__enclos_env__$private$credentials_path
  root_path <- client$get_root()

  # check for a client here
  if(root_path == "" | cred_path == "") return(FALSE)

    # check the credentials file we have for them is still valid
    credentials <- normalizePath(cred_path,winslash="/", mustWork = FALSE)

    if(!file.exists(credentials)) return(FALSE)

    # and check if it is still valid
    out <- tryCatch(expr = read_credentials(credentials),error=function(e) { NULL })

    # and return now if that errored
    if(is.null(out)) return(FALSE)

    # check the storr database is valid
    out <- inherits(client$.__enclos_env__$private$storr,"storr")

    # and check if it can be used
    out <- tryCatch(expr = client$.__enclos_env__$private$storr$set("dummy",value = 1),error=function(e) { NULL })


    # and return now if that errored
    if(is.null(out)) return(FALSE)

    ## if we have got this far it's probably good
    return(TRUE)
}
