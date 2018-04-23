skip_if_no_auth <- function(credentials_path = "credentials") {

  have_cred_path <- file.exists(credentials_path)

  if (!have_cred_path & (identical(Sys.getenv("rdhs_USER_PASS"), "") | identical(Sys.getenv("rdhs_USER_EMAIL"), "") | identical(Sys.getenv("rdhs_USER_PROJECT"), ""))) {
    skip("No authentication available")
  }

  if(have_cred_path){
    set_environment_credentials(read_credentials(credentials_path))
  }
}



new_rand_client <- function(){

# Create new directory
td <- file.path(tempdir(),as.integer(Sys.time()))

# create auth through whichever route is valid for the environment
if(file.exists("credentials")){
  cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
} else {
  cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",root = td)
}

return(cli)

}


save_current_envs <- function(){

  # save them so this test doesn't nuke the others
  em <- Sys.getenv("rdhs_USER_EMAIL")
  pass <- Sys.getenv("rdhs_USER_PASS")
  proj <- Sys.getenv("rdhs_USER_PROJECT")

  return(c(em,pass,proj))

}

restore_current_envs <- function(old_env){

  # restore them so this test doesn't nuke the others
  Sys.setenv("rdhs_USER_EMAIL"=old_env[1])
  Sys.setenv("rdhs_USER_PASS"=old_env[2])
  Sys.setenv("rdhs_USER_PROJECT"=old_env[3])

}

create_correct_credentials <- function(filename){

  envs <- save_current_envs()

  #  make a credentials object
  write(paste0("email=",envs[1],"\npassword=",envs[2],"\nproject=",envs[3]),
        file = filename)
  out <- set_dhs_credentials(credentials=filename)

}
