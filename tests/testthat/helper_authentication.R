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
