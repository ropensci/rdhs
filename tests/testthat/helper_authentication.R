skip_if_no_auth <- function(credentials_path = ".credentials") {

  have_cred_path <- file.exists(credentials_path)

  if (!have_cred_path & (identical(Sys.getenv("rdhs_USER_PASS"), "") | identical(Sys.getenv("rdhs_USER_EMAIL"), "") | identical(Sys.getenv("rdhs_USER_PROJECT"), ""))) {
    skip("No authentication available")
  }

  if(have_cred_path){
    set_environment_credentials(read_credentials(credentials_path))
  }
}
