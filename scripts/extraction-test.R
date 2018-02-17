context("Extraction")

test_that("query codes having downloaded surveys", {

  skip_if_no_auth()

  # Create new directory
  td <- "D:/DHS"

  cli$set_cache_date(cli$get_cache_date()-20000000)
  cli$save_client()

  # create auth through whichever route is valid for the environment
  if(file.exists(".credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = ".credentials",root = td)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td)
  }

# create availbale surveys
  survs <- cli$available_surveys()

  # grab survey types needed
  hhs_dta <- which(survs$FileFormat == "SPSS dataset (.sav)" & survs$FileType %in% c("Household Member Recode"))
  hhs_geo <- which(survs$FileType %in% c("Geographic Data"))
  wanted <- unique(c(hhs_dta))

  # check rds only for 4
  downloads <- sapply(wanted,function(x){
    message(which(wanted==x)/length(wanted))
    cli$download_survey(desired_survey = survs[x,],download_option = "r")
  })

  downloads2 <- sapply(hhs_geo,function(x){
    message(which(hhs_geo==x)/length(hhs_geo))
    cli$download_survey(desired_survey = survs[x,],download_option = "r")
  })

  long_question_list <- c("Case Identification","Country code and phase","Cluster number","Household number",
                          "Respondent's line number (answering Household questionnaire)","Ultimate area unit",
                          "Household sample weight (6 decimals)","Month of interview","Year of interview","Date of interview (CMC)",
                          "Number of household members",
                          "Number of de jure members","Number of de facto members","Number of children 5 and under (de jure)","Province",
                          "Age of household members","age",
                          "Child's age in months","Sex",
                          "Month of birth","Year of birth","Mosquito Bed Net Designation Number","Net treated with insecticide when bought",
                          "Net treatment status","Brand of net","Net treated since receiving","Net treated since receiving",
                          "Insecticide-Treated Net (ITN)","Number of persons who slept under this net","bednet","bed Net",
                          "Someone slept under this net last night","Line number of person who slept in this net",
                          "Type of Mosquito Bed Net(s) person slept under last night","Net Designation Number (HMLIDX) for 1st net person slept under last night",
                          "Net Designation Number (HMLIDX) for 2nd net person slept under last night",
                          "NA - Net Designation Number (HMLIDX) for 3rd net person slept under last night","Corrected age from Individual file",
                          "Age in months (for children)","Read consent statement for malaria","Final result of malaria from blood smear test","blood smear",
                          "Result of malaria measurement","Bar code for blood smear sample","rapid test")

  # create questions
  quest <- cli$survey_questions(survs[hhs_dta,],search_terms = long_question_list)

})

