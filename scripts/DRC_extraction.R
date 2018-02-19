
## DRC extractions

td <- "D:/DHS"

# create auth through whichever route is valid for the environment
if(file.exists("credentials")){
  cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
} else {
  cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = td)
}

# create availbale surveys
survs <- cli$available_surveys()
survs$Survey <- strsplit(survs$FileName,".",fixed=T) %>% lapply(function(x) x[1]) %>% unlist

# grab survey types needed and not the couple or househod recodes
hhs_dta <- which(survs$FileFormat == "SPSS dataset (.sav)" & survs$DHS_CountryCode %in% c("CD") & !survs$FileType %in% c( "Couples' Recode","Household Recode"))
hhs_geo <- which(survs$FileType %in% c("Geographic Data"))

# grab all the needed surveys
downloads <- sapply(hhs_dta,function(x){
  message(which(hhs_dta==x)/length(hhs_dta))
  cli$download_survey(desired_survey = survs[x,],download_option = "r")
})

downloads2 <- sapply(hhs_geo,function(x){
  message(which(hhs_geo==x)/length(hhs_geo))
  cli$download_survey(desired_survey = survs[x,],download_option = "r")
})

## now work through questions and remove any collated questions, i.e. questions that have been repeated by flattening other surveys.
quest <- cli$survey_questions(survs[hhs_dta,],search_terms = "")
quest <- quest[-grep("$",quest$Code,fixed = T),]

## grab countrycode to name conversions
wh <- cli$dhs_api_request(api_endpoint = "countries")
cc_lookup <- cbind(wh$CountryName,wh$DHS_CountryCode)

# set up results and go
list_res <- list() ; length(list_res) <- length(unique(paste0(quest$CountryCode,quest$SurveyYear)))
names(list_res) <- unique(paste0(quest$CountryCode,quest$SurveyYear))

code_years <- unique(paste0(quest$CountryCode,quest$SurveyYear))
quest$CodeYears <- paste0(quest$CountryCode,quest$SurveyYear)

for(j in 1:length(list_res)){

  message(paste0("Starting Survey Year ",j," out of ",length(list_res)," surveys"))
  surveys_j <- unique(quest$SurveyPath[quest$CodeYears==code_years[i]])

  bind_j <- NA

for(i in 1:length(surveys_j)){

  # survey data
  file <- surveys_j[i]
  r <- readRDS(file)

  # fetch codes that relate to this survey
  quest_rows_i <- which(quest$SurveyPath==surveys_j[i])
  codes <- quest$Code[quest_rows_i]
  match <- match(codes,names(r$Survey))
  missing <- which(is.na(match))

  match[which(is.na(match))] <- 1
  results <- r$Survey[match]
  results[,missing] <- NA




  bind_j <- cbind(bind_j,results)

}


#spatial data
ge_match <- match(survs$SurveyNum[match(quest$Survey[quest_rows_i[1]],survs$Survey)],survs[hhs_geo,]$SurveyNum)
if(!is.na(ge_match)){
  g <- readRDS(downloads2[[ge_match]])
  gecols <- c("ALT_DEM","LATNUM","LONGNUM","ADM1NAME","DHSREGNA")
  matches <- match(gecols,names(g@data))
  missing <- which(is.na(match))
  matches[which(is.na(matches))] <- 1

  cluster_code <- grep("cluster number",quest$Description[quest_rows_i],ignore.case = TRUE)

  gedata <- g@data[match(results[cluster_code][[1]],g$DHSCLUST),matches]
  gedata <- as.data.frame.list(lapply(gedata,as.character),stringsAsFactors = FALSE)
} else {
  gedata <- matrix(nrow = dim(trim)[1],ncol=5)
}

list_res[[j]] <- bind_j

}

full_results <- dplyr::bind_rows(list_res)
write.table(full_results,"C:/Users/Oliver/GoogleDrive/AcademicWork/Imperial/PhD/WriteUps/HRP2_Burden/UNC_Share/newDHS_data.txt")
code_descriptons <- c("Survey","Survey Years Label",code_descriptons,"Cluster Altitude","Cluster Latitude","Cluster Longitude","Admin 1 region cluster exists in, taken from the DHS accompanying GEO files",
                      "DHS Region cluster exists in, taken from the DHS accompanying GEO files")

write.table(code_descriptons,"C:/Users/Oliver/GoogleDrive/AcademicWork/Imperial/PhD/WriteUps/HRP2_Burden/UNC_Share/newDHS_data_descriptions.txt",row.names = names(full_results))

## now work through questions and remove any collated questions, i.e. questions that have been repeated by flattening other surveys.
quest <- cli$survey_questions(survs[hhs_dta,],search_terms = "")
quest <- quest[-grep("$",quest$Code,fixed = T),]

## grab countrycode to name conversions
wh <- cli$dhs_api_request(api_endpoint = "countries")
cc_lookup <- cbind(wh$CountryName,wh$DHS_CountryCode)

# set up results and go
list_res <- list() ; length(list_res) <- survs[hhs_dta,]$SurveyYear %>% unique %>% length
names(list_res) <- paste0()

for(i in 1:length(unique(codout$SurveyPath))){

  message(i)

  # survey data
  file <- unique(codout$SurveyPath)[i]
  survey <- unique(codout$Survey)[i]
  survey_year_label <- survs$SurveyYearLabel[which(survs$Survey==survey)]
  r <- readRDS(file)

  # get their survey_code_descriptions
  if(i==3){
    code_descriptons <- r$Survey_Code_Descriptions$Description[match(codes,r$Survey_Code_Descriptions$Code)]
  }

  match <- match(codes,names(r$Survey))
  missing <- which(is.na(match))
  match[which(is.na(match))] <- 1
  results <- r$Survey[match]
  results[,missing] <- NA
  trim <- results[!(is.na(results$HML32)|is.na(results$HML35)),]

  trim$HV000 <- cc_lookup[match(substr(trim$HV000,start = 1,stop = 2),cc_lookup[,2]),1]

  #spatial data
  ge_match <- match(survs$SurveyNum[match(survey,survs$Survey)],survs$SurveyNum[hhs_geo])
  if(!is.na(ge_match)){
    g <- readRDS(downloads2[[ge_match]])
    gecols <- c("ALT_DEM","LATNUM","LONGNUM","ADM1NAME","DHSREGNA")
    match <- match(gecols,names(g@data))
    missing <- which(is.na(match))
    match[which(is.na(match))] <- 1
    gedata <- g@data[match(trim$HV001,g$DHSCLUST),match]
    gedata <- as.data.frame.list(lapply(gedata,as.character),stringsAsFactors = FALSE)
  } else {
    gedata <- matrix(nrow = dim(trim)[1],ncol=5)
  }


  bind <- cbind(rep(survey,dim(trim)[1]),
                rep(survey_year_label,dim(trim)[1]),
                trim,gedata)
  names(bind) <- c("SURVEY","SURVEY_YEARS","COUNTRY", "YEAR", "RDT", "MICRO", "AGE",
                   "SEX", "MONTH",  "WEIGHT","NET_AGE", "TREATED","BRAND","HAS_ITN","SLEPT_UNDER_ITN",
                   "REGION","RESIDENCE","CLUSTER","ALTITUDE", "CLUSTER_LAT", "CLUSTER_LONG", "ADM1_NAME", "DHS_REGION")


  list_res[[i]] <- bind

}

full_results <- dplyr::bind_rows(list_res)
write.table(full_results,"C:/Users/Oliver/GoogleDrive/AcademicWork/Imperial/PhD/WriteUps/HRP2_Burden/UNC_Share/newDHS_data.txt")
code_descriptons <- c("Survey","Survey Years Label",code_descriptons,"Cluster Altitude","Cluster Latitude","Cluster Longitude","Admin 1 region cluster exists in, taken from the DHS accompanying GEO files",
                      "DHS Region cluster exists in, taken from the DHS accompanying GEO files")

write.table(code_descriptons,"C:/Users/Oliver/GoogleDrive/AcademicWork/Imperial/PhD/WriteUps/HRP2_Burden/UNC_Share/newDHS_data_descriptions.txt",row.names = names(full_results))
