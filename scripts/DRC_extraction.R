
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
hhs_dta <- which(survs$FileFormat == "SPSS dataset (.sav)" & survs$DHS_CountryCode %in% c("CD") & !survs$FileType %in% c( "Couples' Recode","Household Recode",""))
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
quested <- quest[grep("fever|malaria|net",quest$Description),]


extraction <- function(questions,available_surveys,geo_surveys,add_geo=TRUE){

  survs <- available_surveys
  survs$Survey <- strsplit(survs$FileName,".",fixed=T) %>% lapply(function(x) x[1]) %>% unlist

  # set up results and go
  list_res <- list() ; length(list_res) <- length(unique(paste0(quest$CountryCode,quest$SurveyYear)))
  names(list_res) <- unique(paste0(quest$CountryCode,quest$SurveyYear))

  code_years <- unique(paste0(quest$CountryCode,quest$SurveyYear))
  quest$CodeYears <- paste0(quest$CountryCode,quest$SurveyYear)


  for(i in 1:length(list_res)){

    message(paste0("Starting Survey Year ",i," out of ",length(list_res)," surveys"))
    surveys_j <- unique(quest$SurveyPath[quest$CodeYears==code_years[i]])
    list_res[[i]] <- list()
    length(list_res[[i]]) <- length(surveys_j)
    names(list_res[[i]]) <- sapply(surveys_j,function(x) quest$Survey[quest$SurveyPath==x][1])

    for(j in 1:length(surveys_j)){

      message("Extracting ",surveys_j[j], " (",j,"/",length(surveys_j),")")

      # survey data
      file <- surveys_j[j]
      r <- readRDS(file)

      # fetch codes that relate to this survey
      quest_rows_j <- which(quest$SurveyPath==surveys_j[j])
      codes <- quest$Code[quest_rows_j]
      matched_rows <- match(codes,names(r$Survey))
      missing <- which(is.na(matched_rows))

      # then create the space for missing data/codes - this shouldn't happen as
      # we have checked the codes before, but just in case
      matched_rows[which(is.na(matched_rows))] <- 1
      results <- r$Survey[matched_rows]
      results[,missing] <- NA

      # add cluster qustions if add_geo
      if(add_geo){
        cluster_quest <- grep("cluster$|cluster number$",r$Survey_Code_Descriptions$Description,ignore.case=TRUE)
        results$CLUSTER <- r$Survey[[cluster_quest]]
        ge_match <- match(survs$SurveyNum[match(quest$Survey[quest_rows_j[1]],survs$Survey)],
                          survs[survs$FileType=="Geographic Data",]$SurveyNum)
        if(!is.na(ge_match)){
          g <- readRDS(geo_surveys[[ge_match]])
          gecols <- c("ALT_DEM","LATNUM","LONGNUM","ADM1NAME","DHSREGNA")
          matched_rows <- match(gecols,names(g@data))
          missing <- which(is.na(matched_rows))
          matched_rows[which(is.na(matched_rows))] <- 1

          gedata <- g@data[match(results$CLUSTER,g$DHSCLUST),matched_rows]
          gedata <- as.data.frame.list(lapply(gedata,as.character),stringsAsFactors = FALSE)
        } else {
          gedata <- matrix(data = NA,nrow = dim(trim)[1],ncol=5)
        }

        results <- append(results,gedata)

      }

      list_res[[i]][[j]] <- results

    }


  }

  return(list_res)

}

