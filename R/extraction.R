##' Create a list of survey responses extracted using output of \code{R6_dhs_client$public_methods$survey_questions}
##'
##' @title DHS survey questions extracted from surveys
##' @param questions Output of \code{R6_dhs_client$public_methods$survey_questions}
##' @param available_surveys Surveys that could be available. Output of \code{R6_dhs_client$public_methods$available_surveys}
##' @param geo_surveys Geographic Data Survey file paths.
##' @param add_geo Boolean detailing if geographic datasets should be added.
##'
##'
##' @return Returns \code{"list"} of length equal to the number of unique country year combinations within questions.
##' Each list will then contain further lsts of the surveys collected in that country at that time.
##'
##'
extraction <- function(questions,available_surveys,geo_surveys,add_geo=TRUE){

  survs <- available_surveys

  # set up results and go
  list_res <- list() ; length(list_res) <- length(unique(paste0(questions$CountryCode,questions$SurveyYear)))
  names(list_res) <- unique(paste0(questions$CountryCode,questions$SurveyYear))

  code_years <- unique(paste0(questions$CountryCode,questions$SurveyYear))
  questions$CodeYears <- paste0(questions$CountryCode,questions$SurveyYear)


  for(i in 1:length(list_res)){

    message(paste0("Starting Survey Year ",i," out of ",length(list_res)," surveys"))
    surveys_j <- unique(questions$SurveyPath[questions$CodeYears==code_years[i]])
    list_res[[i]] <- list()
    length(list_res[[i]]) <- length(surveys_j)
    names(list_res[[i]]) <- sapply(surveys_j,function(x) questions$Survey[questions$SurveyPath==x][1])

    for(j in 1:length(surveys_j)){

      message("Extracting ",surveys_j[j], " (",j,"/",length(surveys_j),")")

      # survey data
      file <- surveys_j[j]
      r <- readRDS(file)

      # fetch codes that relate to this survey
      quest_rows_j <- which(questions$SurveyPath==surveys_j[j])
      codes <- questions$Code[quest_rows_j]
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
        geo_survey_row <- match(survs$SurveyNum[match(questions$Survey[quest_rows_j[1]],survs$Survey)],
                                survs[survs$FileType=="Geographic Data",]$SurveyNum)
        if(!is.na(geo_survey_row)){
          ge_match <- survs[survs$FileType=="Geographic Data",]$Survey[geo_survey_row]
          geo_surveys_match <- which(names(geo_surveys)==ge_match)
          if(length(geo_surveys_match)==1){
            g <- readRDS(geo_surveys[[geo_surveys_match]])
            gecols <- c("ALT_DEM","LATNUM","LONGNUM","ADM1NAME","DHSREGNA")
            matched_rows <- match(gecols,names(g@data))
            missing <- which(is.na(matched_rows))
            matched_rows[which(is.na(matched_rows))] <- 1

            gedata <- g@data[match(results$CLUSTER,g$DHSCLUST),matched_rows]
            gedata <- as.data.frame.list(lapply(gedata,as.character),stringsAsFactors = FALSE)
          }
        } else {
          l <- dim(results)[1]
          gedata <- list("ALT_DEM"=rep(NA,l),"LATNUM"=rep(NA,l),"LONGNUM"=rep(NA,l),
                         "ADM1NAME"=rep(NA,l),"DHSREGNA"=rep(NA,l))
        }

        results_full <- append(results,gedata)

      }

      list_res[[i]][[j]] <- as.data.frame.list(lapply(results_full,type.convert,as.is=TRUE),stringsAsFactors = FALSE)

    }


  }

  return(list_res)

}

