#' Create a list of survey responses extracted using output of
#' \code{R6_client_dhs$public_methods$survey_questions}
#'
#' @title DHS survey questions extracted from datasets
#' @param questions Output of
#'   \code{R6_client_dhs$public_methods$survey_questions}
#' @param available_datasets Datasets that could be available.
#'   Output of \code{R6_client_dhs$public_methods$available_datasets}
#' @param geo_surveys Geographic Data Survey file paths.
#' @param add_geo Boolean detailing if geographic datasets should be added.
#'
#'
#' @return Returns `data.frame` with variables corresponding to
#'   the requested variables in the questions object. Will also have
#'   geographic data related columns if `add_geo=TRUE` is set.
#'   Lastly a SurveyId variable will also be appended corresponding to
#'   \code{\link{dhs_datasets}}$SurveyId
#'
#'
extraction <- function(questions, available_datasets,
                       geo_surveys, add_geo=FALSE) {

  # dats is shorter than available_datasets..
  dats <- available_datasets

  # set up results and go
  filenames <- unique(questions$dataset_filename)
  list_res <- list()
  length(list_res) <- length(filenames)
  names(list_res) <- filenames

  for (i in seq_len(length(list_res))) {
    message(paste0("Starting Survey ", i, " out of ",
                   length(list_res), " surveys:", filenames[i]))

    dataset_i <- unique(
      questions$dataset_path[questions$dataset_filename == filenames[i]]
    )

    # survey data
    r <- readRDS(dataset_i)

    # fetch codes that relate to this dataset
    quest_rows_i <- which(questions$dataset_path == dataset_i)
    codes <- questions$variable[quest_rows_i]
    matched_rows <- match(codes, names(r))
    missing <- which(is.na(matched_rows))

    # then create the space for missing data/codes - this shouldn't happen as
    # we have checked the codes before, but just in case
    matched_rows[which(is.na(matched_rows))] <- 1
    results <- r[matched_rows]
    results[, missing] <- NA

    # add cluster qustions if add_geo
    if (add_geo) {

      # where are the geo datasets
      geos <- which(dats$FileType == "Geographic Data")

      # add the cluster info
      cluster_q <- grep("^cluster$|^cluster number$",
                        vapply(r, attr, character(1), "label", exact = TRUE),
                        ignore.case = TRUE)
      results$CLUSTER <- r[[cluster_q]]

      # where is the geo survey we want
      file_m <- match(questions$dataset_filename[quest_rows_i[1]], dats$Survey)
      geo_survey_row <- match(dats$SurveyNum[file_m], dats[geos, ]$SurveyNum)

      if (!is.na(geo_survey_row)) {

        ge_match <- dats[geos, ]$Survey[geo_survey_row]
        geo_surveys_match <- which(names(geo_surveys) == ge_match)
        if (length(geo_surveys_match) == 1) {
          g <- readRDS(geo_surveys[[geo_surveys_match]])
          gecols <- c("ALT_DEM", "LATNUM", "LONGNUM", "ADM1NAME", "DHSREGNA")
          matched_rows <- match(gecols, names(g))
          missing <- which(is.na(matched_rows))
          matched_rows[which(is.na(matched_rows))] <- 1

          gedata <- g[match(results$CLUSTER, g$DHSCLUST), matched_rows]
          gedata <- as.data.frame.list(lapply(gedata, as.character),
                                       stringsAsFactors = FALSE)
          gedata <- type_convert_list_to_df(gedata)
        }
      } else {
        l <- dim(results)[1]
        gedata <- list(
          "ALT_DEM" = rep(NA, l), "LATNUM" = rep(NA, l), "LONGNUM" = rep(NA, l),
          "ADM1NAME" = rep(NA, l), "DHSREGNA" = rep(NA, l)
        ) %>% as.data.frame.list()
      }

      results_full <- cbind.data.frame(results, gedata)
    } else {
      results_full <- results
    }

    results_full$SurveyId <- unique(questions$survey_id[quest_rows_i])
    list_res[[i]] <- results_full
  }

  return(list_res)
}

# Function to change dataset variables to the descriptions.
# Still undecided if should be exposed.
extract_codes_to_descriptions <- function(extraction, questions) {
  for (i in seq_len(length(extraction))) {
    survey <- names(extraction)[i]
    surv_pos <- which(questions$dataset_filename == survey)
    code_match <- match(names(extraction[[i]]), questions$variable[surv_pos])
    valid <- which(!is.na(code_match))

    if (length(valid) > 1) {
      names(extraction[[i]])[valid] <- questions$description[surv_pos][valid]
    }
  }


  return(extraction)
}
