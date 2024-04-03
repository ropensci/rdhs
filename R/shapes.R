#' Download Spatial Boundaries
#'
#' @title DHS Spatial Boundaries
#' @param surveyNum Numeric for the survey number to be downloaded. Values for
#'   surveyNum can be found in the datasets or surveys endpoints in the DHS API
#'   that can be accessed using \code{\link{dhs_datasets}} and
#'   \code{\link{dhs_surveys}}. Default is NULL, which will cause the SurveyId
#'   to be used to find the survey.
#' @param surveyId Numeric for the survey ID to be downloaded. Values for
#'   surveyId can be found in the datasets or surveys endpoints in the DHS API
#'   that can be accessed using \code{\link{dhs_datasets}} and
#'   \code{\link{dhs_surveys}}. Default is NULL, which will cause the SurveyNum
#'   to be used to find the survey.
#' @param countryId 2-letter DHS country code for the country of the survey
#'   being downloaded. Default = NULL, which will cause the countrycode to be
#'   looked up from the API.
#' @param method Character for how the downloaded shape file is read in.
#'   Default = "sf", which uses \code{sf::st_read}. Currently, this is the only
#'   option available(`rgdal` used to be available) but for development reasons
#'   this will be left as a parameter option for possible future alternatives
#'   to read in spatial files.
#'   To just return the file paths for the files use method = "zip".
#' @param quiet_download Whether to download file quietly. Passed to
#'   [`download_file()`]. Default is `FALSE`.
#' @param quiet_parse Whether to read boundaries dataset quietly. Applies to
#'   `method = "sf"`. Default is `TRUE`.
#' @param server_sleep Numeric for length of sleep prior to downloading file
#'   from their survey. Default 5 seconds.
#' @param client If the request should be cached, then provide a client
#'   object created by \code{\link{client_dhs}}. Default = `NULL`, which will
#'   search for a client to use
#'
#' @details Downloads the spatial boundaries from the DHS spatial repository,
#'   which can be found at \url{https://spatialdata.dhsprogram.com/home/}.
#'
#' @return Returns either the spatial file as a `sf` (see [sf::sf]) object, or
#'   a vector of the file paths of where the boundary was downloaded to.
#' @export
#'
#' @examples
#' \dontrun{
#'  # using the surveyNum
#'  res <- download_boundaries(surveyNum = 471, countryId = "AF")
#'
#'  # using the surveyId and no countryID
#'  res <- download_boundaries(surveyId = "AF2010OTH")
#'
#'  }


download_boundaries <- function(surveyNum=NULL,
                                surveyId = NULL,
                                countryId = NULL,
                                method = "sf",
                                quiet_download = FALSE,
                                quiet_parse = TRUE,
                                server_sleep = 5,
                                client = NULL){

  # helper funcs
  build_final_url <- function(jobId) {

    url <- paste0("https://gis.dhsprogram.com/arcgis/rest/directories/",
                  "arcgisjobs/tools/downloadsubnationaldata_gpserver/")
    date <- as.Date(Sys.time())
    url <- paste0(url, jobId,"/scratch/sdr_subnational_boundaries_", date, ".zip")
    return(url)

  }

  # error checking
  # ----------------------------------------------------------------------------
  assert_null_and_func(surveyNum, assert_scalar_numeric)
  assert_null_and_func(surveyId, assert_scalar_character)
  assert_null_and_func(countryId, assert_scalar_character)
  assert_scalar_numeric(server_sleep)

  # handle arguments
  # ----------------------------------------------------------------------------
  if(is.null(surveyNum) && is.null(surveyId)) {
    stop("Both surveyNum and surveyId can'tbe NULL")
  }

  # create surveyNum if needed
  if(is.null(surveyNum)) {
    dats <- dhs_datasets()
    surveyNum <- dats$SurveyNum[match(surveyId, dats$SurveyId)]
  }

  # create countryid if needed
  if (is.null(countryId)) {
    dats <- dhs_datasets()
    countryId <- dats$DHS_CountryCode[match(surveyNum, dats$SurveyNum)]
  }

  # if no client was provided we'll look for
  # the package environment client by default
  if (is.null(client)) {
    client <-  check_for_client()
  }

  # create db key
  key <- paste0(surveyNum, "_", method)

  # first check against cache
  out <- tryCatch(
    client$.__enclos_env__$private$storr$get(key, "spatial_boundaries"),
    KeyError = function(e) {
      NULL
    }
  )

  # check out agianst cache, if fine then return just that
  if (!is.null(out)) {
    return(out)
  } else {

    # build our url
    # ----------------------------------------------------------------------------

    # create url from surveyNum
    alt_url <- paste0("https://gis.dhsprogram.com/arcgis/rest/services/Tools/",
                      "DownloadSubnationalData/GPServer/",
                      "downloadSubNationalBoundaries/submitJob")

    values <- list(
      survey_ids = surveyNum,
      spatial_format = "shp",
      f = "json"
    )

    # fetch jobID
    z <- httr::GET(
      httr::modify_url(alt_url, query = values)
    )

    tf <- tempfile()
    y <- writeBin(z$content, con = tf)
    h <- jsonlite::fromJSON(brio::read_lines(tf))
    url <- build_final_url(h$jobId)

    # pause for a second for the job id created to appear on their server
    # i.e. the code only works with this...
    Sys.sleep(server_sleep)

    # download the shape file and read it in
    tf2 <- tempfile()
    file <- download.file(url, tf2, quiet = quiet_download)
    unzipped_files <- suppressWarnings(unzip(tf2, exdir = tempfile()))
    file <- grep("dbf", unzipped_files, value=TRUE)

    # how are we reading the dataset in
    methods <- c("sf")
    if (method %in% methods) {

      # here if we want to add more read in options
      if(method == "sf") {
        res <- lapply(file, sf::st_read, quiet = quiet_parse)
        names(res) <- vapply(file,
                             function(x) { sf::st_layers(x)$name },
                             character(1))
      }

    } else {
      message("Provided method not found. Options are: \n",
              paste(methods,collapse=" "),
              "\nReturning zip files.")
      res <- unzipped_files
    }

    ## then cache the resp and return the parsed resp
    client$.__enclos_env__$private$storr$set(key, res, "spatial_boundaries")

    return(res)

  }

}
