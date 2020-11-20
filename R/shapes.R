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
#'   Default = "sf", which uses \code{sf::st_read}. Currenlty, you can also
#'   specify "rgdal", which reads the file using rgdal::readOGR.
#'   To just return the file paths for the files use method = "zip".
#' @param quiet_parse Whether to download file quietly. Passed to
#'   [`download_file()`]. Default is `FALSE`.
#' @param quiet_parse Whether to read boundaries dataset quietly. Applies to
#'   `method = "sf"`. Default is `TRUE`.
#'
#' @details Downloads the spatial boundaries from the DHS spatial repository,
#'   which can be found at \url{https://spatialdata.dhsprogram.com/home/}.
#'
#' @return Returns either the spatial file as a "SpatialPolygonsDataFrame" or
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
#'  # using rgdal
#'  res <- download_boundaries(surveyNum = 471, countryId = "AF", method = "rgdal")
#'  }


download_boundaries <- function(surveyNum=NULL,
                                surveyId = NULL,
                                countryId = NULL,
                                method = "sf",
                                quiet_download = FALSE,
                                quiet_parse = TRUE){

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
  Sys.sleep(1)

  # download the shape file and read it in
  tf2 <- tempfile()
  file <- download.file(url, tf2, quiet = quiet_download)
  unzipped_files <- suppressWarnings(unzip(tf2, exdir = tempfile()))
  file <- grep("dbf", unzipped_files, value=TRUE)

  # how are we reading the dataset in
  methods <- c("rgdal", "sf")
  if (method %in% methods) {

    if(method == "rgdal") {
      res <- lapply(file, function(x) {
        layer <- strsplit(basename(x), ".", fixed = TRUE)[[1]][1]
        rgdal::readOGR(dsn = dirname(x), layer = layer)
      })
      names(res) <- vapply(file, rgdal::ogrListLayers, character(1))
    }

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

    return(unzipped_files)
  }

  return(res)

}
