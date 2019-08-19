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
#'   Default = "rdgal", which reads the file using rgdal::readOGR. At current
#'   you can also specify sf, which uses \code{sf::st_read}.
#'   To just return the file paths for the files use method = "zip".
#'
#' @details Downloads the spatial boundaries from the DHS spatial repository,
#'   which can be found at \url{https://spatialdata.dhsprogram.com/home/}.
#'
#' @return Returns either the spatial file as a "SpatialPolygonsDataFrame" or
#'   a vector of the file paths of where the boundary was downloaded to.
#'
#'  @examples
#'  \dontrun{
#'
#'  # using the surveyNum
#'  res <- download_boundaries(surveyNum = 471, countryId = "AF")
#'
#'  # using the surveyId and no countryID
#'  res <- download_boundaries(surveyId = "AF2010OTH")
#'
#'  # using sf
#'  res <- download_boundaries(surveyNum = 471, countryId = "AF", method = "sf")
#'
#'  }
#'

download_boundaries <- function(surveyNum=NULL,
                                surveyId = NULL,
                                countryId = NULL,
                                method = "rgdal"){

  # helper funcs
  build_final_url <- function(jobId) {

    url <- "https://gis.dhsprogram.com/arcgis/rest/directories/arcgisjobs/tools/downloadsubnationaldata_gpserver/"
    date <- as.Date(Sys.time())
    url <- paste0(url, jobId,"/scratch/sdr_subnational_boundaries_", date,".zip")
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
  sp_url <- "https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId"

  values <- list(
    survey_ids = surveyNum,
    spatial_format = "shp",
    f = "json"
  )

  # fetch jobID
  z <- httr::GET(
    httr::modify_url(alt_url,query = values)
  )

  tf <- tempfile()
  y <- writeBin(z$content,con = tf)
  h <- jsonlite::fromJSON(readLines(tf,warn = FALSE))
  url <- build_final_url(h$jobId)

  # pause for a second for the job id created to appear on their server
  # i.e. the code only works with this...
  Sys.sleep(1)

  # download the shape file and read it in
  tf2 <- tempfile()
  file <- download.file(url, tf2)
  unzipped_files <- suppressWarnings(unzip(tf2, exdir = tempfile()))
  file <- grep("dbf",unzipped_files,value=TRUE)

  # how are we reading the dataset in
  methods <- c("rgdal", "sf")
  if (method %in% methods) {

    if(method == "rgdal") {
      res <- lapply(file, function(x) {
        rgdal::readOGR(dsn = dirname(x),
                                   layer = strsplit(basename(x),
                                                    ".", fixed = TRUE)[[1]][1])
      })
      names(res) <- vapply(file, rgdal::ogrListLayers, character(1))
    }

    # here if we want to add more read in options
    if(method == "sf") {
      res <- lapply(file, sf::st_read)
      names(res) <- vapply(file,function(x) {
        sf::st_layers(x)$name
        }, character(1))
    }

  } else {
    message("Provided method not found. Options are: \n",
            paste(methods,collapse=" "),
            "\nReturning zip files.")
    return(unzipped_files)
  }

  return(res)

}

#' @noRD
match_clean <- function(a,b){

  if (any(!requireNamespace("stringi", quietly = TRUE),
          !requireNamespace("stringdist", quietly = TRUE))) {

    stop("Package \"stringi\" and \"stringdist\" needed for this
         function to work. Please install it.")

  } else {

  a <- gsub("[[:punct:][:space:]]","",tolower(stringi::stri_trans_general(a, "latin-ascii")))
  b <- gsub("[[:punct:][:space:]]","",tolower(stringi::stri_trans_general(b, "latin-ascii")))
  ret <- match(a,b)

  if(sum(is.na(ret)>0)){
    dists <- stringdist::seq_distmatrix(lapply(a,utf8ToInt),lapply(b,utf8ToInt))
    ret[is.na(ret)] <- apply(dists[which(is.na(ret)),,drop=FALSE],1,which.min)
    print(unique(cbind(a,b[ret])))
  }

  return(ret)

  }
}
