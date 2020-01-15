#' DHS model datasets
#'
#' The model datasets from the DHS website in a `data.frame` that is analogous
#' to those returned by `get_available_datasets()`
#'
#' @docType data
#' @usage data(model_datasets)
#'
#' @format A dataframe of 36 observations of 14 variables:
#'
#' \code{model_datasets}: A dataframe of model datasets
#' \itemize{
#'       \item{"FileFormat"}
#'       \item{"FileSize"}
#'       \item{"DatasetType"}
#'       \item{"SurveyNum"}
#'       \item{"SurveyId"}
#'       \item{"FileType"}
#'       \item{"FileDateLastModified"}
#'       \item{"SurveyYearLabel"}
#'       \item{"SurveyType"}
#'       \item{"SurveyYear"}
#'       \item{"DHS_CountryCode"}
#'       \item{"FileName"}
#'       \item{"CountryName"}
#'       \item{"URLS"}
#'       }
#'
#'
#' @rdname model_datasets
#' @aliases model_datasets
#'
#'
"model_datasets"


#' DHS GPS Data Format
#'
#' Data frame to describe the data encoded in DHS GPS files
#'
#' @docType data
#' @usage data(dhs_gps_data_format)
#'
#' @format A dataframe of 20 observations of 3 variables:
#'
#' \code{dhs_gps_data_format}: A dataframe of GPS data descriptions.
#' \itemize{
#'       \item{"Name"}
#'       \item{"Type"}
#'       \item{"Description"}
#'       }
#'
#'
#' @rdname dhs_gps_data_format
#' @aliases dhs_gps_data_format
#'
#'
"dhs_gps_data_format"
