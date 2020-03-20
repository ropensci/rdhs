#' \pkg{rdhs} DHS database through R
#'
#' Provides a client for (1) querying the DHS API for survey indicators
#' and metadata, (2) identifying surveys and datasets for analysis,
#' (3) downloading survey datasets from the DHS website, (4) loading
#' datasets and associate metadata into R, and (5) extracting variables
#' and combining datasets for pooled analysis.
#'
#' @docType package
#' @name rdhs
#'
#' @importFrom stats setNames
#' @importFrom utils tail type.convert packageVersion unzip str capture.output
#' @importFrom utils read.csv download.file
#' @importFrom rappdirs user_cache_dir
#' @importFrom R6 R6Class
#' @importFrom storr storr_rds
#'
"_PACKAGE"

globalVariables(c("x","model_datasets"))
