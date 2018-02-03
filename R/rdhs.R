#' \pkg{rdhs} DHS database through R
#'
#' Provides a API client for the DHS API that allows DHS survey indicators
#' to be queried. This allows desired indicators to be easily found and passed onto
#' subsequent functionality within rdhs for downloading datasets and conducting basic
#' data munging tasks.
#'
#' @docType package
#' @name rdhs
#'
#'
#' @importFrom stats setNames
#' @importFrom utils tail
"_PACKAGE"

globalVariables(c("x"))
