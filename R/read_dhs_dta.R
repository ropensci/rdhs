#' Create dictionary from DHS .MAP codebook
#'
#' @param map A character vector containing .MAP file, e.g. from `readLines()`.
#' @param all_lower Logical indicating whether all value labels should be
#'   converted to lower case
#' @return A data frame containing metadata, principally variable labels and
#'   a vector of value labels.
#'
#' @details Currently hardcoded for 111 char width .MAP files, which covers the
#'   vast majority
#'   of DHS Phase V, VI, and VIII. To be extended in the future and perhaps add other useful options.
#'
#' @examples
#' mrdt_zip <- tempfile()
#' download.file(
#' paste0(
#' "https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm",
#' "?Filename=ZZMR61DT.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs"),
#' mrdt_zip, mode="wb"
#' )
#'
#' map <- rdhs::read_zipdata(mrdt_zip, "\\.MAP", readLines)
#' dct <- rdhs:::parse_map(map)
#'
parse_map <- function(map, all_lower=TRUE) {

  ## Parse code book table between horizontal rules
  ## Ignore first char of line which may be escape character
  hr_idx <- which(substr(map, 2, 5) == "----")

  header <- map[seq.int(hr_idx[length(hr_idx) - 3] + 1,
                        hr_idx[length(hr_idx) - 2] - 1)]
  dat <- map[seq.int(hr_idx[length(hr_idx) - 2] + 1,
                     hr_idx[length(hr_idx) - 1] - 1)]

  ## Identify variable lines based on flush left variable name
  var_idx <- which(substr(dat, 1, 2) != "  ")
  var <- dat[var_idx]

  ## Identify column breaks based on white space in header.
  ## Then check that white spaces also occur in variable rows
  ## (>0.7 arbitrarily) to exclude white spaces
  ## in header variable names.

  hspaces <- vapply(
    strsplit(header, NULL), "==", logical(nchar(header[1])), " "
    )

  hspaces <- which(apply(hspaces, 1, all))
  hspaces <- setdiff(hspaces + 1, hspaces) - 1

  Sys.setlocale("LC_ALL", "C") ## !!! TODO
  vspaces <- lapply(strsplit(var, NULL), "==", " ")
  vspaces <- vapply(
    vspaces, "[", logical(length(vspaces[[1]])), seq_along(vspaces[[1]])
  )
  vspaces <- which(apply(vspaces, 1, mean) > 0.7)

  idx1 <- c(intersect(hspaces, vspaces), max(nchar(c(header, var))))
  idx0 <- idx1 - diff(c(0, idx1)) + 1

  var <- Map(substr, list(x = var), start = idx0, stop = idx1)
  var <- lapply(var, gsub, pattern = "^\\s+", replacement = "")
  var <- lapply(var, gsub, pattern = "\\s+$", replacement = "")
  var <- data.frame(var, stringsAsFactors = FALSE)

  header <- Map(substr, list(x = header), start = idx0, stop = idx1)
  header <- lapply(header, gsub, pattern = "^\\s+", replacement = "")
  header <- lapply(header, gsub, pattern = "\\s+$", replacement = "")
  header <- gsub(
    "^\\s", "", vapply(header, paste, character(1), collapse = " ")
    )

  names(var) <- tolower(header)
  var$name <- tolower(gsub("\\s+.*", "", var[["item name"]]))
  var$label <- tolower(var[["item label"]])
  var$start <- as.integer(var$start)
  var$len <- as.integer(var$len)
  var$occ <- as.integer(var$occ)

  ## Parse value labels

  .parse_labels <- function(x, is_alpha=FALSE, all_lower=TRUE) {
    maxch <- max(nchar(x))
    x <- gsub("^(\\s+?)\\(m\\)", "\\1   ", x) # replance "(m)" with "   "
    x <- gsub("^(\\s+?)\\(na\\)", "\\1    ", x) # replance "(na)" with "    "
    charid <- do.call(c, lapply(lapply(strsplit(x, NULL), "!=", " "), which))
    idx <- seq_len(maxch)[-charid]
    idx <- sort(setdiff(idx + 1, idx))[1:2]
    x <- Map(substr, x = list(x), start = idx, stop = c(idx[-1] - 1, maxch))
    value <- x[[length(x) - 1]]
    if (is_alpha) {
      value <- gsub("^\\s+", "", gsub("\\s+$", "", value))
      value[nchar(value) == 0] <- NA
    } else {
      value <- suppressWarnings(as.integer(value))
    }
    label <- gsub("^\\s+", "", gsub("\\s+$", "", x[[length(x)]]))
    if (all_lower) {
      label <- tolower(label)
    }
    names(value) <- label
    value[!is.na(value) & nchar(names(value)) > 0]
  }


  nval <- diff(c(var_idx, length(dat))) - 1
  hasval <- nval > 0
  val <- Map(function(i, n) dat[i + 1:n], i = var_idx[hasval], nval[hasval])
  names(val) <- var$name[hasval]

  var$labels <- list(NULL)
  var$labels[hasval] <- Map(.parse_labels, x = val,
                            is_alpha = var[["data type"]][hasval] == "AN",
                            all_lower = all_lower)

  var$datatype <- c("Numeric", "Alpha")[match(var[["data type"]], c("N", "AN"))]

  ## Expand dictionary for repeated occurrence variables
  occ_dct <- rep(var$occ, var$occ)
  occ_i <- unlist(lapply(var$occ, seq_len))
  sfx <- lapply(var$occ, function(x) {
    formatC(seq_len(x), width = nchar(x), flag = "0")
  })

  name_dct <- paste0(
    rep(var$name, var$occ),
    ifelse(occ_dct > 1, paste0("_", unlist(sfx)), "")
  )
  len_dct <- rep(var$len, var$occ)

  dct <- data.frame(
    name = name_dct,
    label = rep(var$label, var$occ),
    len = len_dct,
    start = rep(var$start, var$occ) + len_dct * (occ_i - 1L),
    occurences = occ_dct,
    datatype = rep(var$datatype, var$occ),
    stringsAsFactors = FALSE
  )
  dct$labels <- rep(var$labels, var$occ)
  rownames(dct) <- dct$name

  return(dct)
}

#' Read DHS Stata data set
#'
#' This function reads a DHS recode dataset from the zipped Stata dataset.
#' By default (`mode = "haven"`), it reads in the stata data set using
#' \code{\link[haven]{read_dta}}
#'
#' @param zfile Path to `.zip` file containing Stata dataset, usually ending
#'   in filename `XXXXXXDT.zip`
#' @param mode Read mode for Stata `.dta` file. Defaults to "haven", see
#'   'Details' for other options.
#' @param all_lower Logical indicating whether all value labels should be lower
#'   case. Default to `TRUE`.
#' @param ... Other arguments to be passed to \code{\link{read_zipdata}}.
#'   Here this will be arguments to pass to either \code{\link[haven]{read_dta}}
#'   or \code{\link[foreign]{read.dta}} depending on the mode provided
#' @return A data frame. If mode = 'map', value labels for each variable are
#'   stored as the `labelled` class from `haven`.
#' @export
#'
#' @details
#' The default `mode="haven"` uses  \code{\link[haven]{read_dta}}
#' to read in the dataset. We have chosen this option as it is more consistent
#' with respect to variable labels and descriptions than others.
#' The other options either use use \code{\link[foreign]{read.dta}}
#' or they use the `.MAP` dictionary file provided with the DHS Stata datasets
#' to reconstruct the variable labels and value labels. In this case, value
#' labels are stored are stored using the the `labelled` class from `haven`.
#' See `?haven::labelled` for more information. Variable labels are stored in
#' the "label" attribute of each variable, the same as `haven::read_dta()`.
#'
#' Currently, `mode="map"` is only implemented for 111
#' character fixed-width .MAP files, which comprises
#' the vast majority of recode data files from DHS Phases V,
#' VI, and VII and some from Phase IV. Parsers
#' for other .MAP formats will be added in future.
#'
#' Other available modes read labels from the Stata dataset
#' with various options available in R:
#'
#' * `mode="map"` uses the `.MAP` dictionary file provided with the DHS Stata
#' datasets to reconstruct the variable labels and value labels. In this case,
#' value labels are stored are stored using the the `labelled` class
#' from `haven`. See `?haven::labelled` for more information. Variable labels
#' are stored in the "label" attribute of each variable, the same as
#' `haven::read_dta()`.
#'
#' * `mode="haven"`: use `haven::read_dta()` to read dataset.
#' This option retains the native value codings
#' with value labels affixed with the 'labelled' class.
#'
#' * `mode="foreign"`: use `foreign::read.dta()`,
#' with default options convert.factors=TRUE to add
#' variable labels. Note that variable labels will
#' not be added if labels are not present for all
#' values, but variable labels are available via the "val.labels" attribute.
#'
#' * `mode="foreignNA"`: use `foreign::read.dta(..., convert.factors=NA)`,
#' which converts any values without labels to 'NA'. This risks data loss
#' if labelling is incomplete in Stata datasets.
#'
#' * `mode="raw"`: use `foreign::read.dta(..., convert.factors=FALSE)`,
#' which simply loads underlying value coding. Variable labels and value
#' lables are still available through dataset attributes (see examples).
#'
#' @seealso \code{\link[foreign]{read.dta}}, \code{\link[haven]{labelled}},
#'   \code{\link[haven]{read_dta}}.
#'
#' For more information on the DHS filetypes and contents of
#' distributed dataset .ZIP files, see
#' \url{https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10334}.
#'
#' @examples
#' mrdt_zip <- tempfile()
#' download.file(
#' paste0(
#' "https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?",
#' "Filename=ZZMR61DT.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs"),
#' mrdt_zip, mode="wb"
#' )
#'
#' mr <- rdhs::read_dhs_dta(mrdt_zip,mode="map")
#' attr(mr$mv213, "label")
#' class(mr$mv213)
#' head(mr$mv213)
#' table(mr$mv213)
#' table(haven::as_factor(mr$mv213))
#'
#' ## If Stata file codebook is complete, `mode="map"` and `"haven"`
#' ## should be the same.
#' mr_hav <- rdhs::read_dhs_dta(mrdt_zip, mode="haven")
#' attr(mr_hav$mv213, "label")
#' class(mr_hav$mv213)
#' head(mr_hav$mv213)  # "9=missing" omitted from .dta codebook
#' table(mr_hav$mv213)
#' table(haven::as_factor(mr_hav$mv213))
#'
#' ## Parsing codebook when using foreign::read.dta()
#' # foreign issues with duplicated factors
#' # Specifying foreignNA can help but often will not as below.
#' # Thus we would recommend either using mode = "haven" or mode = "raw"
#' \dontrun{
#' mr_for <- rdhs::read_dhs_dta(mrdt_zip, mode="foreign")
#' mr_for <- rdhs::read_dhs_dta(mrdt_zip, mode = "foreignNA")
#' }
#' ## Don't convert factors
#' mr_raw <- rdhs::read_dhs_dta(mrdt_zip, mode="raw")
#' table(mr_raw$mv213)
#'
read_dhs_dta <- function(zfile, mode="haven", all_lower=TRUE, ...) {
  if (!mode %in% c("map", "haven", "foreign", "raw", "foreignNA")) {
    stop(paste0("'", mode, "' is not a recognized read 'mode'"))
  }

  if (mode == "haven") {
    dat <- as.data.frame(read_zipdata(zfile, "\\.dta$", haven::read_dta, ...))
  }
  if (mode == "foreign") {
    dat <- read_zipdata(zfile, "\\.dta$", foreign::read.dta, ...)
  }
  if (mode == "foreignNA") {
    dat <- read_zipdata(
      zfile = zfile, pattern = "\\.dta$",
      readfn = foreign::read.dta, convert.factors = NA, ...
    )
  }
  if (mode == "raw") {
    dat <- read_zipdata(zfile, "\\.dta$", foreign::read.dta,
      convert.factors = FALSE, ...
    )
  }

  if (mode == "map") {
    dat <- read_zipdata(zfile, "\\.dta$", foreign::read.dta,
                        convert.factors = FALSE, ...)
    map <- read_zipdata(zfile, "\\.MAP$", readLines)
    dct <- parse_map(map, all_lower)
    dat[dct$name] <- Map("attr<-", dat[dct$name], "label", dct$label)
    haslbl <- unlist(lapply(dct$labels, length)) > 0
    dat[dct$name[haslbl]] <- Map(haven::labelled, dat[dct$name[haslbl]],
                                 dct$labels[haslbl])
  }

  # lower the names if needed
  if (all_lower) names(dat) <- tolower(names(dat))
  return(dat)
}
