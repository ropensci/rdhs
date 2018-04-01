#' Parse dataset metadata
#'
#' Parse fixed-width file metadata 
#' 
#' @inheritParams all_lower
#' @return data.frame with metadata for parsing fixed-width flat file
#'
#' @examples
#' mrfl_zip <- tempfile()
#' download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZMR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", mrfl_zip)
#'
#' dcf <- read_zipdata(mrfl_zip, "\\.DCF", readLines)
#' dct <- parse_dcf(dcf)
#'
#' sps <- read_zipdata(mrfl_zip, "\\.SPS", readLines)
#' dct <- parse_sps(sps)
#'
#' @name parse_meta
NULL

#' @rdname parse_meta
#' @param dcf .DCF file as character vector (e.g. from readLines)
#' @export
parse_dcf <- function(dcf, all_lower=TRUE){

  Sys.setlocale('LC_ALL','C')   ## !!! TODO
  
  item.start <- which(dcf == "[Item]")
  item.len <- diff(c(item.start, length(dcf)))
  item.idx <- Map("+", item.start-1L, lapply(item.len, seq_len))
  
  items <- lapply(item.idx, function(idx) paste(dcf[idx], collapse="\n"))

  dcf <- data.frame(name       = tolower(sub(".*?\nName=([^\n]*)\n.*", "\\1", items)),
                    label      = sub(".*?\nLabel=([^\n]*)\n.*", "\\1", items),
                    start      = as.integer(sub(".*?\nStart=([^\n]*)\n.*", "\\1", items)),
                    len        = as.integer(sub(".*?\nLen=([^\n]*)\n.*", "\\1", items)),
                    datatype   = "Numeric",
                    occurences = 1,
                    stringsAsFactors = FALSE)
  
  has_datatype <- grep(".*?\nDataType", items)
  dcf$datatype[has_datatype] <- sub(".*?\nDataType=([^\n]*)\n.*", "\\1", items[has_datatype])
  
  has_occ <- grep(".*?\nOccurrences", items)
  dcf$occurences[has_occ] <- as.integer(sub(".*?\nOccurrences=([^\n]*)\n.*", "\\1", items[has_occ]))

  ## add labels
  hasvs <- grepl("\\[ValueSet\\]", items)
  vs <- gsub(".*?(\\[ValueSet\\].*)", "\\1", items[hasvs])
  vs <- strsplit(vs, "\n")
  vs <- lapply(vs, grep, pattern="^Value=", value=TRUE)
  values <- lapply(vs, gsub, pattern="^Value=([^;]*);?(.*)", replacement="\\1")

  numericvar <- dcf$datatype[hasvs] == "Numeric"
  values[numericvar] <- suppressWarnings(lapply(values[numericvar], as.integer))
  values[!numericvar] <- lapply(values[!numericvar], gsub, pattern = "'\\s+'", replacement=NA)

  labels <- lapply(vs, gsub, pattern="^Value=([^;]*);?(.*)", replacement="\\2")
  if(all_lower)
    labels <- lapply(labels, tolower)

  values <- Map("names<-", values, labels)
  values <- lapply(values, function(x) x[!is.na(x) & nchar(names(x)) > 0])

  dcf$labels[hasvs] <- values

  ## Expand dictionary for multiple occurences
  occ_dct <- rep(dcf$occurences, dcf$occurences)
  occ_i <- unlist(lapply(dcf$occurences, seq_len))
  sfx <- lapply(dcf$occurences, function(x) formatC(seq_len(x), width=nchar(x), flag="0"))
  name_dct <- paste0(rep(dcf$name, dcf$occurences),
                     ifelse(occ_dct > 1, paste0("_", unlist(sfx)), ""))
  len_dct <- rep(dcf$len, dcf$occurences)

  dct <- data.frame(name  = name_dct,
                    label = rep(dcf$label, dcf$occurences),
                    len   = len_dct,
                    start = rep(dcf$start, dcf$occurences) + len_dct * (occ_i - 1L),
                    occurences = occ_dct,
                    datatype   = rep(dcf$datatype, dcf$occurences),
                    stringsAsFactors=FALSE)
  dct$labels <- rep(dcf$labels, dcf$occurences)

  return(dct)
}

#' @rdname parse_meta
#' @param sps .SPS file as character vector (e.g. from readLines)
#' @export
parse_sps <- function(sps, all_lower=TRUE){

  endblock <- grep("^ *\\.", sps)

  ## Parse variable list
  varlst_idx <- grep("^DATA LIST.*", sps)
  varlst <- sps[(varlst_idx+1):(min(endblock[endblock > varlst_idx ])-1)]
  varlst <- grep("[a-zA-Z]", varlst, value=TRUE) # remove rows without alpha

  name <- tolower(sub("^ *([^ ]+) +([^ ]+) *([^ ]*)", "\\1", varlst))
  name <- sub("\\$", "_", name)
  start <- as.integer(sub("^ *([^ ]+) +([0-9]+)-([0-9]+) *([^ ]*)", "\\2", varlst))
  end <- as.integer(sub("^ *([^ ]+) +([0-9]+)-([0-9]+) *([^ ]*)", "\\3", varlst))
  datatype <- sub("^ *([^ ]+) +([0-9]+)-([0-9]+) *([^ ]*)", "\\4", varlst)

  dct <- data.frame(name = name,
                    start = start,
                    len   = end - start + 1L,
                    datatype = c("Numeric", "Alpha")[match(datatype, c("", "(A)"))],
                    stringsAsFactors=FALSE)

  ## Parse variable labels
  varlbl_idx <- grep("^VARIABLE LABELS.*", sps)
  varlbl <- sps[(varlbl_idx+1):(min(endblock[endblock > varlbl_idx ])-1)]
  varlbl <- grep("[a-zA-Z]", varlbl, value=TRUE) # remove rows without alpha

  varlabels <- sub("^[^a-zA-Z]*([a-zA-Z][^ ]+) +\\\"([^\\\"]+)\\\".*", "\\2", varlbl)
  names(varlabels) <- tolower(sub("^[^a-zA-Z]*([a-zA-Z][^ ]+) +\\\"([^\\\"]+)\\\".*", "\\1", varlbl))
  names(varlabels) <- sub("\\$", "_", names(varlabels))

  dct$label <- varlabels[dct$name]

  ## Parse value labels

  vallbl_idx <- grep("^VALUE LABELS.*", sps)
  vallbl <- sps[(vallbl_idx+1):(min(endblock[endblock > vallbl_idx ])-1)]

  validx <- c(1, grep("^ +/.+", vallbl))
  itidx <- Map(":", validx+1, c(validx[-1]-1, length(vallbl)))
  items <- lapply(itidx, function(idx) vallbl[idx])

  values <- lapply(items, sub,
                   pattern=" *([0-9]+) +\\\"([^\\\"]+)\\\".*",
                   replacement="\\1")
  values <- lapply(values, as.integer)

  labels <- lapply(items, sub,
                   pattern=" *([0-9]+) +\\\"([^\\\"]+)\\\".*",
                   replacement="\\2")
  if(all_lower)
    labels <- lapply(labels, tolower)
  values <- Map("names<-", values, labels)

  names(values) <- tolower(sub(".*?([a-zA-Z][^ ]*).*", "\\1", vallbl[validx]))
  names(values) <- sub("\\$", "_", names(values))

  dct$labels <- values[dct$name]

  return(dct)
}


#' Read DHS flat file data set
#'
#' This function reads a DHS recode dataset from the zipped flat file dataset.
#'
#' @param zfile Path to `.zip` file containing flat file dataset, usually ending in filename `XXXXXXFL.zip`
#' @param all_lower Logical indicating whether all value labels should be lower case. Default to `TRUE`.
#' @return A data frame. Value labels for each variable are stored as the `labelled` class from `haven`.
#'
#' @seealso \code{\link{haven::labelled}}, \code{\link{read_dhs_dta}}.
#'
#' For more information on the DHS filetypes and contents of distributed dataset .ZIP files,
#' see \url{https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10334}.
#'
#' @examples
#' mrfl_zip <- tempfile()
#' download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZMR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", mrfl_zip)
#'
#' mr <- read_dhs_flat(mrfl_zip)
#' attr(mr$mv213, "label")
#' class(mr$mv213)
#' head(mr$mv213)
#' table(mr$mv213)
#' table(haven::as_factor(mr$mv213))
#'
#' @importFrom hhsurveydata read_zipdata
#' @export
read_dhs_flat <- function(zfile, all_lower=TRUE) {

  if(any(grepl("\\.DCF", unzip(zfile, list=TRUE)$Name), ignore.case=TRUE)) {
    dcf <- read_zipdata(zfile, "\\.DCF", readLines)
    dct <- parse_dcf(dcf)
  }
  else if(any(grepl("\\.SPS", unzip(zfile, list=TRUE)$Name), ignore.case=TRUE)) {
    sps <- read_zipdata(zfile, "\\.SPS", readLines)
    dct <- parse_sps(sps)
  }

  dct$col_types <- c("integer", "character")[match(dct$datatype, c("Numeric", "Alpha"))]
  dat <- read_zipdata(zfile, "\\.DAT$", iotools::input.file,
                      formatter = iotools::dstrfw, col_types = dct$col_types, widths = dct$len)
  names(dat) <- dct$name
  dat[dct$name] <- Map("attr<-", dat[dct$name], "label", dct$label)
  haslbl <- sapply(dct$labels, length) > 0
  dat[dct$name[haslbl]] <- Map(haven::labelled, dat[dct$name[haslbl]], dct$labels[haslbl])
  
  return(dat)
}
