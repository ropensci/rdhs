#' Parse .DCF file
#'
#' @title Parse .DCF dictionary file
#' @param dcf .DCF file path to parse
#' @param alllower logical indicating whether to convert variable labels to lower case. Defaults to `TRUE`.
#' @return data.frame with metadata and labels as attributes
#'
#' @examples
#' mrfl_zip <- tempfile()
#' download.file("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?Filename=ZZMR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs", mrfl_zip)
#'
#' dcf <- read_zipdata(mrfl_zip, "\\.DCF", readLines)
#' dct <- parse_dcf(dcf)
#'
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
  
  .expand_occ <- function(name, label, start, len, datatype, occurences, labels){
    v <- data.frame(name       = paste0(name, "_", formatC(seq_len(occurences), width=nchar(occurences), flag="0")),
                    label      = label,
                    len        = len,
                    start      = start - len + seq_len(occurences)*len,
                    occurences = 1,
                    datatype   = datatype,
                    stringsAsFactors = FALSE)
    v$labels <- list(labels)
    return(v)
  }
  
  dct <- c(f=.expand_occ, dcf[dcf$occurences > 1,])
  dct <- do.call(Map, dct)
  dct <- do.call(rbind, dct)
  dct <- rbind(dcf[dcf$occurences == 1,], dct)
  dct <- dct[order(dct$start),]

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

  dcf <- read_zipdata(zfile, "\\.DCF", readLines)
  dct <- parse_dcf(dcf)
  dct$col_types <- c("integer", "character")[match(dct$datatype, c("Numeric", "Alpha"))]
  dat <- read_zipdata(zfile, "\\.DAT$", iotools::input.file,
                      formatter = iotools::dstrfw, col_types = dct$col_types, widths = dct$len)
  names(dat) <- dct$name
  dat[dct$name] <- Map("attr<-", dat[dct$name], "label", dct$label)
  haslbl <- sapply(dct$labels, length) > 0
  dat[dct$name[haslbl]] <- Map(haven::labelled, dat[dct$name[haslbl]], dct$labels[haslbl])
  
  return(dat)
}
