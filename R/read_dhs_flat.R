#' Parse .DCF file
#'
#' @title Parse .DCF dictionary file
#' @param dcf .DCF file path to parse
#' @param all_lower logical indicating whether to convert variable labels to lower case. Defaults to `TRUE`.
#' @return data.frame with metadata and labels as attributes
#'
#' @examples
#' mrfl_zip <- tempfile()
#' download.file(paste0("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?",
#' "Filename=ZZMR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs"), mrfl_zip, mode="wb")
#'
#' dcf <- rdhs:::read_zipdata(mrfl_zip, "\\.DCF", readLines)
#' dct <- rdhs:::parse_dcf(dcf)
#'

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
#' @seealso \code{\link[haven]{labelled}}, \code{\link{read_dhs_dta}}.
#'
#' For more information on the DHS filetypes and contents of distributed dataset .ZIP files,
#' see \url{https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10334}.
#'
#' @examples
#' mrfl_zip <- tempfile()
#' download.file(paste0("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?",
#' "Filename=ZZMR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs"), mrfl_zip,mode="wb")
#'
#' mr <- rdhs:::read_dhs_flat(mrfl_zip)
#' attr(mr$mv213, "label")
#' class(mr$mv213)
#' head(mr$mv213)
#' table(mr$mv213)
#' table(haven::as_factor(mr$mv213))
#'
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


#' Read filetype from a zipped folder based on the file ending
#'
#'
#' @param zfile Path to `.zip` file containing flat file dataset, usually ending in filename `XXXXXXFL.zip`
#' @param pattern String detailing which filetype is to be read from within the zip by means of a grep. Default = ".dta$"
#' @param readfn Function object to be used for reading in the identified file within the zip. Default = `foreign::read.dta`
#' @param ...  additional arguments to readfn
#'
read_zipdata <- function(zfile, pattern=".dta$", readfn=foreign::read.dta, ...){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  file <- grep(pattern, unzip(zfile, list=TRUE)$Name, ignore.case = TRUE, value=TRUE)
  if(!length(file)){
    warning(paste0("File name matching pattern '", pattern, "' not found in zip file '", basename(zfile), "'."))
    return(invisible(NULL))
  }
  if(length(file) > 1)
    warning(paste0("Multiple file names match pattern '", pattern, "' in zip file '", basename(zfile), "'. Returning file '", file[1], "'."))
  return(readfn(unzip(zfile, file[1], exdir=tmp), ...))
}

read_zipdta <- function(zfile, ...){
  read_zipdata(zfile, ".dta$", foreign::read.dta, TRUE, ...)
}

find_dhsvar <- function(zfile, str="hdpidx", pattern=".MAP$", ignore.case=TRUE){
  map <- read_zipdata(zfile, pattern, readLines, TRUE)
  as.logical(length(grep(str, map, ignore.case)))
}
