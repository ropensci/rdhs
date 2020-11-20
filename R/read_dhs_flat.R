#' Parse dataset metadata
#'
#' @title Parse fixed-width file metadata
#'
#' @param dcf .DCF file path to parse
#' @param all_lower logical indicating whether to convert variable labels to
#'   lower case. Defaults to `TRUE`.
#'
#' @return data.frame with metadata for parsing fixed-width flat file
#'
#' @examples
#' mrfl_zip <- tempfile()
#' download.file("https://dhsprogram.com/data/model_data/dhs/zzmr61fl.zip",
#'               mrfl_zip, mode = "wb")
#'
#' dcf <- rdhs::read_zipdata(mrfl_zip, "\\.DCF", readLines)
#' dct <- rdhs:::parse_dcf(dcf)
#'
#' sps <- rdhs::read_zipdata(mrfl_zip, "\\.SPS", readLines)
#' dct <- rdhs:::parse_sps(sps)
#'
#' do <- rdhs::read_zipdata(mrfl_zip, "\\.DO", readLines)
#' dctin <- rdhs::read_zipdata(mrfl_zip, "\\.DCT", readLines)
#' dct <- rdhs:::parse_do(do, dctin)
#'
#' @name parse_meta
NULL

#' @rdname parse_meta
parse_dcf <- function(dcf, all_lower=TRUE) {

  item.start <- which(dcf == "[Item]")
  item.len <- diff(c(item.start, length(dcf) + 1L))
  item.idx <- Map("+", item.start - 1L, lapply(item.len, seq_len))

  items <- lapply(item.idx, function(idx) paste(dcf[idx], collapse = "\n"))

  dcf <- data.frame(
    name = tolower(sub(".*?\nName=([^\n]*)\n.*", "\\1", items)),
    label = sub(".*?\nLabel=([^\n]*)\n.*", "\\1", items),
    start = as.integer(sub(".*?\nStart=([^\n]*)\n.*", "\\1", items)),
    len = as.integer(sub(".*?\nLen=([^\n]*)\n?.*", "\\1", items)),
    datatype = "Numeric",
    occurences = 1,
    stringsAsFactors = FALSE
  )
  has_datatype <- grep(".*?\nDataType", items)
  dcf$datatype[has_datatype] <- sub(".*?\nDataType=([^\n]*)(\n.*|$)", "\\1",
                                    items[has_datatype])

  has_occ <- grep(".*?\nOccurrences", items)
  dcf$occurences[has_occ] <- as.integer(sub(".*?\nOccurrences=([^\n]*)\n?.*",
                                            "\\1", items[has_occ]))

  ## add labels
  hasvs <- grepl("\\[ValueSet\\]", items)
  vs <- gsub(".*?(\\[ValueSet\\].*)", "\\1", items[hasvs])
  vs <- strsplit(vs, "\n")
  vs <- lapply(vs, grep, pattern = "^Value=", value = TRUE)
  values <- lapply(vs, gsub, pattern = "^Value=([^;]*);?(.*)",
                   replacement = "\\1")

  numericvar <- dcf$datatype[hasvs] == "Numeric"
  values[numericvar] <- suppressWarnings(lapply(values[numericvar], as.integer))
  values[!numericvar] <- lapply(values[!numericvar], gsub, pattern = "'\\s+'",
                                replacement = NA)

  labels <- lapply(vs, gsub, pattern = "^Value=([^;]*);?(.*)",
                   replacement = "\\2")
  if (all_lower) {
    labels <- lapply(labels, tolower)
  }

  values <- Map("names<-", values, labels)
  values <- lapply(values, function(x) x[!is.na(x) & nzchar(names(x))])

  ## remove duplicate values from labels
  values <- lapply(values, function(x) x[!duplicated(x)])

  dcf$labels[hasvs] <- values

  ## Expand dictionary for multiple occurences
  occ_dct <- rep(dcf$occurences, dcf$occurences)
  occ_i <- unlist(lapply(dcf$occurences, seq_len))
  sfx <- lapply(dcf$occurences, function(x) {
    formatC(seq_len(x), width = nchar(x), flag = "0")
    })
  name_dct <- paste0(
    rep(dcf$name, dcf$occurences),
    ifelse(occ_dct > 1, paste0("_", unlist(sfx)), "")
  )
  len_dct <- rep(dcf$len, dcf$occurences)

  dct <- data.frame(
    name = name_dct,
    label = rep(dcf$label, dcf$occurences),
    len = len_dct,
    start = rep(dcf$start, dcf$occurences) + len_dct * (occ_i - 1L),
    occurences = occ_dct,
    datatype = rep(dcf$datatype, dcf$occurences),
    stringsAsFactors = FALSE
  )
  dct$labels <- rep(dcf$labels, dcf$occurences)

  return(dct)
}

#' @rdname parse_meta
#' @param sps .SPS file as character vector (e.g. from readLines)
parse_sps <- function(sps, all_lower=TRUE) {

  sps <- iconv(sps, to = "UTF-8", sub = "") # drop unrecognized multibytes

  endblock <- grep("^ *\\.", sps)

  ## Parse variable list
  varlst_idx <- grep("^DATA LIST.*", sps)
  varlst <- sps[(varlst_idx + 1):(min(endblock[endblock > varlst_idx ]) - 1)]
  varlst <- grep("[a-zA-Z]", varlst, value = TRUE) # remove rows without alpha

  name <- tolower(sub("^ *([^ ]+) +([^ ]+) *([^ ]*)", "\\1", varlst))
  name <- sub("\\$", "_", name)
  start <- as.integer(sub("^ *([^ ]+) +([0-9]+)-([0-9]+) *([^ ]*)",
                          "\\2", varlst))
  end <- as.integer(sub("^ *([^ ]+) +([0-9]+)-([0-9]+) *([^ ]*)",
                        "\\3", varlst))
  datatype <- sub("^ *([^ ]+) +([0-9]+)-([0-9]+) *([^ ]*)",
                  "\\4", varlst)
  datatype <- sub("\\([0-9]+\\)", "(D)", datatype)

  dct <- data.frame(
    name = name,
    start = start,
    len = end - start + 1L,
    datatype = c("Numeric", "Alpha", "Decimal")[match(datatype,
                                                      c("", "(A)", "(D)"))],
    stringsAsFactors = FALSE
  )

  ## Parse variable labels
  varlbl_idx <- grep("^VARIABLE LABELS.*", sps)
  varlbl <- sps[(varlbl_idx + 1):(min(endblock[endblock > varlbl_idx ]) - 1)]
  varlbl <- grep("[a-zA-Z]", varlbl, value = TRUE) # remove rows without alpha

  pattern <- "^[^a-zA-Z]*([a-zA-Z][^ ]+) +(('[^']*')|(\\\"[^\\\"]*\\\")).*"
  varlabels <- sub(pattern, "\\2", varlbl)
  varlabels <- sub("^['\\\"] *([^ ].*?) *['\\\"]$", "\\1", varlabels)

  names_vars <- sub(pattern, "\\1", varlbl)
  names(varlabels) <- tolower(names_vars)
  names(varlabels) <- sub("\\$", "_", names(varlabels))

  dct$label <- varlabels[dct$name]

  ## Parse value labels

  vallbl_idx <- grep("^VALUE LABELS.*", sps)
  vallbl <- sps[(vallbl_idx + 1):(min(endblock[endblock > vallbl_idx ]) - 1)]

  validx <- c(1, grep("^ */.+", vallbl))
  itidx <- Map(":", validx + 1, c(validx[-1] - 1, length(vallbl)))
  items <- lapply(itidx, function(idx) vallbl[idx])

  pattern <- "^ *((' *[^ ].*?')|([0-9]+)) *(('[^']*')|(\\\"[^\\\"]*\\\")).*"

  values <- lapply(items, sub, pattern = pattern, replacement = "\\1")
  values <- lapply(values,
                   sub,
                   pattern = "' *([^ ].*?) *'",
                   replacement = "\\1")

  labels <- lapply(items, sub, pattern = pattern, replacement = "\\4")
  labels <- lapply(labels,
                   sub,
                   pattern = "^['\\\"] *([^ ].*?) *['\\\"]$",
                   replacement = "\\1")

  if (all_lower) {
    labels <- lapply(labels, tolower)
  }
  values <- Map("names<-", values, labels)

  names(values) <- tolower(sub(".*?([a-zA-Z][^ ]*).*", "\\1", vallbl[validx]))
  names(values) <- sub("\\$", "_", names(values))

  val_nam_match <- match(names(values), dct$name)
  numericvar <- dct$datatype[val_nam_match] %in% c("Numeric", "Decimal")
  valnum <- values[!is.na(numericvar) & numericvar]
  valnum <- lapply(valnum, gsub, pattern = "[^0-9]", replacement = "")
  valnum <- lapply(valnum, "storage.mode<-", "integer")
  values[!is.na(numericvar) & numericvar] <- valnum

  ## remove duplicate values from labels
  values <- lapply(values, function(x) x[!duplicated(x)])
  
  dct$labels <- values[dct$name]

  return(dct)
}

#' @rdname parse_meta
#' @param do .DO file as character vector (e.g. from readLines)
#' @param dct .DCT file as character vector (e.g. from readLines)
parse_do <- function(do, dct, all_lower=TRUE) {


  # fixed width parse
  dct <- dct[-c(1:2, length(dct))]

  pattern <- "^ *([^ ]+) +([^ ]+) +([0-9]+): *([0-9]+)-([0-9]+).*"
  name_dct <- tolower(sub(pattern, "\\2", dct))
  datatype_dct <- c("Numeric", "Alpha")[match(sub(pattern, "\\1", dct) == "str",
                                              c(FALSE, TRUE))]
  start_dct <- as.integer(sub(pattern, "\\4", dct))
  len_dct <- as.integer(sub(pattern, "\\5", dct)) - start_dct + 1L

  dct <- data.frame(
    name = name_dct,
    len = len_dct,
    start = start_dct,
    datatype = datatype_dct,
    stringsAsFactors = FALSE
  )

  ## reset delim
  x <- paste(do, collapse = "\n")

  x <- strsplit(paste("#delimit cr\n", x), "#delimit")[[1]]
  delimidx <- sub(" +?([^ \n]+).*", "\\1", x) == ";"
  x[delimidx] <- gsub("\n", " ", x[delimidx])
  x[delimidx] <- gsub("; *", "\n", x[delimidx])
  x <- paste(x, collapse = "\n")
  x <- strsplit(x, "\n")[[1]]

  varlbl <- grep("^label variable", x, value = TRUE)

  varlabels <- sub("^label variable ([^ ]+) +?([^ ].*)", "\\2", varlbl)
  varlabels <- gsub("\"", "", varlabels)
  names(varlabels) <- tolower(sub("^label variable ([^ ]+) +?([^ ].*)",
                                  "\\1", varlbl))

  ## parse value labels
  lbldef <- grep("^ *label define", x, value = TRUE)

  lblname <- sub("^ *label define ([^ ]+) +(.*)", "\\1", lbldef)
  lblstr <- sub("^ *label define ([^ ]+) +(.*)", "\\2", lbldef)
  lblstr <- strsplit(lblstr, "\"")

  if (any(unlist(lapply(lblstr, length)) %% 2 != 1)) {
    stop("Error parsing value labels")
  }

  lblstr <- lapply(lblstr, function(x) x[-length(x)])
  levels <- lapply(lblstr, "[", c(TRUE, FALSE))
  levels <- suppressWarnings(lapply(levels, as.integer))

  labels <- lapply(lblstr, "[", c(FALSE, TRUE))
  if (all_lower) {
    labels <- lapply(labels, tolower)
  }
  levels <- Map("names<-", levels, labels)
  names(levels) <- lblname

  ## assign value labels
  lblval <- grep("^label values", x, value = TRUE)
  lblname <- sub("^label values +([^ ]+) +([^ ]+).*", "\\2", lblval)
  names(lblname) <- sub("^label values +([^ ]+) +([^ ]+).*", "\\1", lblval)

  ## add labels to dct
  dct$label <- varlabels[dct$name]
  dct$labels <- levels[lblname[dct$name]]

  return(dct)
}

#' Read DHS flat file data set
#'
#' This function reads a DHS recode dataset from the zipped flat file dataset.
#'
#' @param zfile Path to `.zip` file containing flat file dataset, usually
#'   ending in filename `XXXXXXFL.zip`
#' @param all_lower Logical indicating whether all value labels should be
#'   lower case. Default to `TRUE`.
#' @param meta_source character string indicating metadata source file for data
#'   dictionary. Default \code{NULL} first tried to use \code{.DCF} and then
#'   {.SPS} if not found.
#' @return A data frame. Value labels for each variable are stored as the
#'   `labelled` class from `haven`.
#'
#' @seealso \code{\link[haven]{labelled}}, \code{\link{read_dhs_dta}}.
#'
#' For more information on the DHS filetypes and contents of distributed
#' dataset .ZIP files, see
#' \url{https://dhsprogram.com/data/File-Types-and-Names.cfm#CP_JUMP_10334}.
#'
#' @examples
#' mrfl_zip <- tempfile()
#' download.file("https://dhsprogram.com/data/model_data/dhs/zzmr61fl.zip",
#'               mrfl_zip,mode="wb")
#'
#' mr <- rdhs:::read_dhs_flat(mrfl_zip)
#' attr(mr$mv213, "label")
#' class(mr$mv213)
#' head(mr$mv213)
#' table(mr$mv213)
#' table(haven::as_factor(mr$mv213))
#'
read_dhs_flat <- function(zfile, all_lower=TRUE, meta_source=NULL) {

  null_meta <- is.null(meta_source)

  if ( (null_meta || tolower(meta_source) == "dcf") &&
    any(grepl("\\.DCF$", unzip(zfile, list = TRUE)$Name, ignore.case = TRUE))) {
    dcf <- read_zipdata(zfile, "\\.DCF$", readLines,
                        encoding = "UTF-8", warn = FALSE)
    dct <- parse_dcf(dcf, all_lower)
  }
  else if ( (null_meta || tolower(meta_source) == "sps") &&
    any(grepl("\\.SPS$", unzip(zfile, list = TRUE)$Name, ignore.case = TRUE))) {
    sps <- read_zipdata(zfile, "\\.SPS$", readLines,
                        encoding = "UTF-8", warn = FALSE)
    dct <- parse_sps(sps, all_lower)
  }
  else if ( (null_meta || tolower(meta_source) %in% c("do", "dct")) &&
    any(grepl("\\.DO$", unzip(zfile, list = TRUE)$Name, ignore.case = TRUE)) &&
    any(grepl("\\.DCT$", unzip(zfile, list = TRUE)$Name, ignore.case = TRUE))) {
    do <- read_zipdata(zfile, "\\.DO$", readLines,
                       encoding = "UTF-8", warn = FALSE)
    dct <- read_zipdata(zfile, "\\.DCT$", readLines,
                        encoding = "UTF-8", warn = FALSE)
    dct <- parse_do(do, dct, all_lower)
  }
  else {
    stop("metadata file not found")
  }

  types <- c("integer", "character", "numeric")
  dct$col_types <- types[match(dct$datatype, c("Numeric", "Alpha", "Decimal"))]
  dat <- read_zipdata(
    zfile, "\\.DAT$", iotools::input.file, formatter = iotools::dstrfw,
    col_types = dct$col_types, widths = dct$len, strict = FALSE
  )
  names(dat) <- dct$name
  dat[dct$name] <- Map("attr<-", dat[dct$name], "label", dct$label)
  haslbl <- unlist(lapply(dct$labels, length)) > 0

  # match on haven package version
  if (packageVersion("haven") > "1.1.2") {
  dat[dct$name[haslbl]] <- Map(haven::labelled, dat[dct$name[haslbl]],
                               dct$labels[haslbl],
                               dct$label[haslbl])
  } else {
    dat[dct$name[haslbl]] <- Map(haven::labelled, dat[dct$name[haslbl]],
                                 dct$labels[haslbl])
  }

  return(dat)
}
