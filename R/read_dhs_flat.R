
#' Parse .DCF file
#'
#' @title Parse .DCF dictionary file
#' @param dcf .DCF file path to parse
#' @return data.frame with metadata and labels as attributes
#'
#' @param zfile path to zip directory containing DHS flat file dataset.
#' @param alllower logical indicating whether to convert variable labels to lower case. Defaults to `TRUE`.
parse_dcf <- function(dcf){
  
  dcf <- readLines(dcf)
  
  item.start <- which(dcf == "[Item]")
  item.len <- diff(c(item.start, length(dcf)))
  item.idx <- Map("+", item.start-1L, lapply(item.len, seq_len))
  
  items <- lapply(item.idx, function(idx) paste(dcf[idx], collapse="\n"))

  ## !WORK IN PROGRESS: parsing value labels
  ## x <- vs[40]
  ## .get_labels <- function(x){
  ##   x <- strsplit(x, "\n")
  ## id <- 
  ## hasvs <- grepl("\\[ValueSet\\]", items)
  ## vs <- gsub(".*?(\\[ValueSet\\].*)", "\\1", items[hasvs])
  ## sub(".*?Name=([^/\n]*)\n.*", "\\1", items[[1]])
  ## sub("\\[Item\\][^(\\[.*\\])]*Name=([^/\n]*)\n.*", "\\1", items[[1]])
  ## sub("\[Item\][^(\n\[)]*Name=([^/\n]*)\n.*", "\\1", items[[1]])
  
  
  dcf <- data.frame(name       = sub(".*?\nName=([^\n]*)\n.*", "\\1", items),
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
  
  
  .expand_occ <- function(name, label, start, len, datatype, occurences){
    data.frame(name       = paste0(name, "_", formatC(seq_len(occurences), width=nchar(occurences), flag="0")),
               label      = label,
               len        = len,
               start      = start - len + seq_len(occurences)*len,
               occurences = 1,
               datatype   = datatype,
               stringsAsFactors = FALSE)
  }
  
  dct <- c(f=.expand_occ, dcf[dcf$occurences > 1,])
  dct <- do.call(Map, dct)
  dct <- do.call(rbind, dct)
  dct <- rbind(dcf[dcf$occurences == 1,], dct)
  dct <- dct[order(dct$start),]
  
  ## tmp <- read.fwf(fwf, dct$len, col.names=dct$name)

  return(dct)
}
