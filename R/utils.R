#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
NULL

#' converts response to json by first converting the response to text
#' @param x A response
response_to_json <- function(x) {
  jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"),
                     simplifyVector = FALSE)
}

#' checks if the response is json or not by looking at the responses headers
#' @param x A response
response_is_json <- function(x) {
  content_type <- httr::headers(x)[["Content-Type"]]
  dat <- httr::parse_media(content_type)
  dat$type == "application" && dat$subtype == "json"
}

#' unzips files without throwing warnings
#' @param ... arguments to pass to \code{unzip}
#' @importFrom utils unzip
unzip_warn_fails <- function (...){
  tryCatch({
    unzip(...)
  }, warning = function(w) stop(conditionMessage(w)))
}

#' raed in dhs standard file types
#' @param file path to file to be read
#' @param reformat boolean detailing if datasets should be nicely reformatted. Default = TRUE
#'
dhs_read_dataset <- function(file, reformat = TRUE){

  filetype <- strsplit(file,".",fixed=T) %>% lapply(function(x) tail(x,1)) %>% unlist
  file_types <- c("dta","sav","dat","sas7bdat","dbf")

  # 1. .dta file
  if(match(toupper(filetype),toupper(file_types))==1){

    res <- foreign::read.dta(file,convert.dates = FALSE,
                             convert.factors = FALSE,
                             warn.missing.labels = FALSE)

    # are we reformatting
    if(reformat){
      res <- dta_factor_format(res)
    }

    # 2. .sav file
  } else if(match(toupper(filetype),toupper(file_types))==2){

    # haven 1.1.1 has some issues, so have changed the description
    res <-  haven::read_sav( file )

    # are we reformatting
    if(reformat){
      res <- haven_factor_format(res)
    }

    # 3. .dat file
  } else if(match(toupper(filetype),toupper(file_types))==3){

    message("No support for reading in .dat files as of yet. Perhaps read/download .dta datasets instead?")
    res <- "No support for importing .dat"

    # 4. .sas7bdat file
  } else if(match(toupper(filetype),toupper(file_types))==4){

    message("No support for reading in .sas7bdat files as of yet. Perhaps read/download .dta datasets instead?")
    res <- "No support for importing .dat"

    # 5. .dbf (geographic datasets)
  } else if(match(toupper(filetype),toupper(file_types))==5){

    res <- rgdal::readOGR(dsn = dirname(file),layer = strsplit(basename(file),".",fixed=TRUE)[[1]][1])

  }

  return(res)
}


#' reformat dta to be more useful
#' @param dta read in dta dataset using  foreign::read.dta(file,convert.dates = FALSE,
#' convert.factors = FALSE,warn.missing.labels = FALSE)
#'
#' @return list with the eformatted dataset and the code descriptions
dta_factor_format <- function(dta){

  # grab the attributes and the label table
  atts <- attributes(dta)
  lab <- atts$label.table

  # make all upper for match purposes
  names(lab) <- toupper(names(lab))
  names(dta) <- toupper(names(dta))

  # create the description table
  description_table <- data.frame("Code"=names(dta),"Description"=atts$var.labels,
                                  stringsAsFactors = FALSE)

  # reformat the dta so the values are the actual value rather than their code
  for(i in 1:length(lab)){
    pos <- match(names(lab[i]),names(dta))
    lab_matches <- which(!is.na(match(dta[[pos]],lab[[i]])))
    if(length(lab_matches)>0){
    dta[[pos]][lab_matches] <- names(lab[[i]])[match(dta[[pos]][lab_matches],lab[[i]])]
    }
  }

  dta <- lapply(dta,as.character) %>% lapply(type.convert,as.is=TRUE)
  dta <- as.data.frame.list(dta,stringsAsFactors = FALSE)

  return(list("Survey"=dta,"Survey_Code_Descriptions"=description_table))
}

#' reformat haven read ins to be more useful
#' @param res read in dta dataset using  haven::read_dta(file)
#'
#' @return list with the formatted dataset and the code descriptions
haven_factor_format <- function(res){

  # grab the labels from attributes
  lab <- lapply(res,function(x) attr(x,"labels"))
  lab <- lab[!lapply(lab,is.null) %>% unlist]

  # make all upper for match purposes
  names(lab) <- toupper(names(lab))
  names(res) <- toupper(names(res))

  # create the description table
  description <- lapply(res,attr,"label")
  description_table <- data.frame("Code"=names(res),"Description"=as.character(description),
                                  stringsAsFactors = FALSE)

  # reformat the res so the values are the actual value rather than their code
  for(i in 1:length(lab)){
    pos <- match(names(lab[i]),names(res))
    lab_matches <- which(!is.na(match(res[[pos]],lab[[i]])))
    if(length(lab_matches)>0){
      res[[pos]][lab_matches] <- names(lab[[i]])[match(res[[pos]][lab_matches],lab[[i]])]
    }
  }

  res <- lapply(res,as.character) %>% lapply(type.convert,as.is=TRUE)
  res <- as.data.frame.list(res, stringsAsFactors = FALSE)


  return(list("Survey"=res,"Survey_Code_Descriptions"=description_table))
}

# refresh client
client_refresh <- function(cli,root){

  cli$set_cache_date(cli$get_cache_date()-20000000)
  cli$save_client()
  if(file.exists("credentials")){
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",credentials = "credentials",root = root)
  } else {
    cli <- rdhs::dhs_client(api_key = "ICLSPH-527168",root = root)
  }

  return(cli)

}

## convert list
type_convert_list_to_df <- function(l){

  l[lapply(l,is.character) %>% unlist] <- lapply(l[lapply(l,is.character) %>% unlist],type.convert,as.is=TRUE)

  return(l)
}

# type convert factor df to normal df
type_convert_df <- function(df){

  l <- lapply(df,as.character)
  df <- as.data.frame.list(type_convert_list_to_df(l),stringsAsFactors = FALSE)

}

# remove punctuation, space and non ascii for string matching purposes
rm_punct_non_ascii <- function(string){

  string <- stringi::stri_trans_general(string, "latin-ascii")
  return(gsub('[[:punct:] ]+','',string))
}

## helper functions - not package related

# open file outside'
sopen <- function(txt_path) system(paste0("open ","\"",txt_path,"\""))

# open folder outside
sdir <- function(x)  shell(paste0("explorer ",x))

# chracterise vector
rechar_vec <- function(what) cat(paste0("c(\"",paste0(what,collapse="\",\""),"\")"))
