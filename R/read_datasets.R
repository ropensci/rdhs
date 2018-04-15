#' read in dhs standard file types
#' @param file path to zip file to be read
#' @param reformat boolean detailing if datasets should be nicely reformatted. Default = TRUE
#' @param all_lower Logical indicating whether all value labels should be lower case. Default to `TRUE`.
#' @param ... Extra arguments to be
#'
read_dhs_dataset <- function(file, reformat = FALSE, all_lower = TRUE, ...){

  zip_contents <- unzip_warn_fails(file,list=TRUE)
  filetype <- strsplit(zip_contents$Name,".",fixed=T) %>% lapply(function(x) tail(x,1)) %>% unlist
  file_types <- c("dta","sav","dat","sas7bdat","dbf")
  file_match <- which(toupper(file_types) %in% toupper(filetype))

  # 1. .dta file
  if(file_match==1){

    ## read in data with any arguments provided
    res <- read_dhs_dta(file,...)

    # reformat to create the nice description table
    res <- factor_format(res,reformat,all_lower)

    # 2. .sav file
  } else if(file_match==2){

    # haven 1.1.1 has some issues, so have changed the description
    res <- read_zipdata(file,".sav$",haven::read_spss,user_na=TRUE)

    # reformat to create the nice description table
    res <- factor_format(res,reformat,all_lower)

    # 3. .dat file
  } else if(file_match==3){

    # check that it's a flat .dat file, otherwise we can't read it yet
    if(grepl("FL",basename(file),ignore.case=TRUE)){
      res <- read_dhs_flat(file,all_lower = all_lower,...)
      res <- factor_format(res,reformat,all_lower)
    } else {
      message("No support for reading in hierarchal .dat files as of yet. Perhaps read/download flat .dat datasets instead?")
      res <- "No support for importing hierarchal .dat"
    }

    # 4. .sas7bdat file
  } else if(file_match==4){

    message("No support for reading in .sas7bdat files as of yet. Perhaps read/download .dat datasets instead?")
    res <- "No support for importing .sas7bdat"

    # 5. .dbf (geographic datasets)
  } else if(file_match==5){

    # dbf is a bit different due to the arguments of readOGR so we have to unzip here and handle
    unzipped_files <- unzip_warn_fails(file,exdir=tempfile())
    file <- unzipped_files[which(toupper(filetype) %in% toupper(file_types))]
    res <- list("dataset"=rgdal::readOGR(dsn = dirname(file), layer = strsplit(basename(file), ".", fixed=TRUE)[[1]][1]))

  }

  return(res)
}


#' reformat haven and labelled read ins to have no factors or labels
#' @param res dataset to be formatted
#' @param reformat Boolean whether to remove all factors and labels and
#' just return the unfactored data. Default = FALSE
#' @param all_lower Logical indicating whether all value labels should be lower case. Default to `TRUE`.
#'
#' @return list with the formatted dataset and the code descriptions

factor_format <- function(res,reformat=FALSE,all_lower=TRUE){

  # what kind of dataset is it we are working with
  if(is.element("label.table",attributes(res) %>% names)) {
    type <- "foreign"
  } else if(any(lapply(res,class) %>% unlist == "labelled")) {
    type <- "labelled"
  } else {
    stop ("Dataset does not have a label.table attribute or any labelled vaiable classes")
  }

  if(type=="labelled"){

    # grab the labels from attributes
    lab <- lapply(res, function(x) attr(x, "labels"))
    lab <- lab[!lapply(lab, is.null) %>% unlist]

    # make all one case for match purposes
    if(all_lower){
      names(lab) <- tolower(names(lab))
      names(res) <- tolower(names(res))
    } else {
      names(lab) <- toupper(names(lab))
      names(res) <- toupper(names(res))
    }

    # create the description table
    description <- lapply(res, attr, "label")
    description_table <- data.frame("variable"=names(res), "description"=as.character(description),
                                    stringsAsFactors = FALSE)
  } else if(type=="foreign"){

    # grab the attributes and the label table
    atts <- attributes(res)
    lab <- atts$label.table

    # make all one case for match purposes
    if(all_lower){
      names(lab) <- tolower(names(lab))
      names(res) <- tolower(names(res))
    } else {
      names(lab) <- toupper(names(lab))
      names(res) <- toupper(names(res))
    }

    # create the description table
    description_table <- data.frame("variable"=names(res), "description"=atts$var.labels,
                                    stringsAsFactors = FALSE)

    # assign variable labels to res
    res[description_table$variable] <- Map("attr<-", res[description_table$variable], "label", description_table$description)
  }

  # are we reformatting
  if(reformat){

    # replace value codes with value labels
    for(i in 1:length(lab)){
      pos <- match(names(lab[i]), names(res))
      lab_matches <- which(!is.na(match(res[[pos]], lab[[i]])))
      if(length(lab_matches) > 0){
        res[[pos]][lab_matches] <- names(lab[[i]])[match(res[[pos]][lab_matches], lab[[i]])]
      }
    }

    res <- lapply(res, as.character) %>% lapply(type.convert, as.is=TRUE)
    res <- as.data.frame.list(res, stringsAsFactors = FALSE)

    # reassign variable labels
    res[description_table$variable] <- Map("attr<-", res[description_table$variable], "label", description_table$description)

  }

  return(list("dataset"=res, "variable_names"=description_table))
}

#' Return variable labels from a dataset
#'
#' Returns variable labels stored as \code{"label"} attribute.
#'
#' @param data A \code{data.frame} from which to extract variable labels.
#' @param return_all Logical whether to return all variables (\code{TRUE}) or only those with labels.
#' @return A \code{data.frame} consisting of the variable name and labels.
#' @export
get_var_labels <- function(data, return_all=TRUE) {

  # what kind of dataset is it we are working with
  if(is.element("label.table",attributes(data) %>% names)) {
    type <- "foreign"
  } else if(any(lapply(data,class) %>% unlist == "labelled")) {
    type <- "labelled"
  } else if(any(lapply(data,attributes) %>% lapply(names) %>% unlist == "label")){
    type <- "labelled"
  }

  if(type=="labelled"){

    # and for labelled it's the the label attribute
    description <- lapply(data,attr,"label")

  } else if(type=="foreign"){

    # for the foreign extracts we grab the var.labels attributes
    description <- attributes(data)$var.labels
  }

  if(return_all){
    description[setdiff(names(data), names(description))] <- NA
    description <- description[names(data)]
  }

  # create the description table
  description_table <- data.frame("variable"=names(data),"description"=as.character(description),
                                  stringsAsFactors = FALSE)

  return(description_table)

}

#' Create list of dataset and its variable names
#'
#' Function to give the former output of get_datasets as it can be nice to have both the
#' definitions and the dataset attached together
#'
#' @param dataset Any read in dataset created by \code{get_datasets}
#' @export
data_and_labels <- function(dataset){

  variable_names <- get_var_labels(dataset)
  res <- list("dataset"=dataset,"variable_names"=variable_names)

}


#' Read filetype from a zipped folder based on the file ending
#'
#'
#' @param zfile Path to `.zip` file containing flat file dataset, usually ending in filename `XXXXXXFL.zip`
#' @param pattern String detailing which filetype is to be read from within the zip by means of a grep. Default = ".dta$"
#' @param readfn Function object to be used for reading in the identified file within the zip. Default = `foreign::read.dta`
#' @param ...  additional arguments to readfn
#' @export
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


# create simplest unique filenames that handles the india duplication.
create_new_filenames <- function(data){
  data$file <- strsplit(data$FileName,".",fixed=T) %>% lapply(function(x)x[1]) %>% unlist
  issues <- which(!(tolower(substr(data$FileName,1,2))==tolower(data$DHS_CountryCode)))
  data$file[issues] <- paste0(data$file[issues],"_",data$CountryName[issues])
  data
}

# NOT USED ANYMORE - REMOVE AT NEXT PACKAGE VERSION MOVE
read_zipdta <- function(zfile, ...){
  read_zipdata(zfile, ".dta$", foreign::read.dta, TRUE, ...)
}

# NOT USED ANYMORE - REMOVE AT NEXT PACKAGE VERSION MOVE
find_dhsvar <- function(zfile, str="hdpidx", pattern=".MAP$", ignore.case=TRUE){
  map <- read_zipdata(zfile, pattern, readLines, TRUE)
  as.logical(length(grep(str, map, ignore.case)))
}
