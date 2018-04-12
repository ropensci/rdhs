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
    if(is.element("label.table",attributes(res) %>% names)){
      res <- factor_format(res,type = "foreign",reformat,all_lower)
    } else {
      res <- factor_format(res,type="labelled",reformat,all_lower)
    }

    # 2. .sav file
  } else if(file_match==2){

    # haven 1.1.1 has some issues, so have changed the description
    res <- read_zipdata(file,".sav$",haven::read_spss,user_na=TRUE)

    # reformat to create the nice description table
    res <- factor_format(res,type="labelled",reformat,all_lower)

    # 3. .dat file
  } else if(file_match==3){

    # check that it's a flat .dat file, otherwise we can't read it yet
    if(grepl("FL",basename(file))){
      res <- read_dhs_flat(file,all_lower = all_lower,...)
      res <- factor_format(res,type="labelled",reformat,all_lower)
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
    res <- rgdal::readOGR(dsn = dirname(file),layer = strsplit(basename(file),".",fixed=TRUE)[[1]][1])

  }

  return(res)
}


#' reformat haven and labelled read ins to be more useful
#' @param res dataset to be formatted
#' @param type One of "labelled" or "foreign" as to whether the dataset was read in using foreign or
#' was read in and suitably labelled using `haven::labelled`. Default = "labelled"
#' @param reformat Boolean whether to remove all factors and labels and
#' just return the unfactored data. Default = FALSE
#' @param all_lower Logical indicating whether all value labels should be lower case. Default to `TRUE`.
#'
#' @return list with the formatted dataset and the code descriptions
factor_format <- function(res,type="labelled",reformat=FALSE,all_lower=TRUE){

  if(type=="labelled"){

    # grab the labels from attributes
    lab <- lapply(res,function(x) attr(x,"labels"))
    lab <- lab[!lapply(lab,is.null) %>% unlist]

    # make all one case for match purposes
    if(all_lower){
      names(lab) <- tolower(names(lab))
      names(res) <- tolower(names(res))
    } else {
      names(lab) <- toupper(names(lab))
      names(res) <- toupper(names(res))
    }

    # create the description table
    description <- lapply(res,attr,"label")
    description_table <- data.frame("variable"=names(res),"description"=as.character(description),
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
    description_table <- data.frame("variable"=names(res),"description"=atts$var.labels,
                                    stringsAsFactors = FALSE)
  }

  # are we reformating
  if(reformat){

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

  }

  return(list("dataset"=res,"variable_names"=description_table))
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

read_zipdta <- function(zfile, ...){
  read_zipdata(zfile, ".dta$", foreign::read.dta, TRUE, ...)
}

find_dhsvar <- function(zfile, str="hdpidx", pattern=".MAP$", ignore.case=TRUE){
  map <- read_zipdata(zfile, pattern, readLines, TRUE)
  as.logical(length(grep(str, map, ignore.case)))
}