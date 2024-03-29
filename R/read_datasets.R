#' read in dhs standard file types
#' @param file path to zip file to be read
#' @param dataset row from \code{\link{dhs_datasets}} that
#'   corresponds to the file
#' @param reformat boolean detailing if datasets should be nicely
#'   reformatted. Default = `FALSE`
#' @param all_lower Logical indicating whether all value labels
#'   should be lower case. Default to `TRUE`.
#' @param ... Extra arguments to be passed to either
#'   \code{\link{read_dhs_dta}} or \code{\link{read_dhs_flat}}
#'
read_dhs_dataset <- function(file, dataset,
                             reformat = FALSE, all_lower = TRUE, ...) {

  # open the zip and grab the file endings
  zip_contents <- suppressWarnings(unzip(file, list = TRUE))
  filetype <- strsplit(zip_contents$Name, ".", fixed = TRUE) %>%
    lapply(function(x) tail(x, 1)) %>%
    unlist()

  # now let's check the file endings against those we expect to see
  file_types <- c("dta", "sav", "dat", "sas7bdat", "dbf", "csv")
  format_expected <- file_dataset_format(dataset$FileFormat)

  # we'll check the file format we are expecting is actually in the zip
  format_match <- any(tolower(filetype) == tolower(format_expected))

  # if there is no match it is probably because it is a geographic file
  if (!format_match && dataset$FileType %in%
      c("Geographic Data", "Geospatial Covariates")) {
    if (dataset$FileType == "Geospatial Covariates") {
      file_match <- 6
    } else {
    file_match <- 5
    }
  } else if (format_match) {
    file_match <- which(tolower(file_types) == tolower(format_expected))
  } else {
    stop("Downloaded zip contents are not recognised for: ", dataset$FileName)
  }

  # 1. .dta file
  if (file_match == 1) {

    ## read in data with any arguments provided
    res <- read_dhs_dta(file, ...)

    # reformat to create the nice description table
    res <- factor_format(res, reformat, all_lower)

    # 2. .sav file
  } else if (file_match == 2) {

    # haven 1.1.1 has some issues, so have changed the description
    res <- read_zipdata(file, ".sav$", haven::read_spss, user_na = TRUE)

    # reformat to create the nice description table
    res <- factor_format(res, reformat, all_lower)

    # 3. .dat file
  } else if (file_match == 3) {

    # check that it's a flat .dat file, otherwise we can't read it yet
    if (grepl("FL", basename(file), ignore.case = TRUE)) {
      res <- read_dhs_flat(file, all_lower = all_lower, ...)
      res <- factor_format(res, reformat, all_lower)
    } else {
      message("No support for reading in hierarchal .dat files as of yet. ",
              "Perhaps read/download flat .dat datasets instead?")
      res <- "No support for importing hierarchal .dat"
    }

    # 4. .sas7bdat file
  } else if (file_match == 4) {
    message("No support for reading in .sas7bdat files as of yet. ",
            "Perhaps read/download .dat datasets instead?")
    res <- "No support for importing .sas7bdat"

    # 5. .dbf (geographic datasets)
  } else if (file_match == 5) {

    # dbf is a bit different due to the arguments of readOGR
    # so we have to unzip here and handle
    unzipped_files <- suppressWarnings(unzip(file, exdir = tempfile()))
    file <- grep("dbf", unzipped_files, value=TRUE)
    res <- list(
      "dataset" = sf::read_sf(file, quiet = TRUE)
    )

    # 6. Geospatial Covariate files (csv)
  } else if (file_match == 6) {

    # csv geo is again different
    unzipped_files <- suppressWarnings(unzip(file, exdir = tempfile()))
    file <- unzipped_files[which(toupper(filetype) %in% toupper(file_types))]
    res <- read.csv(file)
    res <- list("dataset"=res)
  }

  return(res)
}


#' reformat haven and labelled read ins to have no factors or labels
#' @param res dataset to be formatted
#' @param reformat Boolean whether to remove all factors and labels and
#' just return the unfactored data. Default = FALSE
#' @param all_lower Logical indicating whether all value labels
#'   should be lower case. Default to `TRUE`.
#'
#' @return list with the formatted dataset and the code descriptions

factor_format <- function(res, reformat=FALSE, all_lower=TRUE) {

  # what kind of dataset is it we are working with
  type <- dataset_label_type(res, stop = TRUE)

  if (type == "labelled") {

    # grab the labels from attributes
    lab <- lapply(res, function(x) attr(x, "labels", exact = TRUE))
    lab <- lab[!lapply(lab, is.null) %>% unlist()]

    # make all one case for match purposes
    if (all_lower) {
      names(lab) <- tolower(names(lab))
      names(res) <- tolower(names(res))
    } else {
      names(lab) <- toupper(names(lab))
      names(res) <- toupper(names(res))
    }

    # create the description table
    description <- lapply(res, attr, "label", exact = TRUE)
    description_table <- data.frame(
      "variable" = names(res), "description" = as.character(description),
      stringsAsFactors = FALSE
    )
  } else if (type == "foreign") {

    # grab the attributes and the label table
    atts <- attributes(res)
    lab <- atts$label.table

    # make all one case for match purposes
    if (all_lower) {
      names(lab) <- tolower(names(lab))
      names(res) <- tolower(names(res))
    } else {
      names(lab) <- toupper(names(lab))
      names(res) <- toupper(names(res))
    }

    # create the description table
    description_table <- data.frame(
      "variable" = names(res), "description" = atts$var.labels,
      stringsAsFactors = FALSE
    )

    # assign variable labels to res
    res[description_table$variable] <- Map("attr<-",
                                           res[description_table$variable],
                                           "label",
                                           description_table$description)
  }

  # are we reformatting
  if (reformat) {

    # replace value codes with value labels
    for (i in seq_len(length(lab))) {
      pos <- match(names(lab[i]), names(res))
      lab_match <- which(!is.na(match(res[[pos]], lab[[i]])))
      if (length(lab_match) > 0) {
        res[[pos]] <- unclass(res[[pos]])
        res[[pos]][lab_match] <- names(lab[[i]])[match(res[[pos]][lab_match], lab[[i]])]
      }
    }

    res <- lapply(res, as.character) %>% lapply(type.convert, as.is = TRUE)
    res <- as.data.frame.list(res, stringsAsFactors = FALSE)

    # reassign variable labels
    res[description_table$variable] <- Map("attr<-",
                                           res[description_table$variable],
                                           "label",
                                           description_table$description)
  }

  return(list("dataset" = res, "variable_names" = description_table))
}

#' Return variable labels from a dataset
#'
#' Returns variable labels stored as \code{"label"} attribute.
#'
#' @param data A \code{data.frame} from which to extract variable labels.
#' @param return_all Logical whether to return all variables (\code{TRUE})
#'   or only those with labels.
#' @return A \code{data.frame} consisting of the variable name and labels.
#'
get_labels_from_dataset <- function(data, return_all=TRUE) {

  # what kind of dataset is it we are working with
  type <- dataset_label_type(data, stop = TRUE)

  if (type == "labelled" || type == "label_only") {

    # and for labelled it's the the label attribute
    description <- lapply(data, attr, "label", exact = TRUE)
  } else if (type == "foreign") {

    # for the foreign extracts we grab the var.labels attributes
    description <- attributes(data)$var.labels
  }

  if (return_all) {
    description[setdiff(names(data), names(description))] <- NA
    description <- description[names(data)]
  }

  # create the description table
  description_table <- data.frame(
    "variable" = names(data), "description" = as.character(description),
    stringsAsFactors = FALSE
  )

  return(description_table)
}


#' Create list of dataset and its variable names
#'
#' Function to give the former output of get_datasets as it can be nice to have
#' both the definitions and the dataset attached together
#'
#' @param dataset Any read in dataset created by \code{get_datasets},
#'   either as the file path or after having
#'   been read using \code{readRDS}
#' @export
#'
#' @examples
#' \dontrun{
#' # get the model datasets included with the package
#' model_datasets <- model_datasets
#'
#' # download one of them
#' g <- get_datasets(dataset_filenames = model_datasets$FileName[1])
#' dl <- data_and_labels(g$zzbr62dt)
#'
#' # now we easily have our survey question labels easily accessible
#' grep("bed net", dl$variable_names$description, value = TRUE)
#' }
data_and_labels <- function(dataset) {

  if (class(dataset)[1] == "character") {
    if (file.exists(dataset)) {
      dataset <- readRDS(dataset)
    } else {
      stop(
        "Invalid dataset argument. Must be a character",
        "string to where a dataset has been saved, or a read in dataset.",
        "See ?data_and_labels for more inforation"
      )
    }
  }
  variable_names <- get_variable_labels(dataset)
  res <- list("dataset" = dataset, "variable_names" = variable_names)
  return(res)
}


#' Read filetype from a zipped folder based on the file ending
#'
#'
#' @param zfile Path to `.zip` file containing flat file dataset,
#'   usually ending in filename `XXXXXXFL.zip`
#' @param pattern String detailing which filetype is to be read
#'   from within the zip by means of a grep. Default = ".dta$"
#' @param readfn Function object to be used for reading in the
#'   identified file within the zip. Default = `haven::read_dta`
#' @param ...  additional arguments to readfn
#' @export
#'
#' @examples
#' \dontrun{
#' # get the model datasets included in the package
#' model_datasets <- model_datasets
#'
#' # download just the zip
#' g <- get_datasets(
#' dataset_filenames = model_datasets$FileName[1],
#' download_option = "zip"
#' )
#'
#' # and then read from the zip. This function is used internally by rdhs
#' # when using `get_datasets` with `download_option = .rds` (default)
#' r <- read_zipdata(
#' g[[1]], pattern = ".dta"
#' )
#'
#' # and we can pass a function to read the file and any other args with ...
#' r <- read_zipdata(
#' g[[1]], pattern = ".dta", readfn = haven::read_dta, encoding = "UTF-8"
#' )
#' }
read_zipdata <- function(zfile, pattern=".dta$",
                         readfn=haven::read_dta, ...) {

  tmp <- tempfile()
  on.exit(unlink(tmp))
  file <- grep(pattern, unzip(zfile, list = TRUE)$Name,
               ignore.case = TRUE, value = TRUE)

  if (!length(file)) {
    warning(paste0("File name matching pattern '", pattern,
                   "' not found in zip file '", basename(zfile), "'."))
    return(invisible(NULL))
  }

  if (length(file) > 1) {
    warning(paste0("Multiple file names match pattern '", pattern,
                   "' in zip file '", basename(zfile), "'. Returning file '",
                   file[1], "'."))
  }

  return(readfn(unzip_special(zfile, file[1], exdir = tmp), ...))
}


## create simplest unique filenames that handles the india duplication.
#' @noRd
create_new_filenames <- function(data) {

  data$file <- strsplit(data$FileName, ".", fixed = TRUE) %>%
    lapply(function(x) x[1]) %>%
    unlist()

  filename_stems <- substr(data$FileName, 1, 2)
  issues <- which(!(tolower(filename_stems) == tolower(data$DHS_CountryCode)))
  data$file[issues] <- paste0(data$file[issues], "_", data$CountryName[issues])
  data
}

## what dataset labelling type is it
#' @noRd
dataset_label_type <- function(data, stop = TRUE) {

  # what kind of dataset is it we are working with
  if (is.element("label.table", attributes(data) %>% names())) {
    type <- "foreign"
  } else if (any(lapply(data, class) %>% unlist() == "haven_labelled") &&
             packageVersion("haven") > "1.1.2") {
    type <- "labelled"
  } else if (any(lapply(data, class) %>% unlist() == "labelled") &&
             packageVersion("haven") <= "1.1.2") {
    type <- "labelled"
  } else if (any(lapply(data,attributes) %>%
                 lapply(names) %>% unlist() =="label")) {
    type <- "label_only"
  } else if (stop) {
    stop("Dataset does not have a label.table attribute or any ",
         "labelled variable classes")
  } else {
    type <- "reformat"
  }

  return(type)

}
