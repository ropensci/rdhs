# check to see if the package client exists and is le
check_for_client <- function(){
  if(is.null(.rdhs$client)) stop("Please set up your DHS credentials first using set_dhs_credentials()")
}

#' @title Get Datasets
#' Downloads datasets you have access to from the DHS website
#'
#' @rdname get_datasets
#' @details
#'   Gets datasets from your cache or dwonlaods from the DHS website. By providing the filenames, as specified in one of the
#'   returned fields from \code{\link{dhs_datasets}}, the client will log in for you and download all
#'   the files you hae requested. If any of the requested files are unavailable for your log in, these
#'   will be flagged up first as a message so you can make a note and request them through the DHS website.
#'   You also have the option to control whether the downloaded zip file is then extracted and converted
#'   into a more convenient R \code{data.frame}. This converted object will then be subsequently saved as
#'   a ".rds" object within the client root directory datasets folder, which can then be more quickly loaded
#'   when needed with \code{readRDS}. You also have the option to reformat the dataset, which will ensure that the
#'   datasets returned are encoded simply as character strings, i.e. there are no factors or labels.
#'
#' @param dataset_filenames
#'   The desired filenames to be downloaded. These can be found as one of the
#'   returned fields from \code{\link{dhs_datasets}}. Alternatively you can also pass the desired rows from
#'   \code{\link{dhs_datasets}}.
#' @param download_option
#'   Character specifying whether the dataset should be just downloaded ("zip"),
#'   imported and saved as an .rds object ("rds"), or both extract and rds ("both").
#'   Conveniently you can just specify any letter from these options.
#'   reformat
#' @param Boolean concerning whether to reformat read in datasets by removing all factors and labels.
#'   Default = FALSE.
#' @param all_lower
#'   Logical indicating whether all value labels should be lower case. Default to `TRUE`.
#' @param output_dir_root
#'   Root directory where the datasets will be stored within. The default will download
#'   datasets to a subfolder of the client root called "datasets"
#' @param ...
#'   Any other arguments to be passed to \code{\link{read_dhs_dataset}}
#' @return
#'   Depends on the download_option requested, but ultimately it is a file path to where the dataset
#'   was downloaded to, so that you can interact with it accordingly.
#' @export
#'

get_datasets <- function(dataset_filenames,
                         download_option="rds",
                         reformat=FALSE,
                         all_lower=TRUE,
                         output_dir_root=file.path(private$root,"datasets"),
                         ...){

  check_for_client()
  .rdhs$client$get_datasets(dataset_filenames,
                            download_option=download_option,
                            reformat=reformat,
                            all_lower=all_lower,
                            output_dir_root=output_dir_root,
                            ...)
}


#' @title Get Variable Labels
#' Returns a data.frame of
#'
#' @rdname get_downloaded_datasets
#' @details
#'   Returns a \code{data.frame} of the datasets that have been downloaded within this client.
#'   This could be useful if you are without an interent connection and wish to know which saved
#'   dataset files in your root diretory correspond to which dataset
#' @return
#'   Data.frame of downlaoded datasets
#' @export
#'

get_downloaded_datasets <- function() {

  check_for_client()
  .rdhs$client$get_downloaded_datasets()

}


#' @title Get Downloaded Datasets
#' Detail the datasets that you have already downloaded
#'
#' @rdname get_downloaded_datasets
#' @details
#'   Returns a \code{data.frame} of the datasets that have been downloaded within this client.
#'   This could be useful if you are without an interent connection and wish to know which saved
#'   dataset files in your root diretory correspond to which dataset
#' @return
#'   Data.frame of downlaoded datasets
#' @export
#'

get_downloaded_datasets <- function() {

  check_for_client()
  .rdhs$client$get_downloaded_datasets()

}


#' @title Get Available Datasets
#' Details the datasets that your login credentials have access to
#'
#' @rdname get_available_datasets
#' @details
#'   Searches the DHS website for all the datasets that you can donwload. The results
#'   of this function are cached in the client. If you have recently reqeusted new datasets
#'   from the DHS website then you can specify to clear the cache first so that you get the new
#'   set of datasets avaialble to you. This function is used by \code{\link{get_datasets}} and should
#'   thus be used with `clear_cache_first = TRUE` before using `get_datasets` if you have recently
#'   requested new datasets.
#'
#' @param clear_cache_first
#'   Boolean detailing if you would like to clear the cached available datasets first.
#'   The default is set to FALSE. This option is available so that you can make sure your
#'   client fetches any new datasets that you have recently been given access to.
#' @return
#'   Data.frame object with 14 variables that detail the surveys you can download,
#'   their url download links and the country, survey, year etc info for that link.
#' @export
#'

get_available_datasets <- function() {

  check_for_client()
  .rdhs$client$available_datasets()

}

#' @title Extract Data
#' Extracts data from your downloaded datasets according to a data.frame of requested survey variables or survey definitions
#'
#' @rdname extract_dhs
#' @details
#'   Function to extract datasets using a set of survey questions as taken from the output from \code{\link{search_variables}}
#'   or \code{\link{search_variable_labels}}
#'
#' @param questions
#'   Questions to be queried, in the format from \code{\link{search_variables}} or \code{\link{search_variable_labels}}
#' @param add_geo
#'   Add geograpic information to the extract. Defaut = `TRUE`
#' @export
#'

extract_dhs <- function(questions, add_geo=FALSE){

  check_for_client()
  .rdhs$client$extract(questions,add_geo = add_geo)

}

#' @title Search Survey Variables
#' Searches across datasets specified for requested survey variables. This function (or \code{\link{search_variable_labels}})
#' should be used to provide the `questions` argument for \code{\link{extract_dhs}}.
#'
#' @rdname search_variables
#' @details
#'   Use this function after \code{\link{get_datasets}} to look up all the survey variables that have the required variable.
#'
#' @param dataset_filenames
#'   The desired filenames to be downloaded. These can be found as one of the
#'   returned fields from \code{\link{dhs_datasets}}.
#' @param variables
#'   Character vector of survey variables to be looked up
#' @param essential_variables
#'   Character vector of variables that need to present. If any of the codes are not present in that survey,
#'   the survey will not be returned by this functon. Default = `NULL`.
#' @param ...
#'   Any other arguments to be passed to \code{\link{download_datasets}}
#' @return
#'   Data frame of the surveys where matches were found and then all the resultant codes and descriptions.
#' @export
#'

search_variables <- function(dataset_filenames,
                             variables,
                             essential_variables = NULL,
                             ...){

  check_for_client()
  .rdhs$client$survey_variables(dataset_filenames=dataset_filenames,
                                variables=variables,
                                essential_variables = essential_variables,
                                ...)

}

#' @title Search Survey Variable Definitions
#' Searches across datasets specified for requested survey variable definitions. This function (or
#' \code{\link{search_variable_labels}}) should be used to provide the `questions` argument for
#' \code{\link{extract_dhs}}.
#'
#' @rdname search_variable_labels
#' @details
#'   Use this function after \code{\link{get_datasets}} to query downloaded datasets for what survey questions they asked.
#'   This function will look for your downloaded and imported survey datasets from your cached files, and will
#'   download them if not previously downloaded.
#'
#' @param dataset_filenames
#'   The desired filenames to be downloaded. These can be found as one of the
#'   returned fields from \code{\link{dhs_datasets}}.
#' @param search_terms
#'   Character vector of search terms. If any of these terms are found within the survey question
#'   definitions, the corresponding survey variable and definitions will be returned.
#' @param regex
#'   Regex character pattern for matching. If you want to specify your regex search pattern, then specify
#'   this argument. N.B. If both `search_terms` and `regex`` are supplied as arguments then regex will be ignored.
#' @param essential_terms
#'   Character pattern that has to be in the definitions of survey question definitions. I.e. the function will first find
#'   all survey variable definitions that contain your `search_terms` (or regex) OR `essential_terms`. It will then remove any questions
#'   that did not contain your `essential_terms`. Default = `NULL`.
#' @param ...
#'   Any other arguments to be passed to \code{\link{download_datasets}}
#' @return
#'   Data frame of the surveys where matches were found and then all the resultant codes and descriptions.
#' @export
#'

search_variable_labels <- function(dataset_filenames,
                                        search_terms = NULL,
                                        essential_terms = NULL,
                                        regex = NULL,
                                        ...){

  check_for_client()
  .rdhs$client$survey_questions(dataset_filenames,
                                search_terms = search_terms,
                                essential_terms = essential_terms,
                                regex = regex,
                                ...)


}

#' @title Get Survey Variable Labels
#' Return variable labels from a dataset
#'
#' @rdname get_var_labels
#'
#' @details Returns variable names and their labels from a dataset. You can pass for the `data` argument any of
#' the following:
#' \itemize{
#'       \item The file path to a saved dataset. This would be the direct output of \code{\link{get_datasets}}
#'       \item A read in dataset, i.e. produced by using \code{\link{readRDS}} to load a dataset from
#'       a file path produced by \code{\link{get_datasets}}
#'       \item Dataset filenames. These can be found as one of the returned fields from \code{\link{dhs_datasets}}.
#'       If these datasets have not been downloaded before this will download them for you.
#'       }
#'
#'
#' @param dataset Can be either the file path to a dataset, the dataset as a `data.frame` or the filenaes of datasets.
#' See details for more information
#' @param return_all Logical whether to return all variables (\code{TRUE}) or only those with labels.
#'
#' @return A \code{data.frame} consisting of the variable name and labels.
#'
#' @export
get_var_labels <- function(data, return_all=TRUE) {

  # if it is a recognised dhs_dataset data.frame then we read the labels from that
  if(all(class(res) %in% c("dhs_dataset","dhs_dataset"))){

    res <- get_labels_from_dataset(data, return_all)

  } else if (is.character(data)){

    # if the file exists we'll pass them through as dataset_paths, otherise as filenames
    if(any(file.exists(data))){
      res <-  .rdhs$client$get_var_labels(dataset_paths = data[file.exists(data)])
    } else {
      res <-  .rdhs$client$get_var_labels(dataset_filenames = data[file.exists(data)])
    }

  }

  return(res)

}

#' Create list of dataset and its variable names
#'
#' Function to give the former output of get_datasets as it can be nice to have both the
#' definitions and the dataset attached together
#'
#' @param dataset Any read in dataset created by \code{get_datasets}, either as the file path or after having
#' been read using \code{readRDS}
#' @export
data_and_labels <- function(dataset){

  if(class(dataset)=="character"){
    if(file.exists(dataset)){
      dataset <- readRDS(dataset)
    } else {
      stop("Invalid dataset argument. Must be a character string to where a dataset has been saved, or a read in dataset.",
           "See ?data_and_labels for more inforation")
    }
  }
  variable_names <- get_var_labels(dataset)
  res <- list("dataset"=dataset,"variable_names"=variable_names)

}
