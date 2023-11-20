# check for the client and create if needed
#' @noRd
check_for_client <- function() {

  # have we done setup before
  if (is.null(.rdhs$client)) {
    client <- rdhs_setup()
    if (is_r_tempdir(client$get_root())) {
      rdhs_setup_message(
        verbose = client$get_config()$verbose_setup,
        "You have not granted permision to rdhs to write outside of \n",
        "your temporary directory. As a result any datasets or API  \n",
        "calls will not be saved after you close this R sessiton. \n",
        "To cache your results please use set_rdhs_config()"
      )
    } else {
      rdhs_setup_message(
        verbose = client$get_config()$verbose_setup,
        "Your datasets and API calls will be cached here: \n",
        "   -> ", client$get_root(), "\n",
        "Your datasets will be downloaded using the following config:\n"
        )
      print_rdhs_config(client$get_config())
    }
  } else {
    client <- .rdhs$client
  }

  return(invisible(client))

}

#' @noRd
rdhs_package_client_exists <- function() {
  is.null(.rdhs$client)
}

#' Get rdhs config
#'
#' Gets the rdhs config being used
#'
#' @rdname get_rdhs_config
#'
#' @details Returns the config being used by rdhs at the moment. This will
#'   either be a `data.frame` with class `rdhs_config` or will be NULL if
#'   this has not been set up yet
#'
#' @return A \code{data.frame} containing your rdhs config
#' @export
#'

get_rdhs_config <- function() {

  client <- .rdhs$client
  if (is.null(client)) {
    return(NULL)
  } else {
    return(client$get_config())
  }
}


#' Get Datasets
#'
#' Downloads datasets you have access to from the DHS website
#'
#' @param dataset_filenames The desired filenames to be downloaded. These can be
#'   found as one of the returned fields from \code{\link{dhs_datasets}}.
#'   Alternatively you can also pass the desired rows from
#'   code{\link{dhs_datasets}}.
#' @param download_option Character specifying whether the dataset should be
#'   just downloaded ("zip"), imported and saved as an .rds object ("rds"), or
#'   both extract and rds ("both"). Conveniently you can just specify any letter
#'   from these options.
#' @param reformat Boolean concerning whether to reformat read in datasets by
#'   removing all factors and labels. Default = FALSE.
#' @param all_lower Logical indicating whether all value labels should be lower
#' case. Default to `TRUE`.
#' @param output_dir_root Root directory where the datasets will be stored
#'   within. The default will download
#'   datasets to a subfolder of the client root called "datasets"
#' @param clear_cache Should your available datasets cache be cleared first.
#'   This will allow newly accessed datasets to be available. Default = `FALSE`
#' @param ... Any other arguments to be passed to \code{\link{read_dhs_dataset}}
#'
#' @rdname get_datasets
#'
#' @details Gets datasets from your cache or downloads from the DHS website.
#'   By providing the filenames, as specified in one of the returned fields
#'   from \code{\link{dhs_datasets}}, the client will log in for you and download
#'   all the files you have requested. If any of the requested files are
#'   unavailable for your log in, these will be flagged up first as a message so
#'   you can make a note and request them through the DHS website. You also have
#'   the option to control whether the downloaded zip file is then extracted and
#'   converted into a more convenient R \code{data.frame}. This converted object
#'   will then be subsequently saved as a ".rds" object within the client root
#'   directory datasets folder, which can then be more quickly loaded when
#'   needed with \code{readRDS}. You also have the option to reformat the
#'   dataset, which will ensure that the datasets returned are encoded simply
#'   as character strings, i.e. there are no factors or labels.
#'
#' @return Depends on the download_option requested, but ultimately it is a file
#'   path to where the dataset was downloaded to, so that you can interact with
#'   it accordingly.
#' @export
#'
#' @examples
#'  \dontrun{
#' # get the model datasets included with the package
#' model_datasets <- model_datasets
#'
#' # download one of them
#' g <- get_datasets(dataset_filenames = model_datasets$FileName[1])
#' }

get_datasets <- function(dataset_filenames,
                         download_option="rds",
                         reformat=FALSE,
                         all_lower=TRUE,
                         output_dir_root=NULL,
                         clear_cache=FALSE,
                         ...) {

  client <- check_for_client()
  if (is.null(output_dir_root)) {
    output_dir_root <- file.path(client$get_root(), "datasets")
  }
  client$get_datasets(dataset_filenames,
                      download_option = download_option,
                      reformat = reformat,
                      all_lower = all_lower,
                      output_dir_root = output_dir_root,
                      clear_cache = clear_cache,
                      ...
  )
}


#' Get Downloaded Datasets
#'
#' Detail the datasets that you have already downloaded
#'
#' @rdname get_downloaded_datasets
#'
#' @details Returns a \code{data.frame} of the datasets that have been
#'   downloaded within this client. This could be useful if you are without
#'   an internet connection and wish to know which saved
#'   dataset files in your root directory correspond to which dataset
#'
#' @return A \code{data.frame} of downloaded datasets
#' @export
#'
#' @examples
#' \dontrun{
#' # get the model datasets included with the package
#' model_datasets <- model_datasets
#'
#' # download one of them
#' g <- get_datasets(dataset_filenames = model_datasets$FileName[1])
#'
#' # these will then be stored so that we know what datasets we have downloaded
#' d <- get_downloaded_datasets()
#'
#' # which returns a names list of file paths to the datasets
#' d[1]
#' }

get_downloaded_datasets <- function() {

  client <- check_for_client()
  client$get_downloaded_datasets()
}

#' Get Available Datasets
#'
#' Details the datasets that your login credentials have access to
#'
#' @rdname get_available_datasets
#'
#' @details Searches the DHS website for all the datasets that you can download.
#'   The results of this function are cached in the client. If you have recently
#'   requested new datasets from the DHS website then you can specify to clear
#'   the cache first so that you get the new set of datasets available to you.
#'   This function is used by \code{\link{get_datasets}} and should thus be
#'   used with `clear_cache_first = TRUE` before using `get_datasets` if you
#'   have recently requested new datasets.
#'
#' @param clear_cache Boolean detailing if you would like to clear the
#'   cached available datasets first. The default is set to FALSE. This option
#'   is available so that you can make sure your client fetches any new datasets
#'   that you have recently been given access to.
#' @return A \code{data.frame} with 14 variables that detail the surveys you can
#'   download, their url download links and the country, survey, year etc info
#'   for that link.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # grab the datasets
#' datasets <- get_available_datasets()
#'
#' # and if we look at the last one it will be the model datasets from DHS
#' tail(datasets, 1)
#' }

get_available_datasets <- function(clear_cache = FALSE) {

  client <- check_for_client()
  avs <- client$available_datasets(clear_cache)
  return(rbind(avs, rdhs::model_datasets))
}

#' Extract Data
#'
#' Extracts data from your downloaded datasets according to a data.frame of
#' requested survey variables or survey definitions
#'
#' @rdname extract_dhs
#'
#' @details Function to extract datasets using a set of survey questions as
#'   taken from the output from \code{\link{search_variables}}
#'   or \code{\link{search_variable_labels}}
#'
#' @param questions Questions to be queried, in the format from
#'   \code{\link{search_variables}} or \code{\link{search_variable_labels}}
#' @param add_geo Add geographic information to the extract. Defaut = `TRUE`
#'
#' @return A \code{list} of `data.frames` for each survey data extracted.
#' @export
#'
#' @examples
#' \dontrun{
#' # get the model datasets included with the package
#' model_datasets <- model_datasets
#'
#' # download one of them
#' g <- get_datasets(dataset_filenames = model_datasets$FileName[1])
#'
#' # create some terms of data me may want to extrac
#' st <- search_variable_labels(names(g), "bed net")
#'
#' # and now extract it
#' ex <- extract_dhs(st)
#' }

extract_dhs <- function(questions, add_geo=FALSE) {

  client <- check_for_client()
  client$extract(questions, add_geo = add_geo)
}

#' Search Survey Variables
#'
#' Searches across datasets specified for requested survey variables.
#' This function (or \code{\link{search_variable_labels}})
#' should be used to provide the `questions` argument
#' for \code{\link{extract_dhs}}.
#'
#' @rdname search_variables
#'
#' @details Use this function after \code{\link{get_datasets}} to look up all
#'   the survey variables that have the required variable.
#'
#' @param dataset_filenames The desired filenames to be downloaded.
#'   These can be found as one of the returned fields from
#'   \code{\link{dhs_datasets}}.
#' @param variables Character vector of survey variables to be looked up
#' @param essential_variables Character vector of variables that need to
#'   present. If any of the codes are not present in that survey,
#'   the survey will not be returned by this function. Default = `NULL`.
#' @param ... Any other arguments to be passed to
#'   \code{\link{download_datasets}}
#'
#' @return A \code{data.frame} of the surveys where matches were
#' found and then all the resultant codes and descriptions.
#' @export
#'
#' @examples
#' \dontrun{
#' # get the model datasets included with the package
#' model_datasets <- model_datasets
#'
#' # download two of them
#' g <- get_datasets(dataset_filenames = model_datasets$FileName[1:2])
#'
#' # and now seearch within these for survey variables
#' search_variables(
#' dataset_filenames = names(g), variables = c("v002","v102","ml13"),
#' )
#'
#' # if we specify an essential variable then that dataset has to have that
#' # variable or else no variables will be returned for that datasets
#' search_variables(
#' dataset_filenames = names(g),
#' variables = c("v002","v102","ml13"),
#' essential_variables = "ml13"
#' )
#' }
search_variables <- function(dataset_filenames,
                             variables,
                             essential_variables = NULL,
                             ...) {

  client <- check_for_client()
  client$survey_variables(dataset_filenames = dataset_filenames,
                          variables = variables,
                          essential_variables = essential_variables,
                          ...)
}

#' Search Survey Variable Definitions
#'
#' Searches across datasets specified for requested survey variable definitions.
#' This function (or \code{\link{search_variable_labels}}) should be used to
#' provide the `questions` argument for \code{\link{extract_dhs}}.
#'
#' @rdname search_variable_labels
#'
#' @details Use this function after \code{\link{get_datasets}} to query
#'   downloaded datasets for what survey questions they asked.
#'   This function will look for your downloaded and imported survey datasets
#'   from your cached files, and will download them if not downloaded.
#'
#' @param dataset_filenames The desired filenames to be downloaded. These can be
#'   found as one of the returned fields from \code{\link{dhs_datasets}}.
#' @param search_terms Character vector of search terms. If any of these terms
#'   are found within the survey question definitions, the corresponding survey
#'   variable and definitions will be returned.
#' @param regex Regex character pattern for matching. If you want to specify
#'   your regex search pattern, then specify this argument. N.B. If both
#'   `search_terms` and `regex`` are supplied as arguments then regex will
#'   be ignored.
#' @param essential_terms Character pattern that has to be in the definitions of
#'   survey question definitions. I.e. the function will first find
#'   all survey variable definitions that contain your `search_terms`
#'   (or regex) OR `essential_terms`. It will then remove any questions
#'   that did not contain your `essential_terms`. Default = `NULL`.
#' @param ... Any other arguments to be passed to
#'   \code{\link{download_datasets}}
#'
#' @return A \code{data.frame} of the surveys where matches were found
#'   and then all the resultant codes and descriptions.
#' @export
#'
#' @examples
#' \dontrun{
#' # get the model datasets included with the package
#' model_datasets <- model_datasets
#'
#' # download two of them
#' g <- get_datasets(dataset_filenames = model_datasets$FileName[1:2])
#'
#' # and now seearch within these for survey variable labels of interest
#' vars <- search_variable_labels(
#' dataset_filenames = names(g), search_terms = "fever"
#' )
#'
#' head(vars)
#'
#' # if we specify an essential term then no results will be returned from
#' # a dataset if it does not have any results from the search with this term
#' search_variable_labels(
#' dataset_filenames = names(g),
#' search_terms = "fever",
#' essential_terms = "primaquine",
#' )
#'
#' # we can also use regex queries if we prefer, by passing `regex = TRUE`
#' vars <- search_variable_labels(
#' dataset_filenames = names(g), search_terms = "fever|net", regex = TRUE
#' )
#' }

search_variable_labels <- function(dataset_filenames,
                                   search_terms = NULL,
                                   essential_terms = NULL,
                                   regex = NULL,
                                   ...) {
  client <- check_for_client()
  client$survey_questions(dataset_filenames,
                          search_terms = search_terms,
                          essential_terms = essential_terms,
                          regex = regex,
                          ...)
}

#' Get Survey Variable Labels
#'
#' Return variable labels from a dataset
#'
#' @rdname get_variable_labels
#'
#' @details Returns variable names and their labels from a dataset.
#'   You can pass for the `data` argument any of
#'   the following:
#'   \itemize{
#'       \item The file path to a saved dataset. This would be the direct
#'       output of \code{\link{get_datasets}}
#'       \item A read in dataset, i.e. produced by using \code{\link{readRDS}}
#'       to load a dataset from
#'       a file path produced by \code{\link{get_datasets}}
#'       \item Dataset filenames. These can be found as one of the returned
#'       fields from \code{\link{dhs_datasets}}. If these datasets have not been
#'       downloaded before this will download them for you.
#'       }
#'
#' @param dataset Can be either the file path to a dataset, the dataset as a
#'   `data.frame` or the filenames of datasets. See details for more information
#' @param return_all Logical whether to return all variables (\code{TRUE})
#'   or only those with labels.
#'
#' @return A \code{data.frame} consisting of the variable name and labels.
#' @export
#'
#' @examples
#' \dontrun{
#' # get the model datasets included with the package
#' model_datasets <- model_datasets
#'
#' # download one of them
#' g <- get_datasets(dataset_filenames = model_datasets$FileName[1])
#'
#' # we can pass the list of filepaths to the function
#' head(get_variable_labels(g))
#'
#' # or we can pass the full dataset
#' r <- readRDS(g[[1]])
#' head(get_variable_labels(r))
#' }
get_variable_labels <- function(dataset, return_all=TRUE) {

  # if it is a data.frame then we try to read the labels from that
  if (is.data.frame(dataset)) {
    res <- get_labels_from_dataset(dataset, return_all)
  } else if (is.character(dataset) || is.list(dataset)) {

    # get the file paths if it is in list form
    if (is.list(dataset)) {
      dataset <- unlist(dataset)
    }

    # grab our client
    client <- check_for_client()

    # if the file exists we'll pass them through as dataset_paths,
    # otherise as filenames
    if (any(file.exists(dataset))) {
      paths <- dataset[file.exists(dataset)]
      res <- client$get_variable_labels(dataset_paths = paths)
    } else {
      res <- client$get_variable_labels(dataset_filenames = dataset)
    }
  } else if (inherits(dataset, "sf")) {
    stop(
      "Not possible to return variable labels for a geographic data set.",
      "Have a look at the following for help on simple features objects: ",
      "https://r-spatial.github.io/sf/"
    )
  }

  return(res)
}
