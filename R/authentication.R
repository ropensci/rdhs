##' Create a data frame of surveys that your log in can download
##'
##' @title DHS surveys that can be downloaded
##' @param your_email Character for email address for DHS website
##' @param your_password Character for password for DHS website
##' @param your_project Character string for the name of your project that gives you access to the DHS database
##' @param output_dir Character for file directory where surveys will be downloaded to when using \code{survey_download}.
##' @param max_urls Numeric or maximum number of urls to generate. Default = NULL, which is all
##'
##' @note Credit for function to \url{https://github.com/ajdamico/lodown/blob/master/R/dhs.R}
##'
##' @return Returns \code{"data.frame"} of length 3:
##' \itemize{
##'       \item{"country"}{Survey Country}
##'       \item{"year"}{Survey year}
##'       \item{"Apr_Ctry_list_id"}{Unique numeric for your project for auth purposes}
##'       \item{"output_folder"}{Folder root directory for where surveys will be downloaded to, based on \strong{output_dir} argument}
##'       \item{"full_url"}{Full file url for survey download purposes}
##'       \item{"file_format"}{File format for full_url}
##'       }
##'
##' @export
##'
##' @examples rdhs:::available_surveys(your_email="rdhs.tester@gmail.com",
##' your_password="rdhstesting",
##' your_project="Testing Malaria Investigations",
##' output_dir=tempdir(), max_urls = 1)
##'
##'
##'
available_surveys <- function(  your_email , your_password , your_project ,
                                   output_dir , max_urls = NULL){


  if(is.null(max_urls)) max_urls <- Inf
  url_count <- 0

  catalog <- NULL

  tf <- tempfile()

  values <- dhs_authenticate( your_email , your_password , your_project )

  project.number <- values$proj_id

  # re-access the download-datasets page
  z <- httr::POST( "https://dhsprogram.com/data/dataset_admin/download-datasets.cfm" , body = list( proj_id = project.number ) )

  # write the information from the `countries` page to a local file
  writeBin( z$content , tf )

  # load the text
  y <- readLines( tf , warn = FALSE )

  # figure out the country lines
  country_lines <- unique( grep( 'notranslate' , y , value = TRUE ) )

  # figure out which countries are available for download
  country.names <- gsub( "(.*)>(.*)<(.*)" , "\\2" , country_lines )
  country.numbers <- gsub( '(.*)value = \"(.*)\"(.*)' , "\\2" , country_lines )


  # loop through each available country #
  for ( j in seq( length( country.numbers ) ) ){

    # extract the current country number..
    this.number <- country.numbers[ j ]
    # ..and current country name
    this.name <- country.names[ j ]

    # create the country directory on the local disk
    # dir.create( paste0( "./" , this.name ) )



    # create a website key pointing the specific country
    values <-
      list(
        proj_id = project.number ,
        Apr_Ctry_list_id = this.number ,
        submitted = 2 ,
        action = "View Surveys" ,
        submit = "View Surveys"
      )

    # re-access the download data page
    # using the new country-specific key
    z <- httr::POST( "https://dhsprogram.com/data/dataset_admin/download-datasets.cfm" , body = values )

    # pull all links
    link.urls <- XML::xpathSApply( XML::htmlParse( httr::content( z ) ) , "//a" , XML::xmlGetAttr , "href" )

    # extract all links containing the current country's name
    valid.surveys <- grep( "?flag=1" , link.urls )
    link.urls <- unlist( link.urls [ valid.surveys ] )

    # loop through each available data set within the country #
    for ( this.link in link.urls ){

      # access each dataset's link
      z <- httr::GET( paste0( "https://dhsprogram.com" , this.link ) )

      writeBin( z$content , tf )

      # read the table from each country page, remove the country name, and remove extraneous characters
      this.title <- gsub( ": |," , "" , gsub( this.name , "" , gsub( '(.*)surveyTitle\">(.*)<(.*)' , "\\2" , grep( 'surveyTitle\">' , readLines( tf , warn = FALSE ) , value = TRUE ) ) ) )

      # create a dataset-specific folder within the country folder within the current working directory
      # dir.create( paste0( "./" , this.name , "/" , this.title ) )

      # store all dataset-specific links
      all.links <- XML::xpathSApply( XML::htmlParse( httr::content( z ) ) , "//div//a" , XML::xmlGetAttr , "href" )

      # keep only /data/dataset/ links
      data.link <- unique( all.links[ grepl( "customcf/legacy/data/download_dataset" , all.links ) ] )

      # directory path
      # this_dir <- paste0( "./" , this.name , "/" , this.title )

      for( file.url in unlist( data.link ) ){

        this_catalog <-
          data.frame(
            country = this.name ,
            year = substr( gsub( "[^0-9]" , "" , this.title ) , 1 , 4 ) ,
            proj_id = project.number ,
            Apr_Ctry_list_id = this.number ,
            output_folder = file.path(output_dir , this.name , this.title ) ,
            full_url = paste0( "https://dhsprogram.com" , file.url ) ,
            stringsAsFactors = FALSE
          )

        catalog <- rbind( catalog , this_catalog )

        url_count <- url_count + 1

        if(url_count == max_urls) break
      }
      if(url_count == max_urls) break
    }
    if(url_count == max_urls) break
  }

  unique( catalog )

}



##' Create a data frame of surveys that your log in can download
##'
##' @title Download surveys specified using output of \code{available_surveys}
##' @param your_email Character for email address for DHS website
##' @param your_password Character for password for DHS website
##' @param your_project Character string for the name of your project that gives you access to the DHS database
##' @param desired_surveys output or rows from \code{available_surveys}
##'
##' @note Credit for function to \url{https://github.com/ajdamico/lodown/blob/master/R/dhs.R}
##'
##' @export
##'
##' @examples
##'
##' dhs_surveys <- rdhs:::available_surveys(your_email="rdhs.tester@gmail.com",
##' your_password="rdhstesting",
##' your_project="Testing Malaria Investigations",
##' output_dir=tempdir(),
##' max_urls=2)
##'
##' download_datasets(your_email="rdhs.tester@gmail.com",
##' your_password="rdhstesting",
##' your_project="Testing Malaria Investigations",
##' desired_surveys=dhs_surveys[1,])
##'
##'
##'
download_datasets <- function(   your_email , your_password , your_project ,
                                 desired_surveys ){


  ## TODO::

  ## Add client as an argument, and then check against key from the desired_survey call

  # log in
  values <- dhs_authenticate( your_email , your_password , your_project )

  project.number <- values$proj_id

  tf <- tempfile()

  # re-access the download-datasets page
  z <- httr::POST( "https://dhsprogram.com/data/dataset_admin/download-datasets.cfm" , body = list( proj_id = project.number ) )

  for ( i in seq_len( nrow( desired_surveys ) ) ){

    # create a website key pointing the specific country
    values <-
      list(
        proj_id = desired_surveys[ i , "proj_id" ] ,
        Apr_Ctry_list_id = desired_surveys[ i , "Apr_Ctry_list_id" ] ,
        submitted = 2 ,
        action = "View Surveys" ,
        submit = "View Surveys"
      )

    # re-access the download data page
    # using the new country-specific key
    z <- httr::POST( "https://dhsprogram.com/data/dataset_admin/download-datasets.cfm" , body = values)

    # download the actual microdata file directly to disk
    # don't read it into memory.  save it as `tf` immediately (RAM-free)
    httr::GET(desired_surveys[ i , 'full_url' ] , destfile = tf, httr::write_disk( tf , overwrite = TRUE ) , httr::progress() )

    # make sure the file-specific folder exists
    dir.create( normalizePath( desired_surveys[ i , 'output_folder' ] ) , showWarnings = FALSE, recursive = T )

    # unzip the contents of the zipped file
    unzipped_files <- unzip_warn_fails( tf , exdir = normalizePath( desired_surveys[ i , 'output_folder' ] ) )

    # some zipped files contained zipped subfiles
    for( this_zip in grep( "\\.zip$" , unzipped_files , ignore.case = TRUE , value = TRUE ) ){

      unzipped_files <- unzipped_files[ unzipped_files != this_zip ]

      unzipped_files <- c( unzipped_files , unzip_warn_fails( this_zip , exdir = normalizePath( desired_surveys[ i , 'output_folder' ] ) ) )

    }

    # remove files with the same names
    unzipped_files <- unzipped_files[ !duplicated( tolower( unzipped_files ) ) ]


    # # and now, if there's a stata file, import it!
    # if ( any( st <- grepl( "\\.dta$" , tolower( unzipped_files ) ) ) ){
    #
    #   for( this_dta in unzipped_files[ which( st ) ] ){
    #
    #     # remove any prior `x` tables ; clear up RAM
    #     suppressWarnings( { rm( x ) ; gc() } )
    #
    #     # figure out the correct location for the rds
    #     rds_name <- file.path( catalog[ i , 'output_folder' ] , gsub( "\\.dta$" , ".rds" , basename( this_dta ) , ignore.case = TRUE ) )
    #
    #     # load the current stata file into working memory
    #     attempt_one <- try( x <- data.frame( haven::read_dta( this_dta ) ) , silent = TRUE )
    #
    #     if( class( attempt_one ) == 'try-error' ) x <- foreign::read.dta( this_dta , convert.factors = FALSE )
    #
    #     # convert all column names to lowercase
    #     names( x ) <- tolower( names( x ) )
    #
    #     catalog[ i , 'case_count' ] <- max( catalog[ i , 'case_count' ] , nrow( x ) , na.rm = TRUE )
    #
    #     # save the file on the local disk, within the appropriate country-survey filepath
    #     saveRDS( x , file = rds_name ) ; rm( x ) ; gc()
    #
    #   }
    #
    # }
    #
    # # if a file has not been saved as an rds yet,
    # # look for an spss file as well.  this way, stata always takes priority.
    # if ( !exists( 'rds_name' ) || !file.exists( rds_name ) ){
    #
    #   # if there's any spss file, import it!
    #   if ( any( st <- grepl( "\\.sav$" , tolower( unzipped_files ) ) ) ){
    #
    #     for( this_sav in unzipped_files[ which( st ) ] ){
    #
    #       # remove any prior `x` tables ; clear up RAM
    #       suppressWarnings( { rm( x ) ; gc() } )
    #
    #       # figure out the correct location for the rds
    #       rds_name <- file.path( catalog[ i , 'output_folder' ] , gsub( "\\.sav$" , ".rds" , basename( this_sav ) , ignore.case = TRUE ) )
    #
    #       # load the current stata file into working memory
    #       x <- data.frame( haven::read_spss( this_sav ) )
    #
    #       # convert all column names to lowercase
    #       names( x ) <- tolower( names( x ) )
    #
    #       catalog[ i , 'case_count' ] <- max( catalog[ i , 'case_count' ] , nrow( x ) , na.rm = TRUE )
    #
    #       # save the file on the local disk, within the appropriate country-survey filepath
    #       saveRDS( x , file = rds_name ) ; rm( x ) ; gc()
    #
    #     }
    #   }
    # }

    # delete the temporary file
    suppressWarnings( file.remove( tf ) )

    message( paste0( "\n\n" , "DHS" , " survey entry " , i , " of " , nrow( desired_surveys ) , " stored in '" , desired_surveys[ i , 'output_folder' ] , "'\r\n\n" ) )



  }


}




##' Autheticate Users for DHS website
##'
##' @title DHS Wesbite Authentication
##' @param your_email Character for email address for DHS website
##' @param your_password Character for password for DHS website
##' @param your_project Character string for the name of your
##' project that gives you access to the DHS database
##'
##'
##' @note Credit for function to \url{https://github.com/ajdamico/lodown/blob/master/R/dhs.R}
##'
##' @return Returns list of length 3:
##' \itemize{
##'       \item{"user_name"}{ your email usually}
##'       \item{"user_pass"}{ your pasword you provided}
##'       \item{"proj_id"}{ your project number that will enable your project to be accessed downstream}
##'       }
##'
##' @examples
##'
##' rdhs:::dhs_authenticate(your_email="rdhs.tester@gmail.com",your_password="rdhstesting",
##' your_project="Testing Malaria Investigations")
##'
##'
##'

dhs_authenticate <- function( your_email , your_password , your_project ){


  # Argument Checking
  if(!is.character(your_email)) stop ("your_email is not a character string")
  if(!is.character(your_project)) stop ("your_password is not a character string")
  if(!is.character(your_password)) stop ("your_project is not a character string")

  # authentication page
  terms <- "https://dhsprogram.com/data/dataset_admin/login_main.cfm"

  # countries page
  countries.page <- "https://dhsprogram.com/data/dataset_admin/download-datasets.cfm"

  # create a temporary file and temporary directory
  tf <- tempfile(fileext = ".txt")

  # set the username and password
  values <-
    list(
      UserName = your_email ,
      UserPass = your_password ,
      Submitted = 1 ,
      UserType = 2
    )

  # log in.
  httr::GET( terms , query = values )
  httr::POST( terms , body = values )

  # extract the available countries from the projects page
  z <- httr::GET( countries.page )

  # write the information from the `projects` page to a local file
  writeBin( z$content , tf )

  # load the text
  y <- readLines( tf , warn = FALSE )

  # figure out the project number - only use first 30 chars due to ellipsis formation
  project.line <- unique( y[ grepl( "option value" , y ) &
                               grepl( paste0(strsplit(your_project,"")[[1]][1:30],collapse="") , y , fixed = TRUE ) ] )

  # confirm only one project
  stopifnot( length( project.line ) == 1 )

  # extract the project number from the line above
  project.number <- gsub( "(.*)<option value=\"([0-9]*)\">(.*)" , "\\2" , project.line )

  # log in again, but specifically with the project number
  list(
    user_name = your_email ,
    user_pass = your_password ,
    proj_id = project.number
  )

}
