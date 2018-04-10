
  # Create new directory
  td <- "D:/DHS_Clean"


  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client(api_key = "ICLSPH-527168",root = td)
  }

# create availbale surveys
  datasets <- dhs_datasets(fileFormat = "FL")

  # grab survey types needed
  downloads <- tryCatch(cli$download_datasets(dataset_filenames = datasets$FileName,download_option = "rds",
                                     reformat = TRUE,all_lower = TRUE))


  outputFile <-file("output.txt")
  downloads <- sapply(datasets$FileName[1613:length(datasets$FileName)], function(x){

    tryCatch({cli$download_datasets(dataset_filenames = x,download_option = "rds",
                                  reformat = TRUE,all_lower = TRUE)
  }, error = function(e) {
    write(paste0(x," - ", as.character(e)), "output.txt",append = TRUE)
  })})


    close(outputFile)


