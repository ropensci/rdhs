library(rdhs)
url = "https://dhsprogram.com/data/download-model-datasets.cfm"

base_xml <- xml2::read_html(url)

extract_dataset_type_code <- function(file) basename(file) %>% substr(start=3, stop=4)

file_names <- rvest::html_text(rvest::html_nodes(base_xml,"form a"))
file_types <- rvest::html_text(rvest::html_nodes(base_xml,".td_style32 strong"))
type_codes <- extract_dataset_type_code(file_names)

type_key <- c("fu" = "Model Datasets Full Report Tables and Sampling Errors (English and French)",
              "ta" = "Model Datasets Full Report Tables and Sampling Errors (English and French)",
              "br" = "Births Recode",
              "cr" = "Couples' Recode",
              "hr" = "Household Recode",
              "ir" = "Individual Recode", 
              "kr" = "Children's Recode",
              "mr"= "Men's Recode",
              "pr" = "Household Member Recode", 
              "ar" = "HIV Test Results Recode")
stopifnot(file_types %in% type_key)

file_types <- unname(type_key)[match(type_codes, names(type_key))]
files <- rvest::html_text(rvest::html_nodes(base_xml,"td~ td+ td"))

urls <- paste0(
  "https://dhsprogram.com/data/model_data/",
  vapply(type_codes, switch, character(1),
         "fu" = "tables", "ta" = "tables", "ar" = "hiv", "dhs"),
  "/", file_names
)

sizes <- rep(0,length(urls))
tf <- tempfile()
for(i in 1:length(sizes)){
  message(i)
  resp <- httr::GET(urls[i],
                    destfile = tf,
                    httr::user_agent("https://github.com/ropensci/rdhs"),
                    httr::write_disk(tf, overwrite = TRUE)
  )
  sizes[i] <- file.size(tf)
}

## remove first two rows to remove PDF table zips
df <- data.frame("FileFormat" = files[-(1:2)],
                 "FileSize" = sizes[-(1:2)],
                 "DatasetType" = "Survey Datasets",
                 "SurveyNum" = NA,
                 "SurveyId" = NA,
                 "FileType" = file_types[-(1:2)],
                 "FileDateLastModified" = NA,
                 "SurveyYearLabel" = NA,
                 "SurveyType" = "DHS",
                 "SurveyYear" = "ModelDatasetSurveyYear",
                 "DHS_CountryCode" = "ZZ",
                 "FileName" = file_names[-(1:2)],
                 "CountryName" = "ModelDatasetCountry",
                 "URLS" = urls[-(1:2)],
                 stringsAsFactors = FALSE)
model_datasets <- df

usethis::use_data(model_datasets, overwrite=TRUE)

## NOTE: I'm not sure why model_datasets is saved both as external and internal dataset...

api_key_internal <- "ICLSPH-527168"
usethis::use_data(api_key_internal, model_datasets, internal = TRUE, overwrite = TRUE)
