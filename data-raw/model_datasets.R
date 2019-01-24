library(rdhs)
url = "https://dhsprogram.com/data/download-model-datasets.cfm"

base_xml <- xml2::read_html(url)
urls <- paste0(
  "https://dhsprogram.com",
  rvest::html_attr(rvest::html_nodes(base_xml,"form a"), name = "href")
)
extract_dataset_type_code <- function(file) basename(file) %>% substr(start=3, stop=4)

file_names <- rvest::html_text(rvest::html_nodes(base_xml,"form a"))
file_types <- rvest::html_text(rvest::html_nodes(base_xml,".td_style32 strong"))
file_short <- extract_dataset_type_code(file_names) %>% unique
file_types <- file_types[match(extract_dataset_type_code(file_names), file_short)]
files <- rvest::html_text(rvest::html_nodes(base_xml,"td~ td+ td"))

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

# remove first row to remove
df <- data.frame("FileFormat" = files[-1],
                 "FileSize" = sizes[-1],
                 "DatasetType" = "Survey Datasets",
                 "SurveyNum" = NA,
                 "SurveyId" = NA,
                 "FileType" = file_types[-1],
                 "FileDateLastModified" = NA,
                 "SurveyYearLabel" = NA,
                 "SurveyType" = "DHS",
                 "SurveyYear" = "ModelDatasetSurveyYear",
                 "DHS_CountryCode" = "ZZ",
                 "FileName" = file_names[-1],
                 "CountryName" = "ModelDatasetCountry",
                 "URLS" = urls[-1],
                 stringsAsFactors = FALSE)
model_datasets <- df


devtools::use_data(model_datasets,overwrite=TRUE)


