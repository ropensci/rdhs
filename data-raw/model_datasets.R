
url = "https://dhsprogram.com/data/download-model-datasets.cfm"

base_xml <- xml2::read_html(url)
urls <- paste0(
  "https://dhsprogram.com",
  rvest::html_attr(rvest::html_nodes(base_xml,"form a"), name = "href")
)

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
                    httr::user_agent("https://github.com/OJWatson/rdhs"),
                    httr::write_disk(tf, overwrite = TRUE)
  )
  sizes[i] <- file.size(tf)
}

df <- data.frame("FileFormat" = files,
                 "FileSize" = sizes,
                 "DatasetType" = "Survey Datasets",
                 "SurveyNum" = NA,
                 "SurveyId" = NA,
                 "FileType" = file_types,
                 "FileDateLastModified" = NA,
                 "SurveyYearLabel" = NA,
                 "SurveyType" = "DHS",
                 "SurveyYear" = "ModelDatasetSurveyYear",
                 "DHS_CountryCode" = "ZZ",
                 "FileName" = file_names,
                 "CountryName" = "ModelDatasetCountry",
                 "URLS" = urls)
model_datasets <- df

devtools::use_data(model_datasets,overwrite=TRUE)


