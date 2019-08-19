# sourced from https://dhsprogram.com/What-We-Do/upload/measuredhs_gps_dataformat.pdf
dhs_gps_data_format <- read.csv2("data-raw/measuredhs_gps_dataformat.csv",sep = ",")
usethis::use_data(dhs_gps_data_format)
