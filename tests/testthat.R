library(testthat)
library(rdhs)

Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)
test_file("test_credentials.R")
test_check("rdhs")
