library(testthat)
library(rdhs)

Sys.setenv("rdhs_RENVIRON_PERMISSION" = 1)
test_check("rdhs",filter = "credentials")
test_check("rdhs")
