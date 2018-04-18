context("Miscellaneous utils")

test_that("file_dataset_format", {

  dhs_file_formats <- c("Flat ASCII data (.dat)",
                        "Hierarchical ASCII data (.dat)",
                        "SAS dataset (.sas7bdat) ",
                        "SPSS dataset (.sav)",
                        "Stata dataset (.dta)")

  expect_identical(file_dataset_format(dhs_file_formats[1]),"dat")
  expect_identical(file_dataset_format(dhs_file_formats[2]),"dat")
  expect_identical(file_dataset_format(dhs_file_formats[3]),"sas7bdat")
  expect_identical(file_dataset_format(dhs_file_formats[4]),"sav")
  expect_identical(file_dataset_format(dhs_file_formats[5]),"dta")

})
