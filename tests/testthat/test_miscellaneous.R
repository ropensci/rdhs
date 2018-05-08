context("Miscellaneous utils")

test_that("file_dataset_format", {
  dhs_file_formats <- c(
    "Flat ASCII data (.dat)",
    "Hierarchical ASCII data (.dat)",
    "SAS dataset (.sas7bdat) ",
    "SPSS dataset (.sav)",
    "Stata dataset (.dta)"
  )

  expect_identical(file_dataset_format(dhs_file_formats[1]), "dat")
  expect_identical(file_dataset_format(dhs_file_formats[2]), "dat")
  expect_identical(file_dataset_format(dhs_file_formats[3]), "sas7bdat")
  expect_identical(file_dataset_format(dhs_file_formats[4]), "sav")
  expect_identical(file_dataset_format(dhs_file_formats[5]), "dta")
})

# test the base version of rbindlist from data.table implemented for our uses
test_that("rbind_list_base", {
  l <- list()
  l[[1]] <- list("a" = 1, "b" = 2, "c" = 3)
  l[[2]] <- list("a" = 1, "b" = 2, "c" = 3)
  l <- rbind_list_base(l)

  expect_equal(dim(l), c(2, 3))
  expect_equal(names(l), c("a", "b", "c"))
  expect_equal(l$a, c(1, 1))

  l <- list()
  l[[1]] <- list()
  l <- rbind_list_base(l)
})

# test slow api
test_that("slow api response", {

  # if the response hasn't timed out without our doing then should be time
  resp <- last_api_update()
  if(resp != 0) {
  expect_true(inherits(resp,"POSIXlt"))
  }

  # now set the timeout super low, to try and mimic a slow cache
  Sys.setenv("rdhs_TIMEOUT" = 0)
  expect_equal(resp, -1)

  # set back to normal
  Sys.setenv("rdhs_TIMEOUT" = 30)

  })

