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
  testthat::skip_on_cran()
  skip_if_no_auth()

  # if the response hasn't timed out without our doing then should be time
  resp <- last_api_update(30)
  if (resp != 0) {
  expect_true(inherits(resp, "POSIXlt"))
  }

  # now set the timeout super low, to try and mimic a slow cache
  resp <- last_api_update(0)
  expect_equal(resp, -0.5)

  })

test_that("type_convert_df", {
  df <- data.frame("huh" = c("apple", "apple", "orange"))
  df <- type_convert_df(df)
  expect_null(levels(df$huh))
})

test_that("different locales", {

  locale_lc_time <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", locale_lc_time))

  tryCatch({
    Sys.setlocale("LC_TIME","French_Belgium.1252")
    }, warning = function(w) {
      skip("OS can't test different locale test")
    })

  date <- "July, 15 2016 19:17:14"

  # our function catches for any locale issues
  expect_true(!is.na(mdy_hms(date)))



})

test_that("password obscure", {

  # what is the config
  if (file.exists("rdhs.json")) {
  config <- read_rdhs_config_file("rdhs.json")

  # the message should have * in
  mes <- paste0(rep("*", nchar(config$password)), collapse="")
  expect_message(print_rdhs_config(config), regexp = mes, fixed = TRUE)
  } else {
  skip("No authentication available for password test")
}
})

test_that("update_rdhs_config", {

  # what is the config
  testthat::skip_on_cran()
  skip_if_no_auth()
  if (file.exists("rdhs.json")) {
    config <- read_rdhs_config_file("rdhs.json")
    d <- dhs_data_updates()

    update_rdhs_config(cache_path = "demo")
    expect_true(dir.exists("demo"))
    unlink("demo", force = TRUE)

    expect_error(update_rdhs_config(config_path = "twaddle"))

    # set it back to previous
    class(config) <- NULL
    config$data_frame <- config$data_frame_nice
    config$data_frame_nice <- NULL
    config <- write_rdhs_config_file(config, config$config_path)

  } else {
    skip("No authentication available for update_rdhs_config test")
  }
})

test_that("model datasets correct", {

  md <- model_datasets
  expect_true(
    grepl("br",
          md$FileName[which(md$FileType == "Births Recode")[1]],
          ignore.case = TRUE)
    )

})

test_that("model datasets onAttach", {

  testthat::skip_on_cran()
  skip_if_no_auth()
  if(!exists("model_datasets")) {
    skip("No model datasets found")
  }

  md <- model_datasets

  ## remove the dataset so that it is pseudo us not having rdhs loaded
  rm(model_datasets, envir = parent.env(environment()))
  ar <- rdhs::get_datasets("MWAR7ASV.ZIP")
  expect_true(is.list(ar))

  assign("model_datasets",md,parent.env(environment()))

  })


test_that("convert labelled df to chars", {

  # correct df
  df1 <- data.frame(
    area = haven::labelled(c(1L, 2L, 3L), c("reg 1"=1,"reg 2"=2,"reg 3"=3)),
    climate = haven::labelled(c(0L, 1L, 1L), c("cold"=0,"hot"=1))
  )

  df_char <- delabel_df(df = df1)
  expect_identical(df_char$area, c("reg 1", "reg 2", "reg 3"))

  # failing df
  df_fail <- data.frame(
    area = c("reg 1","reg 2","reg 3"),
    climate = haven::labelled(c(0L, 1L, 1L), c("cold"=0,"hot"=1))
  )
  expect_error(df_fail <- delabel_df(df = df_fail), regexp = "labelled")

})


