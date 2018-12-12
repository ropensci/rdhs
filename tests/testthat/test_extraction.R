context("Extraction")

test_that("query codes having downloaded surveys", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  cli <- new_rand_client()

  # create availbale surveys
  survs <- cli$available_datasets()

  # check rds only for one survey
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62DT.ZIP", download_option = "r"
  )
  downloads <- cli$get_datasets(
    dataset_filenames = "AOBR62SV.ZIP", download_option = "r",
    all_lower = FALSE, reformat = TRUE
  )

  ## QUESTIONS AND VARIABLE TESTS

  # create questions
  quest <- cli$survey_questions(
    dataset_filenames = "AOBR62DT.ZIP",
    search_terms = c("fever", "malaria", "test")
  )

  # check the regeex option
  quest <- cli$survey_questions(
    dataset_filenames = "AOBR62DT.ZIP", regex = c("fever|test")
  )

  # cehck for message on both
  quest <- cli$survey_questions(
    dataset_filenames = "AOBR62DT.ZIP",
    regex = c("fever|test"),
    search_terms = c("fever|test")
  )

  # check the essetial temrs option
  quest <- cli$survey_questions(
    dataset_filenames = "AOBR62DT.ZIP",
    search_terms = c("fever|test"),
    essential_terms = "malaria"
  )


  # check the same with an uppercase survey, one variable that is na essential
  quest <- cli$survey_variables(
    dataset_filenames = "AOBR62SV.ZIP", variables = "hml32",
    essential_variables = c("hml35", "h32n"),
    reformat = TRUE
  )

  # check variable
  quest <- cli$survey_questions(
    dataset_filenames = "AOBR62DT.ZIP",
    search_terms = c("fever|test")
  )


  ####

  # extract the data
  extract <- cli$extract(quest, add_geo = T)

  # extract the qeustions
  extract_neat <- rdhs:::extract_codes_to_descriptions(extract, quest)

  ## and repeat for sruveys that have no geo

  # check rds only for one survey
  downloads <- cli$get_datasets(
    dataset_filenames = "ZWHR31SV.ZIP",
    download_option = "r"
  )
  r <- readRDS(downloads$ZWHR31SV)
  r1 <- data_and_labels(r)
  r2 <- data_and_labels(downloads$ZWHR31SV)
  expect_identical(r1, r2)
  expect_error(data_and_labels("twaddle"))

  # create questions for a regex and non
  quest <- cli$survey_questions(
    dataset_filenames = "ZWHR31SV.ZIP",
    regex = c("Has refrigerator", "fever")
  )
  quest <- cli$survey_questions(
    dataset_filenames = "ZWHR31SV.ZIP",
    search_terms = c("Has refrigerator")
  )

  # extract the data
  extract <- cli$extract(quest, add_geo = T)
  expect_identical(extract$ZWHR31SV$LATNUM[1], NA)

  # and repreat for reformatted ones
  quest <- cli$survey_questions(
    dataset_filenames = "AOBR62DT.ZIP",
    search_terms = "malaria", reformat = TRUE
  )
  extract <- cli$extract(quest, add_geo = T)
  unlink(cli$get_root())
})

test_that("surveyId in extract", {
  skip_if_no_auth()
  testthat::skip_on_cran()
  cli <- new_rand_client()

  dat <- cli$survey_variables(
    dataset_filenames = "ZWHR31SV.ZIP",
    variables = "hv024"
  )
  expect_identical(names(dat)[5], "survey_id")

  extract <- cli$extract(dat)
  unlink(cli$get_root())
})



test_that("rbind_labelled", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  # create auth through whichever route is valid for the environment
  cli <- new_rand_client()

  # get some datasets
  d <- cli$get_datasets(c("AOBR62FL.ZIP", "BJBR41FL.ZIP"))

  quest <- cli$survey_variables(c("AOBR62FL.ZIP", "BJBR41FL.ZIP"),
    variables = c("v024", "v130")
  )

  extract <- cli$extract(quest, add_geo = TRUE)

  expect_warning(rbind_labelled(extract))

  dat <- rbind_labelled(extract, labels = list(
    "v024" = "concatenate",
    "v130" = "concatenate"
  ))

  dat <- rbind_labelled(extract,
    labels = list("v024" = "concatenate"),
    warn = FALSE
  )

  # now do it for just one variable
  quest <- cli$survey_variables(c("AOBR62FL.ZIP", "BJBR41FL.ZIP"),
    variables = c("v024")
  )
  extract <- cli$extract(quest, add_geo = FALSE)
  dat <- rbind_labelled(extract, labels = list("v024" = "concatenate"))
  expect_warning(rbind_labelled(extract))


  # now let's force it to look out for factors and reformats
  quest <- cli$survey_variables(c("AOBR62FL.ZIP", "BJBR41FL.ZIP"),
    variables = c("v024", "v130"), reformat = TRUE
  )
  extract <- cli$extract(quest, add_geo = FALSE)
  dat <- rbind_labelled(extract, labels = list("v024" = "concatenate"))

  # now for factors
  extract$AOBR62FL$v024 <- as.factor(extract$AOBR62FL$v024)
  extract$BJBR41FL$v024 <- as.factor(extract$BJBR41FL$v024)
  dat <- rbind_labelled(extract, labels = list("v024" = "concatenate"))

  expect_equal(
    as.numeric(sort(table(dat$v024))),
    as.numeric(sort(c(
      table(extract$AOBR62FL$v024),
      table(extract$BJBR41FL$v024)
    )))
  )
  unlink(cli$get_root())
})

test_that("as_factor.labelled", {

  df1 <- data.frame(
    area = haven::labelled(c(1L, 2L, 3L), c("reg 1"=1,"reg 2"=2,"reg 3"=3)),
    climate = haven::labelled(c(0L, 1L, 1L), c("cold"=0,"hot"=1))
  )

  # manually change it to the old style
  class(df1$area) <- "labelled"
  class(df1$climate) <- "labelled"

  # rdhs as_factor.labelled check
  expect_true(class(as_factor.labelled(df1$area)) == "factor")

})

test_that("add_geo issue for Kenya", {
  skip_if_no_auth()
  testthat::skip_on_cran()
  cli <- new_rand_client()

  dat <- cli$survey_variables(
    dataset_filenames = "KEBR71FL.ZIP",
    variables = "v024"
  )

  extract <- cli$extract(dat, add_geo = TRUE)
  expect_true(length(unique(extract$KEBR71FL$DHSREGNA))>1)
  unlink(cli$get_root())
})
