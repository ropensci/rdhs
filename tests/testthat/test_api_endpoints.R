## TESTING LOGIC:
# test most the examples in the endpoints but combine the larger results to few
# test the specific nuances with passing the format and client caches

context("API endpoints")

test_that("format catches and all_results tests", {
  testthat::skip_on_cran()

  # Create new directory
  old_config <- if(file.exists("rdhs.json")) "rdhs.json" else NULL
  cli <- new_rand_client()

  dat <- api_timeout_safe_test(
    dhs_data_updates(
      client = cli, lastUpdate = "20150901", all_results = FALSE
    ), cli
  )
  dat2 <- api_timeout_safe_test(
    dhs_data_updates(
      client = cli, lastUpdate = "20150901", all_results = FALSE
    ), cli
  )
  expect_identical(dat, dat2)

  # tets for non "json" specification
  dat <- api_timeout_safe_test(
    dhs_datasets(f = "xml", all_results = FALSE), cli
  )

  # tests for rubbish arguments
  expect_error(api_timeout_safe_test(
    dhs_data(
      indicatorIds = "ML_FEVT_C_AMasfafasfL",
      surveyYearStart = 202231231306,
      breakdown = "subParTyping"
    ), cli
  )
  )
  dat <- api_timeout_safe_test(
    dhs_datasets(
      surveyIds = "laksjdoash,dasjd", f = "xml", all_results = FALSE
    ), cli
  )

  # test for misstyped args
  expect_error(api_timeout_safe_test(dhs_countries(countryIds = "SE"), cli))

  # test for all_results  and smaller than 5000 page
  dat <- api_timeout_safe_test(
    dhs_indicators(), cli
  )

  # test for all_results  and larger than 5000 page
  datasets <- api_timeout_safe_test(
    dhs_datasets(), cli
  )

  # test for data.table extension
  set_rdhs_config(data_frame = "data.table::as.data.table", prompt = FALSE)
  library(data.table)
  dat <- api_timeout_safe_test(
    dhs_countries(
      countryIds = "TZ", surveyYearStart = "2010", all_results = FALSE
    ), cli
  )
  expect_true(inherits(dat, "data.table"))
  Sys.setenv("rdhs_DATA_TABLE" = FALSE)

  # if we created a config for these tests then remove it
  if(is.null(old_config)){
    file.remove("rdhs.json")
  }

})

test_that("geojson works", {

  testthat::skip("geojson test too heavy an API test")
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  skip_if_slow_API()

  # one that has to paginate
  d <- api_timeout_safe_test(
    dhs_data(countryIds = "SN",
             surveyYearStart = 2014,
             breakdown = "subnational",
             returnGeometry = TRUE,
             f = "geojson"), cli
  )

  expect_true(inherits(d, "list"))
  expect_equal(names(d), c("crs", "type", "features"))
  expect_true(d$features %>% length > 100)

  # one that doesnt
  d <- api_timeout_safe_test(
    dhs_data(countryIds = "SN",
             indicatorIds = "FE_FRTR_W_A15",
             surveyYearStart = 2014,
             breakdown = "subnational",
             returnGeometry = TRUE,
             f = "geojson"), cli
  )

  expect_true(inherits(d, "list"))
  expect_equal(names(d), c("crs", "type", "features"))
  expect_true(d$features %>% length < 100)


})


test_that("dhs_countries works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_countries(
      countryIds = "SN", surveyYearStart = "2010", all_results = FALSE
    ), cli
  )
  expect_identical(dat$UNAIDS_CountryCode[1], "SEN")
  dat <- api_timeout_safe_test(
    dhs_countries(
      countryIds = c("EG", "SN"),
      surveyYearStart = "1991", surveyYearEnd = "2006",
      surveyType = "DHS", surveyCharacteristicIds = "32"
    ), cli
  )
  expect_identical(dat$ISO3_CountryCode[1:2], c("EGY", "SEN"))
  dat <- api_timeout_safe_test(
    dhs_countries(tagIds = "1", all_results = FALSE), cli
  )
  expect_true(any(dat$SubregionName %in% "South Asia"))
})

test_that("dhs_data works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_data(
      countryIds = "EG", indicatorIds = "FE_FRTR_W_TFR",
      selectSurveys = "latest", all_results = FALSE
    ), cli
  )
  expect_true(is.numeric(dat$DataId[1]))
  dat <- api_timeout_safe_test(
    dhs_data(surveyIds = "SN2010DHS", all_results = FALSE), cli
  )
  expect_true(any(dat$DataId > 30000))
  dat <- api_timeout_safe_test(
    dhs_data(
      selectSurveys = "byIndicator", indicatorIds = "FE_CEBA_W_CH0",
      surveyCharacteristicIds = "32", all_results = FALSE
    ), cli
  )
  expect_true(is.numeric(dat$DataId[1]))
  dat <- api_timeout_safe_test(
    dhs_data(surveyYear = "2010", surveyType = "DHS", all_results = FALSE), cli
  )
  expect_true(is.numeric(dat$DataId[1]))
  dat <- api_timeout_safe_test(
    dhs_data(surveyCharacteristicIds = "32", all_results = FALSE), cli
  )
  expect_true(is.numeric(dat$DataId[1]))
  dat <- api_timeout_safe_test(
    dhs_data(
      breakdown = "subnational", countryIds = "AZ",
      all_results = FALSE
    ), cli
  )
  expect_true(any(dat$CharacteristicLabel %in% "Baku"))
})

test_that("dhs_dataUpdates works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_data_updates(lastUpdate = "20150901", all_results = FALSE), cli
  )
  expect_true(any(dat$SurveyId %in% "TL2016DHS"))
  dat <- api_timeout_safe_test(
    dhs_data_updates(f = "html", all_results = FALSE), cli
  )
  expect_true(class(dat) == "response")
})

test_that("dhs_datasets works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_datasets(
      countryIds = "EG", selectSurveys = "latest",
      surveyYearStart = 2000, surveyYearEnd = 2016,
      surveyType = "DHS", all_results = FALSE
    ), cli
  )
  expect_true(any(dat$FileName %in% "EGGE42FL.zip"))
  dat <- api_timeout_safe_test(
    dhs_datasets(fileType = "KR", all_results = FALSE), cli
  )
  expect_true(any(dat$FileType %in% "Children's Recode"))
})

test_that("dhs_indicators works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_indicators(
      countryIds = "EG", all_results = FALSE,
      indicatorIds = "FE_FRTR_W_TFR"
    ), cli
  )
  expect_identical(dat$ShortName[1], "TFR 15-49")
  dat <- api_timeout_safe_test(
    dhs_indicators(
      surveyIds = "SN2010DHS", surveyYearStart = "2006", all_results = FALSE,
      surveyYearEnd = "2015"
    ), cli
  )
  expect_true(any(dat$MeasurementType %in% "Rate"))
  dat <- api_timeout_safe_test(
    dhs_indicators(
      surveyType = "DHS", surveyCharacteristicIds = "32",
      tagIds = "1", all_results = FALSE
    ), cli
  )
  expect_true(any(dat$DenominatorWeightedId %in% "FP_CUSM_W_NUM"))
})

test_that("dhs_info works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_info(infoType = "version", all_results = FALSE), cli
  )
  expect_identical(dat$InfoType, "Version")
  dat <- api_timeout_safe_test(
    dhs_info(infoType = "citation", all_results = FALSE), cli
  )
  expect_identical(dat$InfoType, "Citation")
})

test_that("dhs_publications works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_publications(
      countryIds = "EG", all_results = FALSE,
      selectSurveys = "latest"
    ), cli
  )
  expect_true(any(dat$SurveyYear %in% 2015))
  dat <- api_timeout_safe_test(
    dhs_publications(
      surveyYearStart = "2006", surveyYearEnd = "2016",
      all_results = FALSE
    ), cli
  )
  expect_true(any(dat$PublicationSize %in% 926663))
  dat <- api_timeout_safe_test(
    dhs_publications(
      surveyType = "DHS", surveyCharacteristicIds = "32",
      all_results = FALSE, tagIds = 1
    ), cli
  )
  expect_true(any(dat$PublicationTitle %in% "Final Report"))
})

test_that("dhs_survey_characteristics works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_survey_characteristics(
      countryIds = "EG",
      surveyYearStart = 2000, surveyYearEnd = 2016,
      surveyType = "DHS", all_results = FALSE
    ), cli
  )
  alc <- which(dat$SurveyCharacteristicID == 16)
  expect_equal(dat$SurveyCharacteristicID[alc], 16)
  expect_equal(dat$SurveyCharacteristicName[alc], "Abortion")
  dat <- api_timeout_safe_test(
    dhs_survey_characteristics(surveyYearStart = "1991",
                               surveyType = "DHS")
  )

expect_true(any(dat$SurveyCharacteristicName %in% "Abortion"))
})

test_that("dhs_surveys works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_surveys(
      countryIds = "EG",
      surveyYearStart = 2000, surveyYearEnd = 2016,
      surveyType = "DHS", all_results = FALSE
    ), cli
  )
  expect_true(any(dat$NumberofHouseholds %in% 16957))
  dat <- api_timeout_safe_test(
    dhs_surveys(surveyType = "DHS", all_results = FALSE), cli
  )
  dat <- api_timeout_safe_test(
    dhs_surveys(surveyStatus = "Ongoing", all_results = FALSE), cli
  )
  expect_identical(dat$SurveyStatus[1], "Ongoing")
})

test_that("dhs_tags works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_tags(indicatorIds = "FE_FRTR_W_TFR", all_results = FALSE), cli
  )
  expect_equal(dim(dat)[2], 4)
  dat <- api_timeout_safe_test(
    dhs_tags(countryIds = "SN", all_results = FALSE), cli
  )
  expect_true(any(dat$TagName %in% "DHS Mobile"))
})

test_that("dhs_uiUpdates works", {
  testthat::skip_on_cran()
  skip_if_slow_API()

  dat <- api_timeout_safe_test(
    dhs_ui_updates(lastUpdate = "20150901", all_results = FALSE), cli
  )
  expect_true(any(dat$Interface %in% "Surveys"))

})

test_that("post api tidy", {

  if (file.exists("rdhs.json")) {
    conf <- read_rdhs_config_file("rdhs.json")
    if (is.null(conf$email)){
      expect_true(file.remove("rdhs.json"))
    }

  } else {
    expect_true(TRUE)
  }

})
