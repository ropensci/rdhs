## TESTING LOGIC:
# test most the examples in the endpoints but combine the larger results into fewer
# test the specific nuances with passing the format and client caches

context("API endpoints")

test_that("format catches and al_results tests", {
  skip_if_no_auth()
  testthat::skip_on_cran()

  # Create new directory
  td <- file.path(tempdir(), as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if (file.exists("credentials")) {
    cli <- rdhs::client_dhs(
      api_key = "ICLSPH-527168", credentials = "credentials", root = td
    )
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168", root = td)
  }

  dat <- dhs_data_updates(
    client = cli, lastUpdate = "20150901", all_results = FALSE
  )
  dat2 <- dhs_data_updates(
    client = cli, lastUpdate = "20150901", all_results = FALSE
  )
  expect_identical(dat, dat2)

  # tets for non "json" specification
  dat <- dhs_datasets(f = "xml", all_results = FALSE)

  # tests for rubbish arguments
  expect_error(dhs_data(
    indicatorIds = "ML_FEVT_C_AMasfafasfL",
    surveyYearStart = 202231231306,
    breakdown = "subParTyping"
  ))
  dat <- dhs_datasets(
    surveyIds = "laksjdoash,dasjd", f = "xml", all_results = FALSE
  )

  # test for misstyped args
  expect_error(dhs_countries(countryIds = "SE"))

  # test for all_results  and smaller than 5000 page
  dat <- dhs_indicators()

  # test for all_results  and larger than 5000 page
  datasets <- dhs_datasets()

  # test for data.table extension
  Sys.setenv("rdhs_DATA_TABLE" = TRUE)
  library(data.table)
  dat <- dhs_countries(
    countryIds = "TZ", surveyYearStart = "2010", all_results = FALSE
  )
  expect_true(inherits(dat, "data.table"))

  Sys.setenv("rdhs_DATA_TABLE" = FALSE)
})

test_that("geojson works", {

# one that has to paginate
  d <- dhs_data(countryIds = "SN",
                surveyYearStart = 2014,
                breakdown = "subnational",
                returnGeometry = TRUE,
                f = "geojson")

  expect_true(inherits(d, "list"))
  expect_equal(names(d), c("crs", "type", "features"))
  expect_true(d$features %>% length > 100)

  # one that doesnt
  d <- dhs_data(countryIds = "SN",
                indicatorIds = "FE_FRTR_W_A15",
                surveyYearStart = 2014,
                breakdown = "subnational",
                returnGeometry = TRUE,
                f = "geojson")

  expect_true(inherits(d, "list"))
  expect_equal(names(d), c("crs", "type", "features"))
  expect_true(d$features %>% length < 100)


})


test_that("dhs_countries works", {
  testthat::skip_on_cran()

  dat <- dhs_countries(
    countryIds = "SN", surveyYearStart = "2010", all_results = FALSE
  )
  expect_identical(dat$UNAIDS_CountryCode[1], "SEN")
  dat <- dhs_countries(
    countryIds = c("EG", "SN"),
    surveyYearStart = "1991", surveyYearEnd = "2006",
    surveyType = "DHS", surveyCharacteristicIds = "32"
  )
  expect_identical(dat$ISO3_CountryCode[1:2], c("EGY", "SEN"))
  dat <- dhs_countries(tagIds = "1", all_results = FALSE)
  expect_true(any(dat$SubregionName %in% "South Asia"))
})

test_that("dhs_data works", {
  testthat::skip_on_cran()

  dat <- dhs_data(
    countryIds = "EG", indicatorIds = "FE_FRTR_W_TFR",
    selectSurveys = "latest", all_results = FALSE
  )
  expect_true(is.numeric(dat$DataId[1]))
  dat <- dhs_data(surveyIds = "SN2010DHS", all_results = FALSE)
  expect_true(any(dat$DataId > 30000))
  dat <- dhs_data(
    selectSurveys = "byIndicator", indicatorIds = "FE_CEBA_W_CH0",
    surveyCharacteristicIds = "32", all_results = FALSE
  )
  expect_true(is.numeric(dat$DataId[1]))
  dat <- dhs_data(surveyYear = "2010", surveyType = "DHS", all_results = FALSE)
  expect_true(is.numeric(dat$DataId[1]))
  dat <- dhs_data(surveyCharacteristicIds = "32", all_results = FALSE)
  expect_true(is.numeric(dat$DataId[1]))
  dat <- dhs_data(
    breakdown = "subnational", countryIds = "AZ",
    all_results = FALSE
  )
  expect_true(any(dat$CharacteristicLabel %in% "Baku"))
})

test_that("dhs_dataUpdates works", {
  testthat::skip_on_cran()

  dat <- dhs_data_updates(lastUpdate = "20150901", all_results = FALSE)
  expect_true(any(dat$SurveyId %in% "TL2016DHS"))
  dat <- dhs_data_updates(f = "html", all_results = FALSE)
  expect_true(class(dat) == "response")
})

test_that("dhs_datasets works", {
  testthat::skip_on_cran()

  dat <- dhs_datasets(
    countryIds = "EG", selectSurveys = "latest",
    surveyYearStart = 2000, surveyYearEnd = 2016,
    surveyType = "DHS", all_results = FALSE
  )
  expect_true(any(dat$FileName %in% "EGGE42FL.zip"))
  dat <- dhs_datasets(fileType = "KR", all_results = FALSE)
  expect_true(any(dat$FileType %in% "Children's Recode"))
})

test_that("dhs_indicators works", {
  testthat::skip_on_cran()

  dat <- dhs_indicators(
    countryIds = "EG", all_results = FALSE,
    indicatorIds = "FE_FRTR_W_TFR"
  )
  expect_identical(dat$ShortName[1], "TFR 15-49")
  dat <- dhs_indicators(
    surveyIds = "SN2010DHS", surveyYearStart = "2006", all_results = FALSE,
    surveyYearEnd = "2015"
  )
  expect_true(any(dat$MeasurementType %in% "Rate"))
  dat <- dhs_indicators(
    surveyType = "DHS", surveyCharacteristicIds = "32",
    tagIds = "1", all_results = FALSE
  )
  expect_true(any(dat$DenominatorWeightedId %in% "FP_CUSM_W_NUM"))
})

test_that("dhs_info works", {
  testthat::skip_on_cran()

  dat <- dhs_info(infoType = "version", all_results = FALSE)
  expect_identical(dat$InfoType, "Version")
  dat <- dhs_info(infoType = "citation", all_results = FALSE)
  expect_identical(dat$InfoType, "Citation")
})

test_that("dhs_publications works", {
  testthat::skip_on_cran()

  dat <- dhs_publications(
    countryIds = "EG", all_results = FALSE,
    selectSurveys = "latest"
  )
  expect_true(any(dat$SurveyYear %in% 2015))
  dat <- dhs_publications(
    surveyYearStart = "2006", surveyYearEnd = "2016",
    all_results = FALSE
  )
  expect_true(any(dat$PublicationSize %in% 926663))
  dat <- dhs_publications(
    surveyType = "DHS", surveyCharacteristicIds = "32",
    all_results = FALSE, tagIds = 1
  )
  expect_true(any(dat$PublicationTitle %in% "Final Report"))
})

test_that("dhs_survey_characteristics works", {
  testthat::skip_on_cran()

  dat <- dhs_survey_characteristics(
    countryIds = "EG",
    surveyYearStart = 2000, surveyYearEnd = 2016,
    surveyType = "DHS", all_results = FALSE
  )
  alc <- which(dat$SurveyCharacteristicID == 16)
  expect_equal(dat$SurveyCharacteristicID[alc], 16)
  expect_equal(dat$SurveyCharacteristicName[alc], "Abortion")
  dat <- dhs_survey_characteristics(surveyYearStart = "1991", surveyType = "DHS")
  expect_true(any(dat$SurveyCharacteristicName %in% "Abortion"))
})

test_that("dhs_surveys works", {
  testthat::skip_on_cran()

  dat <- dhs_surveys(
    countryIds = "EG",
    surveyYearStart = 2000, surveyYearEnd = 2016,
    surveyType = "DHS", all_results = FALSE
  )
  expect_true(any(dat$NumberofHouseholds %in% 16957))
  dat <- dhs_surveys(surveyType = "DHS", all_results = FALSE)
  dat <- dhs_surveys(surveyStatus = "Ongoing", all_results = FALSE)
  expect_identical(dat$SurveyStatus[1], "Ongoing")
})

test_that("dhs_tags works", {
  testthat::skip_on_cran()

  dat <- dhs_tags(indicatorIds = "FE_FRTR_W_TFR", all_results = FALSE)
  expect_equal(dim(dat)[2], 4)
  dat <- dhs_tags(countryIds = "SN", all_results = FALSE)
  expect_true(any(dat$TagName %in% "DHS Mobile"))
})

test_that("dhs_uiUpdates works", {
  testthat::skip_on_cran()

  dat <- dhs_uiUpdates(lastUpdate = "20150901", all_results = FALSE)
  expect_true(any(dat$Interface %in% "Surveys"))
})
