## TESTING LOGIC:
# test most the examples in the endpoints but combine the larger results into fewer
# test the specific nuances with passing the format and client caches

context("API endpoints")

test_that("format catches and al_results tests", {


  skip_if_no_auth()
  testthat::skip_on_cran()

  # Create new directory
  td <- file.path(tempdir(),as.integer(Sys.time()))

  # create auth through whichever route is valid for the environment
  if(file.exists("credentials")){
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",credentials = "credentials",root = td)
  } else {
    cli <- rdhs::client_dhs(api_key = "ICLSPH-527168",root = td)
  }

  dat <- dhs_dataUpdates(client = cli, lastUpdate = "20150901", allResults = FALSE)
  dat2 <- dhs_dataUpdates(client = cli, lastUpdate = "20150901", allResults = FALSE)
  expect_identical(dat,dat2)

  # tets for non "json" specification
  dat <- dhs_datasets(f = "xml", allResults = FALSE)

  # tests for rubbish arguments
  dat <- dhs_datasets(surveyIds = "laksjdoash,dasjd",allResults = FALSE)
  dat <- dhs_datasets(surveyIds = "laksjdoash,dasjd",f="xml",allResults = FALSE)

  # test for misstyped args
  expect_message(dhs_countries(countryIds = "SE"))

  # test for allResults  and smaller than 5000 page
  dat <- dhs_indicators()

  # test for allResults  and larger than 5000 page
  datasets <- dhs_datasets()

})


test_that("dhs_countries works", {

  testthat::skip_on_cran()

  dat <- dhs_countries(countryIds = "EG", indicatorIds = "FE_FRTR_W_TFR",
                       surveyIds = "SN2010DHS", surveyYear = "2010", allResults = FALSE)
  dat <- dhs_countries(surveyYearStart = "1991", surveyYearEnd = "2006",
                       surveyType = "DHS", surveyCharacteristicIds = "32")
  dat <- dhs_countries(tagIds = "1", allResults = FALSE)
})

  test_that("dhs_data works", {

    testthat::skip_on_cran()

    dat <- dhs_data(countryIds = "EG", indicatorIds = "FE_FRTR_W_TFR", allResults = FALSE)
    dat <- dhs_data(surveyIds = "SN2010DHS", allResults = FALSE)
    dat <- dhs_data(selectSurveys = "latest", allResults = FALSE)
    dat <- dhs_data(selectSurveys = "byIndicator", indicatorIds = "FE_CEBA_W_CH0",
                    allResults = FALSE)
    dat <- dhs_data(surveyYear = "2010", allResults = FALSE)
    dat <- dhs_data(surveyYearStart = "2006", allResults = FALSE)
    dat <- dhs_data(surveyType = "DHS", allResults = FALSE)
    dat <- dhs_data(surveyCharacteristicIds = "32", allResults = FALSE)
    dat <- dhs_data(breakdown = "subnational", countryIds = "AZ",
                    characteristicLabel = "6+", allResults = FALSE)
  })

  test_that("dhs_dataUpdates works", {

    testthat::skip_on_cran()

    dat <- dhs_dataUpdates(lastUpdate = "20150901", allResults = FALSE)
    dat <- dhs_dataUpdates(f = "html", allResults = FALSE)
  })

  test_that("dhs_datasets works", {

    testthat::skip_on_cran()

    dat <- dhs_datasets(countryIds = "EG", allResults = FALSE)
    dat <- dhs_datasets(selectSurveys = "latest", allResults = FALSE)
    dat <- dhs_datasets(surveyIds = "SN2010DHS", allResults = FALSE)
    dat <- dhs_datasets(surveyYear = "2010", allResults = FALSE)
    dat <- dhs_datasets(surveyYearStart = "2006", allResults = FALSE)
    dat <- dhs_datasets(surveyYearStart = "1991", surveyYearEnd = "2006",
                        allResults = FALSE)
    dat <- dhs_datasets(surveyType = "DHS", allResults = FALSE)
    dat <- dhs_datasets(fileFormat = "stata", allResults = FALSE)
    dat <- dhs_datasets(fileFormat = "DT", allResults = FALSE)
    dat <- dhs_datasets(fileType = "KR", allResults = FALSE)
  })

  test_that("dhs_indicators works", {

    testthat::skip_on_cran()

    dat <- dhs_indicators(countryIds = "EG", allResults = FALSE)
    dat <- dhs_indicators(indicatorIds = "FE_FRTR_W_TFR",
                          allResults = FALSE)
    dat <- dhs_indicators(surveyIds = "SN2010DHS", allResults = FALSE)
    dat <- dhs_indicators(surveyYear = "2010", allResults = FALSE)
    dat <- dhs_indicators(surveyYearStart = "2006", allResults = FALSE)
    dat <- dhs_indicators(surveyYearStart = "1991", surveyYearEnd = "2006",
                          allResults = FALSE)
    dat <- dhs_indicators(surveyType = "DHS", allResults = FALSE)
    dat <- dhs_indicators(surveyCharacteristicIds = "32",
                          allResults = FALSE)
    dat <- dhs_indicators(tagIds = "1", allResults = FALSE)
    dat <- dhs_indicators(f = "html", allResults = FALSE)
  })

  test_that("dhs_info works", {

    testthat::skip_on_cran()

    dat <- dhs_info(infoType = "version", allResults = FALSE)
    dat <- dhs_info(infoType = "citation", allResults = FALSE)
    dat <- dhs_info(f = "html", allResults = FALSE)
  })

  test_that("dhs_publications works", {

    testthat::skip_on_cran()

    dat <- dhs_publications(countryIds = "EG", allResults = FALSE)
    dat <- dhs_publications(selectSurveys = "latest", allResults = FALSE)
    dat <- dhs_publications(indicatorIds = "FE_FRTR_W_TFR",
                            allResults = FALSE)
    dat <- dhs_publications(surveyIds = "SN2010DHS", allResults = FALSE)
    dat <- dhs_publications(surveyYear = "2010", allResults = FALSE)
    dat <- dhs_publications(surveyYearStart = "2006", allResults = FALSE)
    dat <- dhs_publications(surveyYearStart = "1991", surveyYearEnd = "2006",
                            allResults = FALSE)
    dat <- dhs_publications(surveyType = "DHS", allResults = FALSE)
    dat <- dhs_publications(surveyCharacteristicIds = "32",
                            allResults = FALSE)
    dat <- dhs_publications(tagIds = 1, allResults = FALSE)
    dat <- dhs_publications(f = "html", allResults = FALSE)
  })

  test_that("dhs_surveyCharacteristics works", {

    testthat::skip_on_cran()

    dat <- dhs_surveyCharacteristics(countryIds = "EG",
                                     allResults = FALSE)
    dat <- dhs_surveyCharacteristics(indicatorIds = "FE_FRTR_W_TFR",
                                     allResults = FALSE)
    dat <- dhs_surveyCharacteristics(surveyIds = "SN2010DHS,allResults=FALSE")
    dat <- dhs_surveyCharacteristics(surveyYear = "2010,allResults=FALSE")
    dat <- dhs_surveyCharacteristics(surveyYearStart = "2006",
                                     allResults = FALSE)
    dat <- dhs_surveyCharacteristics(surveyYearStart = "1991",
                                     surveyYearEnd = "2006", allResults = FALSE)
    dat <- dhs_surveyCharacteristics(surveyType = "DHS",
                                     allResults = FALSE)
  })

  test_that("dhs_surveys works", {

    testthat::skip_on_cran()

    dat <- dhs_surveys(countryIds = "EG", allResults = FALSE)
    dat <- dhs_surveys(indicatorIds = "FE_FRTR_W_TFR",
                       allResults = FALSE)
    dat <- dhs_surveys(selectSurveys = "latest", allResults = FALSE)
    dat <- dhs_surveys(surveyIds = "SN2010DHS", allResults = FALSE)
    dat <- dhs_surveys(surveyYear = "2010", allResults = FALSE)
    dat <- dhs_surveys(surveyYearStart = "2006", allResults = FALSE)
    dat <- dhs_surveys(surveyYearStart = "1991", surveyYearEnd = "2006",
                       allResults = FALSE)
    dat <- dhs_surveys(surveyType = "DHS", allResults = FALSE)
    dat <- dhs_surveys(surveyStatus = "surveys", allResults = FALSE)
    dat <- dhs_surveys(surveyStatus = "completed", allResults = FALSE)
    dat <- dhs_surveys(surveyStatus = "ongoing", allResults = FALSE)
    dat <- dhs_surveys(surveyStatus = "all", allResults = FALSE)
    dat <- dhs_surveys(surveyCharacteristicIds = "32",
                       allResults = FALSE)
    dat <- dhs_surveys(tagIds = "1", allResults = FALSE)
  })

  test_that("dhs_tags works", {

    testthat::skip_on_cran()

    dat <- dhs_tags(countryIds = "EG", allResults = FALSE)
    dat <- dhs_tags(indicatorIds = "FE_FRTR_W_TFR", allResults = FALSE)
    dat <- dhs_tags(surveyIds = "SN2010DHS", allResults = FALSE)
    dat <- dhs_tags(surveyYear = "2010", allResults = FALSE)
    dat <- dhs_tags(surveyYearStart = "2006", allResults = FALSE)
    dat <- dhs_tags(surveyYearStart = "1991", surveyYearEnd = "2006",
                    allResults = FALSE)
    dat <- dhs_tags(surveyType = "DHS", allResults = FALSE)
  })

  test_that("dhs_uiUpdates works", {

    testthat::skip_on_cran()

    dat <- dhs_uiUpdates(lastUpdate = "20150901", allResults = FALSE)
  })
