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
  expect_error(dhs_data(indicatorIds="ML_FEVT_C_AMasfafasfL",
                   surveyYearStart=202231231306,
                   breakdown="subParTyping"))
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

  dat <- dhs_countries(countryIds = "SN", surveyYearStart = "2010", allResults = FALSE)
  expect_identical(dat$UNAIDS_CountryCode[1] , "SEN")
  dat <- dhs_countries(countryIds = c("EG","SN"), surveyYearStart = "1991", surveyYearEnd = "2006",
                       surveyType = "DHS", surveyCharacteristicIds = "32")
  expect_identical(dat$ISO3_CountryCode[1:2],c("EGY","SEN"))
  dat <- dhs_countries(tagIds = "1", allResults = FALSE)
  expect_true(any(dat$SubregionName %in% "South Asia"))

})

  test_that("dhs_data works", {

    testthat::skip_on_cran()

    dat <- dhs_data(countryIds = "EG", indicatorIds = "FE_FRTR_W_TFR", selectSurveys = "latest",allResults = FALSE)
    expect_true(dat$DataId[1]==42365)
    dat <- dhs_data(surveyIds = "SN2010DHS", allResults = FALSE)
    expect_true(any(dat$DataId %in% 471035))
    dat <- dhs_data(selectSurveys = "byIndicator", indicatorIds = "FE_CEBA_W_CH0",surveyCharacteristicIds = "32", allResults = FALSE)
    expect_true(any(dat$DataId %in% 966))
    dat <- dhs_data(surveyYear = "2010", surveyType = "DHS", allResults = FALSE)
    expect_true(any(dat$DataId %in% 2086))
    dat <- dhs_data(surveyCharacteristicIds = "32", allResults = FALSE)
    expect_true(any(dat$DataId %in% 952))
    dat <- dhs_data(breakdown = "subnational", countryIds = "AZ",
                     allResults = FALSE)
    expect_true(any(dat$CharacteristicLabel %in% "Baku"))
    })

  test_that("dhs_dataUpdates works", {

    testthat::skip_on_cran()

    dat <- dhs_dataUpdates(lastUpdate = "20150901", allResults = FALSE)
    expect_true(any(dat$SurveyId %in% "TL2016DHS"))
    dat <- dhs_dataUpdates(f = "html", allResults = FALSE)
    expect_true(class(dat)=="response")
  })

  test_that("dhs_datasets works", {

    testthat::skip_on_cran()

    dat <- dhs_datasets(countryIds = "EG",selectSurveys = "latest",
                        surveyYearStart=2000, surveyYearEnd = 2016,
                        surveyType = "DHS", allResults = FALSE)
    expect_true(any(dat$FileName %in% "EGGE42FL.zip"))
    dat <- dhs_datasets(fileType = "KR", allResults = FALSE)
    expect_true(any(dat$FileType %in% "Children's Recode"))
  })

  test_that("dhs_indicators works", {

    testthat::skip_on_cran()

    dat <- dhs_indicators(countryIds = "EG", allResults = FALSE,indicatorIds = "FE_FRTR_W_TFR")
    expect_identical(dat$ShortName[1],"TFR 15-49")
    dat <- dhs_indicators(surveyIds = "SN2010DHS",surveyYearStart = "2006", allResults = FALSE,
                          surveyYearEnd = "2015")
    expect_true(any(dat$MeasurementType %in% "Rate"))
    dat <- dhs_indicators(surveyType = "DHS", surveyCharacteristicIds = "32",
                          tagIds = "1",allResults = FALSE)
    expect_true(any(dat$DenominatorWeightedId %in% "FP_CUSM_W_NUM"))

  })

  test_that("dhs_info works", {

    testthat::skip_on_cran()

    dat <- dhs_info(infoType = "version", allResults = FALSE)
    expect_identical(dat$InfoType,"Version")
    dat <- dhs_info(infoType = "citation", allResults = FALSE)
    expect_identical(dat$InfoType,"Citation")
  })

  test_that("dhs_publications works", {

    testthat::skip_on_cran()

    dat <- dhs_publications(countryIds = "EG", allResults = FALSE,selectSurveys = "latest")
    expect_true(any(dat$SurveyYear %in% 2015))
    dat <- dhs_publications(surveyYearStart = "2006", surveyYearEnd = "2016",
                            allResults = FALSE)
    expect_true(any(dat$PublicationSize %in% 926663))
    dat <- dhs_publications(surveyType = "DHS", surveyCharacteristicIds = "32",
                            allResults = FALSE,tagIds = 1)
    expect_true(any(dat$PublicationTitle %in% "Final Report"))

  })

  test_that("dhs_surveyCharacteristics works", {

    testthat::skip_on_cran()

    dat <- dhs_surveyCharacteristics(countryIds = "EG",
                                     surveyYearStart=2000, surveyYearEnd = 2016,
                                     surveyType = "DHS", allResults = FALSE)
    alc <- which(dat$SurveyCharacteristicID==16)
    expect_equal(dat$SurveyCharacteristicID[alc],16)
    expect_equal(dat$SurveyCharacteristicName[alc],"Abortion")
    dat <- dhs_surveyCharacteristics(surveyYearStart = "1991",surveyType = "DHS")
    expect_true(any(dat$SurveyCharacteristicName %in% "Abortion"))
    })

  test_that("dhs_surveys works", {

    testthat::skip_on_cran()

    dat <- dhs_surveys(countryIds = "EG",
                       surveyYearStart=2000, surveyYearEnd = 2016,
                       surveyType = "DHS", allResults = FALSE)
    expect_true(any(dat$NumberofHouseholds %in% 16957))
    dat <- dhs_surveys(surveyType = "DHS", allResults = FALSE)
    dat <- dhs_surveys(surveyStatus = "Ongoing", allResults = FALSE)
    expect_identical(dat$SurveyStatus[1],"Ongoing")

  })

  test_that("dhs_tags works", {

    testthat::skip_on_cran()

    dat <- dhs_tags(indicatorIds = "FE_FRTR_W_TFR", allResults = FALSE)
    expect_equal(dim(dat)[2],4)
    dat <- dhs_tags(countryIds = "SN", allResults = FALSE)
    expect_true(any(dat$TagName %in% "DHS Mobile"))
  })

  test_that("dhs_uiUpdates works", {

    testthat::skip_on_cran()

    dat <- dhs_uiUpdates(lastUpdate = "20150901", allResults = FALSE)
    expect_true(any(dat$Interface %in% "Surveys"))
  })
