context("Parse datasets")

test_that("datasets parse", {

  mrfl_zip <- tempfile()
  on.exit(unlink(mrfl_zip))
  download.file(paste0("https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?",
                       "Filename=ZZMR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs"), mrfl_zip, mode="wb")

  mr_def <- read_dhs_flat(mrfl_zip)
  mr_dcf <- read_dhs_flat(mrfl_zip, meta_source="dcf")
  mr_sps <- read_dhs_flat(mrfl_zip, meta_source="sps")
  mr_do <- read_dhs_flat(mrfl_zip, meta_source="do")

  mr_defF <- read_dhs_flat(mrfl_zip, all_lower=FALSE)
  mr_dcfF <- read_dhs_flat(mrfl_zip, all_lower=FALSE, meta_source="dcf")
  mr_spsF <- read_dhs_flat(mrfl_zip, all_lower=FALSE, meta_source="sps")
  mr_doF <- read_dhs_flat(mrfl_zip, all_lower=FALSE, meta_source="do")

  expect_equal(attr(mr_def$mv025, "labels"), c("urban"=1, "rural"=2))
  expect_equal(attr(mr_dcf$mv025, "labels"), c("urban"=1, "rural"=2))
  expect_equal(attr(mr_sps$mv025, "labels"), c("urban"=1, "rural"=2))
  expect_equal(attr(mr_do$mv025, "labels"), c("urban"=1, "rural"=2))

  expect_equal(attr(mr_defF$mv025, "labels"), c("Urban"=1, "Rural"=2))
  expect_equal(attr(mr_dcfF$mv025, "labels"), c("Urban"=1, "Rural"=2))
  expect_equal(attr(mr_spsF$mv025, "labels"), c("Urban"=1, "Rural"=2))
  expect_equal(attr(mr_doF$mv025, "labels"), c("Urban"=1, "Rural"=2))
  
  expect_equal(attr(mr_def$mv012, "label"), "Current age")
  expect_equal(attr(mr_dcf$mv012, "label"), "Current age")
  expect_equal(attr(mr_sps$mv012, "label"), "Current age")
  expect_equal(attr(mr_do$mv012, "label"), "Current age")

})
          
