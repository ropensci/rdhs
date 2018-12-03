context("Parse datasets")

test_that("datasets parse", {
  mrfl_zip <- tempfile()
  on.exit(unlink(mrfl_zip))
  download.file(paste0(
    "https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?",
    "Filename=ZZMR61FL.ZIP&Tp=1&Ctry_Code=zz&survey_id=0&doctype=dhs"
  ),
  destfile = mrfl_zip, mode = "wb"
  )

  mr_def <- read_dhs_flat(mrfl_zip)
  mr_dcf <- read_dhs_flat(mrfl_zip, meta_source = "dcf")
  mr_sps <- read_dhs_flat(mrfl_zip, meta_source = "sps")
  mr_do <- read_dhs_flat(mrfl_zip, meta_source = "do")

  mr_defF <- read_dhs_flat(mrfl_zip, all_lower = FALSE)
  mr_dcfF <- read_dhs_flat(mrfl_zip, all_lower = FALSE, meta_source = "dcf")
  mr_spsF <- read_dhs_flat(mrfl_zip, all_lower = FALSE, meta_source = "sps")
  mr_doF <- read_dhs_flat(mrfl_zip, all_lower = FALSE, meta_source = "do")

  expect_equal(attr(mr_def$mv025, "labels"), c("urban" = 1, "rural" = 2))
  expect_equal(attr(mr_dcf$mv025, "labels"), c("urban" = 1, "rural" = 2))
  expect_equal(attr(mr_sps$mv025, "labels"), c("urban" = 1, "rural" = 2))
  expect_equal(attr(mr_do$mv025, "labels"), c("urban" = 1, "rural" = 2))

  expect_equal(attr(mr_defF$mv025, "labels"), c("Urban" = 1, "Rural" = 2))
  expect_equal(attr(mr_dcfF$mv025, "labels"), c("Urban" = 1, "Rural" = 2))
  expect_equal(attr(mr_spsF$mv025, "labels"), c("Urban" = 1, "Rural" = 2))
  expect_equal(attr(mr_doF$mv025, "labels"), c("Urban" = 1, "Rural" = 2))

  expect_equal(attr(mr_def$mv012, "label"), "Current age")
  expect_equal(attr(mr_dcf$mv012, "label"), "Current age")
  expect_equal(attr(mr_sps$mv012, "label"), "Current age")
  expect_equal(attr(mr_do$mv012, "label"), "Current age")

  # check for misssin metadata by first extracting it removing all the meta data
  # and then zipping
  tf <- tempfile()
  suppressWarnings(unzip(mrfl_zip, exdir = tf))
  file.remove(list.files(tf, full.names = TRUE)[-grep(".DAT$", list.files(tf))])
  zip("dumyzip", files = list.files(tf, full.names = TRUE))
  expect_error(read_dhs_flat("dumyzip.zip"), "metadata file not found")

  unlink(c("tf", "tf2", "dumyzip.zip"))
})

test_that("data dictionaries FWF lengths match file width", {
  arfl_zip <- tempfile()
  on.exit(unlink(arfl_zip))
  download.file(paste0(
    "https://dhsprogram.com/customcf/legacy/data/sample_download_dataset.cfm?",
    "Filename=ZZAR61FL.ZIP&Tp=4&Ctry_Code=zz&survey_id=0&doctype=hiv"
  ), arfl_zip, mode = "wb")

  dcf <- rdhs::read_zipdata(arfl_zip, "\\.DCF", readLines)
  sps <- rdhs::read_zipdata(arfl_zip, "\\.SPS", readLines)
  do <- rdhs::read_zipdata(arfl_zip, "\\.DO", readLines)
  dct <- rdhs::read_zipdata(arfl_zip, "\\.DCT", readLines)

  dat <- rdhs::read_zipdata(arfl_zip, "\\.DAT$", iotools::input.file)

  expect_equal(sum(parse_dcf(dcf)$len), nchar(dat[1]))
  expect_equal(sum(parse_sps(sps)$len), nchar(dat[1]))
  expect_equal(sum(parse_do(do, dct)$len), nchar(dat[1]))

  # check for incorrect pattern
  expect_warning(rdhs::read_zipdata(arfl_zip, "\\.notachance", readLines))
})


test_that("lower case flat file check", {
  testthat::skip_on_cran()
  skip_if_no_auth()

  # Create new directory
  td <- file.path(tempdir(), as.integer(Sys.time()))

  # create
    cli <- rdhs::client_dhs(api_key = api_key_internal, root = td,
                            config = read_rdhs_config_file("rdhs.json"))

  dat <- cli$get_datasets("ngcr4afl.zip")
})
