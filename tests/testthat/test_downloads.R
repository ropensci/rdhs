context("Downloads")

test_that("download", {


   dhs_surveys <- rdhs:::available_surveys(your_email="rdhs.tester@gmail.com",
   your_password="rdhstesting",
   your_project="Testing Malaria Investigations",
   output_dir=tempdir(),
   max_urls=2)

   testthat::expect_message(download_datasets(your_email="rdhs.tester@gmail.com",
   your_password="rdhstesting",
   your_project="Testing Malaria Investigations",
   desired_surveys=dhs_surveys[1,]),paste0("DHS survey entry 1 of 1 stored in '",dhs_surveys[1,]$output_folder,"'"))




})
