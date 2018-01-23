context("Downloads")

test_that("download", {

  # test that
   dhs_surveys <- rdhs:::available_surveys(your_email="rdhs.tester@gmail.com",
   your_password="rdhstesting",
   your_project="Testing Malaria Investigations",
   output_dir=tempdir(),
   max_urls=2)

   testthat::expect_message(download_datasets(your_email="rdhs.tester@gmail.com",
   your_password="rdhstesting",
   your_project="Testing Malaria Investigations",
   desired_surveys=dhs_surveys[1,]),paste0("\n\nDHS survey entry 1 of 1 stored in '",dhs_surveys[1,]$output_folder,"'\r\n\n\n"),fixed=TRUE)




})

"\n\nDHS survey entry 1 of 1 stored in 'C:\\Users\\Oliver\\AppData\\Local\\Temp\\Rtmp0i1UTJ/Angola/Standard DHS 2015-16'\r\n\n\n"
"\n\nDHS survey entry 1 of 1 stored in 'C:\\Users\\Oliver\\AppData\\Local\\Temp\\Rtmp0i1UTJ/Angola/Standard DHS 2015-16'\r\n\n\n"
