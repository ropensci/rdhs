context("Authentication")

test_that("dhs_authenticate works", {
  expect_equal(rdhs:::dhs_authenticate(your_email = "rdhs.tester@gmail.com",
                                your_password = "rdhstesting",
                                your_project = "Testing Malaria Investigations")$proj_id, "111616")
})

test_that("available_surveys works", {

  # test max_urls for 1
  dhs_cat <- rdhs::available_surveys(output_dir = tempdir(),
                                  your_email = "rdhs.tester@gmail.com",
                                  your_password = "rdhstesting",
                                  your_project = "Testing Malaria Investigations",
                                  max_urls=1)

  # test max_urls works
  expect_equal(dim(dhs_cat),c(1,6))

  # test max_urls for 5
  dhs_cat <- rdhs::available_surveys(output_dir = tempdir(),
                                     your_email = "rdhs.tester@gmail.com",
                                     your_password = "rdhstesting",
                                     your_project = "Testing Malaria Investigations",
                                     max_urls=5)

  # test max_urls works
  expect_equal(dim(dhs_cat),c(5,6))


})
