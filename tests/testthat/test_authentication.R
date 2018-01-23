context("Authentication")

test_that("dhs_authenticate works", {
  expect_equal(dhs_authenticate(your_email = "rdhs.tester@gmail.com",
                                your_password = "rdhstesting", your_project = "Testing Malaria Investigations")$proj_id, "111616")
})


