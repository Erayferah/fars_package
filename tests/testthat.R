library(testthat)

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

test_that("fars make_filename function tests", {
  testthat::expect_that(make_filename(2013), testthat::equals("accident_2013.csv.bz2"))
  testthat::expect_that(make_filename(2014), testthat::equals("accident_2014.csv.bz2"))
  testthat::expect_that(make_filename(2015), testthat::equals("accident_2015.csv.bz2"))
})
