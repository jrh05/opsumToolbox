library(testthat)
library(opsumToolbox)

context("DTG Checks")

test_that("DTG converts regardless of spacing", {
          expect_equal(DTG("01 1700Q JAN 17"), DTG("011700Q JAN 17"))
          expect_equal(DTG("312359ZDEC15"), DTG("312359Z DEC 15"))
  })

test_that("DTG gives warning and returns NA for invalid DTG's", {
  expect_warning(DTG("303030ZDEC18"))
  expect_warning(DTG("010000JJAN17"))
})

# test_that("DTG switches to POSIXct correctly", {
#   expect_equal(as.POSIXct(as.DTG(a <- Sys.time()), tz = "GMT"),
#                as.POSIXct(a, "%Y-%m-%d %H:%M:%S %z"))
# })
