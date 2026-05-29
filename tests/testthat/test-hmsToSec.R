test_that("hmsToSec converts '00:00:00' to 0 seconds", {
  expect_equal(hmsToSec("00:00:00"), 0)
})

test_that("hmsToSec converts '01:00:00' to 3600 seconds", {
  expect_equal(hmsToSec("01:00:00"), 3600)
})

test_that("hmsToSec converts '00:01:00' to 60 seconds", {
  expect_equal(hmsToSec("00:01:00"), 60)
})

test_that("hmsToSec converts '01:30:45' correctly", {
  expect_equal(hmsToSec("01:30:45"), 3600 + 30*60 + 45)
})

test_that("hmsToSec is vectorised", {
  res <- hmsToSec(c("00:00:00","00:01:00","01:00:00"))
  expect_equal(res, c(0, 60, 3600))
})

test_that("secToHms converts 0 to '00:00:00'", {
  expect_equal(secToHms(0), "00:00:00")
})

test_that("secToHms converts 3600 to '01:00:00'", {
  expect_equal(secToHms(3600), "01:00:00")
})

test_that("secToHms converts 3661 to '01:01:01'", {
  expect_equal(secToHms(3661), "01:01:01")
})

test_that("hmsToSec and secToHms are inverse operations", {
  original <- c("00:10:30","02:05:00","12:59:59")
  roundtrip <- secToHms(hmsToSec(original))
  expect_equal(roundtrip, original)
})

test_that("secToHms is vectorised", {
  expect_length(secToHms(c(0, 60, 3600)), 3)
})
