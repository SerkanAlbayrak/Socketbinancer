library(BinanceR)


ping_api <- binance_api_ping()

test_that("Test 1: binance_api_ping()", {

  # Check Response
  expect_true(ping_api)

  # Check API attribute
  expect_equal(attr(ping_api, "api"), "SPOT")

})

ping_fapi <- binance_fapi_ping()

test_that("Test 2: binance_fapi_ping()", {

  # Check Response
  expect_true(ping_fapi)

  # Check API attribute
  expect_equal(attr(ping_fapi, "api"), "USD-M")

})

ping_dapi <- binance_dapi_ping()

test_that("Test 3: binance_dapi_ping()", {

  # Check Response
  expect_true(ping_dapi)

  # Check API attribute
  expect_equal(attr(ping_fapi, "api"), "COIN-M")

})

ping_eapi <- binance_eapi_ping()

test_that("Test 4: binance_eapi_ping()", {

  # Check Response
  expect_true(ping_eapi)

  # Check API attribute
  expect_equal(attr(ping_eapi, "api"), "OPTIONS")

})
