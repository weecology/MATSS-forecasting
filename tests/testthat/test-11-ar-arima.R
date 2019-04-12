context("AR/ARIMA functions")

test_that("arima_fracdiff_ts error checking works for too short time series", {
    w <- capture_warnings(output <- arima_fracdiff_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("randomwalk_ts error checking works for too short time series", {
    w <- capture_warnings(output <- randomwalk_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("arima_ts error checking works for too short time series", {
    w <- capture_warnings(output <- arima_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("arima_fracdiff_ts function works", {
    expect_error(output <- arima_fracdiff_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "4d6fb760da")
})

test_that("randomwalk_ts function works", {
    expect_error(output <- randomwalk_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "5e3187a39b")
})

test_that("randomwalk_ts function works with drift", {
    expect_error(output <- randomwalk_ts(Nile, drift = TRUE), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "23d463dbf1")
})

test_that("arima_ts function works", {
    expect_error(output <- arima_ts(Nile), NA)
    expect_forecasts(round(output, 4), 
                     c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "41466ee2a8")
})

test_that("arima_ts function works with different order", {
    expect_error(output <- arima_ts(Nile, order = c(2, 0, 1)), NA)
    expect_forecasts(round(output, 4), 
                     c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "44104d07fa")
})
