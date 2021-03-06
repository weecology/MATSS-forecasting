context("AR/ARIMA functions")

test_that("arima_ts error checking works for too short time series", {
    w <- capture_warnings(output <- arima_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("arfima_ts error checking works for too short time series", {
    w <- capture_warnings(output <- arfima_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
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

test_that("arfima_ts function works", {
    expect_error(output <- arfima_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "4d6fb760da")
})

test_that("autoarima_one_step function works", {
    expect_error(output <- autoarima_one_step(Nile), NA)
    expect_forecasts(round(output, 4), 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "baee6ada23")    
})

test_that("arfima_one_step function works", {
    expect_error(output <- arfima_one_step(Nile), NA)
    expect_forecasts(output, c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "6b22018b55")    
})