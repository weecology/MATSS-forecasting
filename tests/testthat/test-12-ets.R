context("Exponentially smoothed and structural time series functions")

test_that("ets_ts error checking works for too short time series", {
    w <- capture_warnings(output <- ets_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("sts_ts error checking works for too short time series", {
    w <- capture_warnings(output <- sts_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("ets_ts function works", {
    expect_error(output <- ets_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "0aa8554faf")
    
    expect_error(output <- ets_ts(Nile, frequency = 4), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "0aa8554faf")
})

test_that("sts_ts function works", {
    expect_error(output <- sts_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "356e4d7c4d")
    
    expect_error(output <- sts_ts(Nile, frequency = 4), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "c5b61a9433")
})