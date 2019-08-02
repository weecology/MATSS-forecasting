context("Exponentially smoothed time series functions")

test_that("ets_ts error checking works for too short time series", {
    w <- capture_warnings(output <- ets_ts(c(1, NA)))
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

test_that("ets_one_step function works", {
    expect_error(output <- ets_one_step(Nile), NA)
    expect_forecasts(output, 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "5545053a24")
})