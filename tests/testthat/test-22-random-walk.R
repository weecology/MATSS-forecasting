context("Random walk model functions")

test_that("randomwalk_ts error checking works for too short time series", {
    w <- capture_warnings(output <- randomwalk_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
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

test_that("randomwalk_one_step function works", {
    expect_error(output <- randomwalk_one_step(Nile), NA)
    expect_forecasts(output, 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "7cef2b1529")
})

test_that("randomwalk_one_step function works with drift", {
    expect_error(output <- randomwalk_one_step(Nile, drift = TRUE), NA)
    expect_forecasts(output, 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "2f46657822")
})
