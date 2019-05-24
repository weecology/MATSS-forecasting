context("MARSS state space time series functions")

test_that("marss_ts error checking works for too short time series", {
    w <- capture_warnings(output <- marss_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("marss_ts function works", {
    expect_error(output <- marss_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "9c3bc1db43")
})

test_that("marss_ts function works without drift", {
    expect_error(output <- marss_ts(Nile, drift = FALSE), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "d850380eae")
})
