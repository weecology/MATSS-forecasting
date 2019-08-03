context("MARSS state space time series functions")

test_that("marss_ts error checking works for too short time series", {
    w <- capture_warnings(output <- marss_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("marss_ts function works", {
    expect_error(output <- marss_ts(Nile[1:30]), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "ac35ee4c82")
})

test_that("marss_ts function works without drift", {
    expect_error(output <- marss_ts(Nile[1:30], drift = FALSE), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "ac50aa5f78")
})

test_that("marss_one_step function works", {
    expect_error(output <- marss_rw_one_step(Nile[1:9]), NA)
    expect_forecasts(output, 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "c8116f7add")
})

test_that("marss_one_step function works without drift", {
    expect_error(output <- marss_rw_one_step(Nile[1:9], drift = FALSE), NA)
    expect_forecasts(output, 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "de90c58976")
})
