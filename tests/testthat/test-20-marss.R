context("MARSS state space time series functions")

test_that("marss_ts error checking works for too short time series", {
    w <- capture_warnings(output <- marss_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("marss_ts function works", {
    expect_error(output <- marss_ts(Nile[1:30]), NA)
    expect_forecasts(round(output, 4), c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "c7a47fabf3")
})

test_that("marss_ts function works without drift", {
    expect_error(output <- marss_ts(Nile[1:30], drift = FALSE), NA)
    expect_forecasts(round(output, 2), c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "d8d87aaa9e")
})

