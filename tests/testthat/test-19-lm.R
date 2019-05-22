context("Linear Regression time series functions")

test_that("lm_ts error checking works for too short time series", {
    w <- capture_warnings(output <- lm_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("lm_ts function works", {
    expect_error(output <- lm_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "af8d97286a")
})
