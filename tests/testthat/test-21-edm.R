context("rEDM time series forecasting functions")

test_that("simplex_ts error checking works for too short time series", {
    w <- capture_warnings(output <- simplex_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("simplex_ts function works", {
    expect_error(output <- simplex_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "5459da4d0d")
})

test_that("simplex_ts function works for specified E", {
    expect_error(output <- simplex_ts(Nile, E_list = 3:5), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "171330931d")
})
