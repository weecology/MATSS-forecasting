context("rEDM time series forecasting functions")

test_that("simplex_ts error checking works for too short time series", {
    w <- capture_warnings(output <- simplex_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("smap_ts error checking works for too short time series", {
    w <- capture_warnings(output <- smap_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("simplex_ts function works", {
    expect_error(output <- simplex_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "5459da4d0d")
})

test_that("simplex_ts function works for specified E", {
    expect_error(output <- simplex_ts(Nile, E_list = 3), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "e2a5cde309")
})

test_that("smap_ts function works", {
    expect_error(output <- smap_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "f52f0595c9")
})

test_that("smap_ts function works for specified E and theta", {
    expect_error(output <- smap_ts(Nile, E = 3), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "5c324e60c3")
    
    expect_error(output <- smap_ts(Nile, E = 3, theta = 1), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "c1e5612339")
})