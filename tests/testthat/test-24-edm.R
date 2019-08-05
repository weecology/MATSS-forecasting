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
                     known_hash = "0744d46e1a")
})

test_that("simplex_ts function works for specified E", {
    expect_error(output <- simplex_ts(Nile, E_list = 3), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "c3e227e446")
})

test_that("smap_ts function works", {
    expect_error(output <- smap_ts(Nile), NA)
    expect_forecasts(round(output, 4), c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "6df6c653d1")
})

test_that("smap_ts function works for specified E and theta", {
    expect_error(output <- smap_ts(Nile, E = 3), NA)
    expect_forecasts(round(output, 4), c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "30e8ccd2e5")
    
    expect_error(output <- smap_ts(Nile, E = 3, theta = 1), NA)
    expect_forecasts(round(output, 4), c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "60b217dce8")
})

test_that("simplex_one_step function works", {
    expect_error(output <- simplex_one_step(Nile), NA)
    expect_forecasts(output, c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "7ca30a402b")
})

test_that("simplex_one_step function works for specified E", {
    expect_error(output <- simplex_one_step(Nile, E_list = 3), NA)
    expect_forecasts(output, c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "ac43f126fb")
})

test_that("smap_one_step function works", {
    expect_error(output <- smap_one_step(Nile), NA)
    expect_forecasts(round(output, 4), c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "7dab2efa6b")
})

test_that("smap_one_step function works for specified E and theta", {
    expect_error(output <- smap_one_step(Nile, E = 3), NA)
    expect_forecasts(round(output, 4), c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "b92df8a035")
    
    expect_error(output <- smap_one_step(Nile, E = 3, theta = 1), NA)
    expect_forecasts(round(output, 4), c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "431d965599")
})