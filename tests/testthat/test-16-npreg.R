context("Nonparametric kernel regression time series functions")

test_that("npreg_ts error checking works for too short time series", {
    w <- capture_warnings(output <- npreg_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("npreg_ts function works", {
    expect_error(suppressWarnings(output <- npreg_ts(Nile)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "ea5b935c11")
    
    expect_error(suppressWarnings(output <- npreg_ts(Nile, regtype = "lc")), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"),
                     known_hash = "a53dac9912")
    
    expect_error(suppressWarnings(output <- npreg_ts(Nile, bwmethod = "cv.ls")), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"),
                     known_hash = "b73c951fa6")
})
