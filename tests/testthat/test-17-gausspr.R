context("Gaussian Process time series functions")

test_that("gausspr_ts error checking works for too short time series", {
    w <- capture_warnings(output <- gausspr_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("gausspr_ts function works", {
    set.seed(42)
    expect_error(suppressWarnings(output <- gausspr_ts(Nile)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "c741447bfe")

    set.seed(42)
    expect_error(suppressWarnings(output <- gausspr_ts(Nile, frequency = 2)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"),
                     known_hash = "c741447bfe")
    
    expect_error(suppressWarnings(output <- gausspr_ts(Nile, kernel = "besseldot")), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"),
                     known_hash = "a7a489675d")
})
