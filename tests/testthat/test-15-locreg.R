context("Locally weighted regression time series functions")

test_that("locreg_ts error checking works for too short time series", {
    w <- capture_warnings(output <- locreg_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("locreg_ts function works", {
    expect_error(suppressWarnings(output <- locreg_ts(Nile)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "ba15b1d76b")
    
    expect_error(suppressWarnings(output <- locreg_ts(Nile, nn = 2.2, deg = 2:3)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "45a98ef95f")
})
