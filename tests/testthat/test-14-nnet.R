context("Neural network time series functions")

test_that("nnet_ts error checking works for too short time series", {
    w <- capture_warnings(output <- nnet_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("nnet_ts function works", {
    set.seed(42)
    expect_error(suppressWarnings(output <- nnet_ts(Nile)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "26bbb45ca1")
    
    set.seed(42)
    expect_error(suppressWarnings(output <- nnet_ts(Nile, m = 2)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "08c205a8dc")
    
    set.seed(42)
    expect_error(suppressWarnings(output <- nnet_ts(Nile, size = 2)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "71524792e7")
    
    set.seed(42)
    expect_error(suppressWarnings(output <- nnet_ts(Nile, type = "bootstrap")), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "572b7a52b0")
})
