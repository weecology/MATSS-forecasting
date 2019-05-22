context("Random Forest time series functions")

test_that("ranfor_ts error checking works for too short time series", {
    w <- capture_warnings(output <- ranfor_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
    
    w <- capture_warnings(output <- ranfor_ts(1:5))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("ranfor_ts function works", {
    set.seed(42)
    expect_error(suppressWarnings(output <- ranfor_ts(Nile)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "bac969f429")
})
