context("Forecasting Utility Functions")

test_that("make_forecasts handles an error from `fun`", {
    fun <- function(training, observed)
    {
        stop("This error will be converted into a warning.")
    }
    w <- capture_warnings(make_forecasts(fun, rnorm(100)))
    expect_match(w, "Error in fun\\(training = head\\(timeseries, -num_ahead\\), ")
    expect_match(w, "observed = tail\\(timeseries, : ")
    expect_match(w, "This error will be converted into a warning.")
    expect_match(w, "returning an NA object.")    
})

test_that("make_forecasts handles errors in `ts`", {
    fun <- function(training, observed) {1}
    w <- capture_warnings(make_forecasts(fun, seq(3)))
    expect_NA_warnings(w)
})

test_that("make_forecasts works", {
    fun <- function(training, observed)
    {
        c(num_training = NROW(training), 
          num_observed = NROW(observed), 
          num_finite_training = sum(is.finite(training)), 
          num_finite_observed = sum(is.finite(observed)))
    }
    expect_error(out <- make_forecasts(fun, c(NA, seq(12), NA)), NA)
    expect_equivalent(out, c(7, 5, 7, 5))
    
    expect_error(out <- make_forecasts(fun, c(NA, seq(12), NA), num_ahead = 7), NA)
    expect_equivalent(out, c(5, 7, 5, 7))
    
    expect_error(out <- make_forecasts(fun, c(NA, 1, NA, 2, NA, 3, NA, 4, NA, 5, NA, 6, NA, 7, NA, 8, NA)), NA)
    expect_equivalent(out, c(10, 5, 5, 3))
})