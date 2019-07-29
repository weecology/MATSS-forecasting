context("Forecasting Utility Functions")

test_that("trim_NA works", {
    expect_equal(trim_NA(1:10), 1:10)
    expect_equal(trim_NA(c(1:10, NA)), 1:10)
    expect_equal(trim_NA(c(NA, NA, 1:10)), 1:10)
    expect_equal(trim_NA(c(NA, NA, NA, 1:10, NA)), 1:10)
})

test_that("compute_hindcast_method works", {
    expect_error(compute_hindcast_method(0, "asdf"), 
                 "Unexpected value for `hindcast_method`: asdf")
    expect_error(compute_hindcast_method(10, "pred_frac", pred_frac = 1), 
                 "Bad value for `pred_frac`: 1")
    expect_error(compute_hindcast_method(10, "pred_frac", pred_frac = -0.5), 
                 "Bad value for `pred_frac`: -0.5")
    expect_error(compute_hindcast_method(10, "last_n", last_n = 10), 
                 "Bad value for `last_n`: 10")
    expect_error(compute_hindcast_method(10, "last_n", last_n = -1), 
                 "Bad value for `last_n`: -1")
    expect_error(compute_hindcast_method(10, "pred_start", pred_start = 0), 
                 "Bad value for `pred_start`: 0")
    expect_error(compute_hindcast_method(10, "pred_start", pred_start = 11), 
                 "Bad value for `pred_start`: 11")
    expect_equal(compute_hindcast_method(10, "pred_frac", pred_frac = 1/3), 3)
    expect_equal(compute_hindcast_method(10, "last_n", last_n = 3), 3)
    expect_equal(compute_hindcast_method(10, "pred_start", pred_start = 7), 4)
})

test_that("forecast_iterated handles an error from `fun`", {
    fun <- function(training, observed)
    {
        stop("This error will be converted into a warning.")
    }
    w <- capture_warnings(forecast_iterated(fun, rnorm(100)))
    expect_match(w, "Error in fun\\(training = utils::head\\(timeseries, -num_ahead\\), ")
    expect_match(w, "observed = utils::tail\\(timeseries, : ")
    expect_match(w, "This error will be converted into a warning.")
    expect_match(w, "returning an NA object.")    
})

test_that("forecast_iterated handles errors in `ts`", {
    fun <- function(training, observed) {1}
    w <- capture_warnings(forecast_iterated(fun, seq(3)))
    expect_NA_warnings(w)
})

test_that("forecast_iterated works", {
    fun <- function(training, observed)
    {
        c(num_training = NROW(training), 
          num_observed = NROW(observed), 
          num_finite_training = sum(is.finite(training)), 
          num_finite_observed = sum(is.finite(observed)))
    }
    expect_error(out <- forecast_iterated(fun, c(NA, seq(12), NA)), NA)
    expect_equivalent(out, c(7, 5, 7, 5))
    
    expect_error(out <- forecast_iterated(fun, c(NA, seq(12), NA), num_ahead = 7), NA)
    expect_equivalent(out, c(5, 7, 5, 7))
    
    expect_error(out <- forecast_iterated(fun, c(NA, 1, NA, 2, NA, 3, NA, 4, NA, 5, NA, 6, NA, 7, NA, 8, NA)), NA)
    expect_equivalent(out, c(10, 5, 5, 3))
})

test_that("hindcast handles an error from `fun`", {
    fun <- function(training, observed)
    {
        stop("This error will be converted into a warning.")
    }
    w <- capture_warnings(hindcast(fun, rnorm(100)))
    expect_match(w, "Error in fun\\(training = timeseries\\[1:m\\], ")
    expect_match(w, "observed = timeseries\\[m \\+ 1\\], : ")
    expect_match(w, "This error will be converted into a warning.")
    expect_match(w, "returning an NA object.")
})

test_that("hindcast works", {
    fun <- function(training, observed)
    {
        data.frame(num_training = NROW(training), 
                   num_finite_training = sum(is.finite(training)))
    }
    expect_error(out <- hindcast(fun, c(NA, seq(12), NA)), NA)
    expect_equivalent(out, data.frame(num_training = 8:11, num_finite_training = 8:11))
    
    expect_error(out <- hindcast(fun, c(NA, seq(12), NA), hindcast_method = "last_n", last_n = 7), NA)
    expect_equivalent(out, data.frame(num_training = 5:11, num_finite_training = 5:11))
    
    expect_error(out <- hindcast(fun, c(NA, 1, NA, 2, NA, 3, NA, 4, NA, 5, NA, 6, NA, 7, NA, 8, NA)), NA)
    expect_equivalent(out, data.frame(num_training = 10:14, num_finite_training = c(5, 6, 6, 7, 7)))
})