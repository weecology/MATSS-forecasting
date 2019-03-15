context("Arima forecasting functions")

test_that("arima_fracdiff_ts error checking works for missing data", {
    expect_warning(arima_fracdiff_ts(NULL), "no non-missing arguments to min; returning Inf")
    expect_warning(arima_fracdiff_ts(NULL), "no non-missing arguments to max; returning -Inf")
    expect_warning(arima_fracdiff_ts(NULL), "Error in seq.default\\(ts_bounds\\[1\\], ts_bounds\\[2\\]\\): ")
    expect_warning(arima_fracdiff_ts(NULL), "'from' must be a finite number[[:space:]]+returning a NA object")
})

test_that("arima_fracdiff_ts error checking works for too short time series", {
    expect_warning(arima_fracdiff_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- arima_fracdiff_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning a NA object")
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(1, 4))
    expect_known_hash(output, "b09dcc61e2")
    
    expect_warning(arima_fracdiff_ts(1:6), "Error in fracdiff::fracdiff\\(xx, nar = 2, drange = drange\\): ")
    expect_warning(output <- arima_fracdiff_ts(1:6), "invalid MINPACK input[[:space:]]+returning a NA object")
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(1, 4))
    expect_known_hash(output, "b09dcc61e2")
})

test_that("arima_fracdiff_ts function works", {
    ts <- Nile
    expect_error(output <- arima_fracdiff_ts(ts), NA)
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(5, 4))
    expect_known_hash(output, "7dfe780731")
})

test_that("randomwalk_ts function works", {
    ts <- Nile
    expect_error(output <- randomwalk_ts(ts), NA)
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(5, 4))
    expect_known_hash(output, "d06159b031")
})

test_that("randomwalk_ts function works with drift", {
    ts <- Nile
    expect_error(output <- randomwalk_ts(ts, drift = TRUE), NA)
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(5, 4))
    expect_known_hash(output, "f5b7be8311")
})
