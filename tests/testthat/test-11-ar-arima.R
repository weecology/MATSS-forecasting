context("AR/ARIMA forecasting functions")

test_that("arima_fracdiff_ts error checking works for missing data", {
    expect_warning(arima_fracdiff_ts(NULL), "no non-missing arguments to min; returning Inf")
    expect_warning(arima_fracdiff_ts(NULL), "no non-missing arguments to max; returning -Inf")
    expect_warning(arima_fracdiff_ts(NULL), "Error in seq.default\\(ts_bounds\\[1\\], ts_bounds\\[2\\]\\): ")
    expect_warning(arima_fracdiff_ts(NULL), "'from' must be a finite number[[:space:]]+returning a NA object")
})

test_that("arima_fracdiff_ts error checking works for too short time series", {
    expect_warning(arima_fracdiff_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- arima_fracdiff_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning a NA object")
    expect_forecasts(output)
    expect_known_hash(output, "cca7c70d85")
    
    expect_warning(arima_fracdiff_ts(1:6), "Error in fracdiff::fracdiff\\(xx, nar = 2, drange = drange\\): ")
    expect_warning(output <- arima_fracdiff_ts(1:6), "invalid MINPACK input[[:space:]]+returning a NA object")
    expect_forecasts(output)
    expect_known_hash(output, "cca7c70d85")
})

test_that("arima_fracdiff_ts function works", {
    ts <- Nile
    expect_error(output <- arima_fracdiff_ts(ts), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "4d6fb760da")
})

test_that("randomwalk_ts function works", {
    ts <- Nile
    expect_error(output <- randomwalk_ts(ts), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "5e3187a39b")
})

test_that("randomwalk_ts function works with drift", {
    ts <- Nile
    expect_error(output <- randomwalk_ts(ts, drift = TRUE), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "23d463dbf1")
})
