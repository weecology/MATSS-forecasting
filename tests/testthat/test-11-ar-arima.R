context("AR/ARIMA functions")

test_that("arima_fracdiff_ts error checking works for too short time series", {
    expect_warning(arima_fracdiff_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- arima_fracdiff_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning a NA object")
    expect_forecasts(output)
    expect_known_hash(output, "cca7c70d85")
})

test_that("randomwalk_ts error checking works for too short time series", {
    expect_warning(randomwalk_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- randomwalk_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning a NA object")
    expect_forecasts(output)
    expect_known_hash(output, "cca7c70d85")
})

test_that("arima_ts error checking works for too short time series", {
    expect_warning(arima_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- arima_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning a NA object")
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

test_that("arima_ts function works", {
    ts <- Nile
    expect_error(output <- arima_ts(ts), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(round(output, 4), "41466ee2a8")
})

test_that("arima_ts function works with different order", {
    ts <- Nile
    expect_error(output <- arima_ts(ts, order = c(2, 0, 1)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(round(output, 4), "44104d07fa")
})
    