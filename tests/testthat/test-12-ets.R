context("Exponentially smoothed time series forecasting functions")

test_that("ets_ts error checking works for missing data", {
    expect_warning(ets_ts(NULL), "no non-missing arguments to min; returning Inf")
    expect_warning(ets_ts(NULL), "no non-missing arguments to max; returning -Inf")
    expect_warning(ets_ts(NULL), "Error in seq.default\\(ts_bounds\\[1\\], ts_bounds\\[2\\]\\): ")
    expect_warning(ets_ts(NULL), "'from' must be a finite number[[:space:]]+returning a NA object")
})

test_that("ets_ts error checking works for too short time series", {
    expect_warning(ets_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- ets_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning a NA object")
    expect_forecasts(output)
    expect_known_hash(output, "cca7c70d85")
})

test_that("ets_ts function works", {
    ts <- Nile
    expect_error(output <- ets_ts(ts), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "0aa8554faf")

    expect_error(output <- ets_ts(ts, frequency = 4), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "0aa8554faf")
})