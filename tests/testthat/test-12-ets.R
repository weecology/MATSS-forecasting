context("Exponentially smoothed and structural time series functions")

test_that("ets_ts error checking works for too short time series", {
    expect_warning(ets_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- ets_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning an NA object")
    expect_forecasts(output)
    expect_known_hash(output, "cca7c70d85")
})

test_that("sts_ts error checking works for too short time series", {
    expect_warning(sts_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- sts_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning an NA object")
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

test_that("sts_ts function works", {
    ts <- Nile
    expect_error(output <- sts_ts(ts), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "356e4d7c4d")
    
    expect_error(output <- sts_ts(ts, frequency = 4), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "c5b61a9433")
})