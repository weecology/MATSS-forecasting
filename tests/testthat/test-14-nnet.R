context("Neural network time series functions")

test_that("nnet_ts error checking works for too short time series", {
    expect_warning(nnet_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- nnet_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning an NA object")
    expect_forecasts(output)
    expect_known_hash(output, "cca7c70d85")
})

test_that("nnet_ts function works", {
    set.seed(42)
    ts <- Nile
    expect_error(suppressWarnings(output <- nnet_ts(ts)), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "26bbb45ca1")
})

test_that("nnet_ts function works with bootstrap SE", {
    set.seed(42)
    ts <- Nile
    expect_error(suppressWarnings(output <- nnet_ts(ts, type = "bootstrap")), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
    expect_known_hash(output, "572b7a52b0")
})
