context("Autoarima forecasting functions")

data_file <- file.path(tempdir(), "ward_data.RDS")

test_that("autoarima_ts error checking works for missing data", {
    expect_warning(autoarima_ts(NULL), "no non-missing arguments to min; returning Inf")
    expect_warning(autoarima_ts(NULL), "no non-missing arguments to max; returning -Inf")
    expect_warning(autoarima_ts(NULL), "Error in seq.default\\(ts_bounds\\[1\\], ts_bounds\\[2\\]\\): ")
    expect_warning(autoarima_ts(NULL), "'from' must be a finite number[[:space:]]+returning a NA object")
})

test_that("autoarima_ts error checking works for too short time series", {
    expect_warning(autoarima_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
    expect_warning(output <- autoarima_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning a NA object")
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(1, 4))
    expect_known_hash(output, "b09dcc61e2")
    
    expect_warning(autoarima_ts(1:6), "Error in fracdiff::fracdiff\\(xx, nar = 2, drange = drange\\): ")
    expect_warning(output <- autoarima_ts(1:6), "invalid MINPACK input[[:space:]]+returning a NA object")
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(1, 4))
    expect_known_hash(output, "b09dcc61e2")
})

test_that("autoarima_ts function works", {
    ts <- sunspot.year
    expect_error(output <- autoarima_ts(ts), NA)
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(5, 4))
    expect_known_hash(output, "60434a48e7")
})

test_that("autoarima_ts function works with different num of forecasts", {
    ts <- sunspot.year
    expect_error(output <- autoarima_ts(ts, 11), NA)
    expect_true(is.data.frame(output))
    expect_true(all(c("observed", "predicted", "lower_95", "upper_95") %in% names(output)))
    expect_equal(dim(output), c(11, 4))
    expect_known_hash(output, "10b9492b47")
})