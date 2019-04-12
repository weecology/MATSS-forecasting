context("GAM time series functions")

test_that("gam_ts error checking works for too short time series", {
    w <- capture_warnings(output <- gam_ts(c(1, NA)))
    expect_NA_warnings(w)
    expect_forecasts(output)
})

test_that("gam_ts function works", {
    expect_error(output <- gam_ts(Nile), NA)
    expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "b06e58c4da")
})
