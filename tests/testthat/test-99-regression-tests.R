context("Regression Tests")

test_that("autoarima_ts works as expected", {
    expect_error(output <- autoarima_ts(Nile), NA)
    expect_identical(digest::digest(output), 
                     "7dfe780731ae1802bd3a85323facf062")
})

test_that("autoarima_ts works on SGS dataset", {
    data <- MATSS::get_sgs_data()
    
    # check if it worked properly and had the right size
    expect_warning(output <- forecast_wrapper(data, autoarima_ts))
    expect_identical(digest::digest(output), 
                     "c1894dd7e9d7a748cecf0d1ea49672bc")
})