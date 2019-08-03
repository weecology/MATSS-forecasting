context("MARSS random walk functions")

test_that("marss_one_step function works", {
    expect_error(output <- marss_rw_one_step(Nile[1:9]), NA)
    expect_forecasts(output, 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "c8116f7add")
})

test_that("marss_one_step function works without drift", {
    expect_error(output <- marss_rw_one_step(Nile[1:9], drift = FALSE), NA)
    expect_forecasts(output, 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "de90c58976")
})
