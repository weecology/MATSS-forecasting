context("MARSS random walk functions")

test_that("marss_one_step function works", {
    expect_error(output <- marss_rw_one_step(Nile[1:6]), NA)
    expect_forecasts(round(output, 4), 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "cb2aa39b83")
})

test_that("marss_one_step function works without drift", {
    expect_error(output <- marss_rw_one_step(Nile[1:6], drift = FALSE), NA)
    expect_forecasts(round(output, 4), 
                     c("time", "observed", "predicted", "lower_CI", "upper_CI"), 
                     known_hash = "4ed57cba4e")
})
