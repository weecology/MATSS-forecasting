# context("GAM time series functions")
# 
# test_that("gam_ts error checking works for too short time series", {
#     expect_warning(gam_ts(c(1, NA)), "Error in seq_len\\(num_points - num_ahead\\): ")
#     expect_warning(output <- gam_ts(c(1, NA)), "argument must be coercible to non-negative integer[[:space:]]+returning an NA object")
#     expect_forecasts(output)
#     expect_known_hash(output, "cca7c70d85")
# })
# 
# test_that("gam_ts function works", {
#     ts <- Nile
#     expect_error(output <- gam_ts(ts), NA)
#     expect_forecasts(output, c("observed", "predicted", "lower_CI", "upper_CI"))
#     expect_known_hash(output, "b06e58c4da")
# })
