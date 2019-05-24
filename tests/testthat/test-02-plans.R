context("Build Plans")

data_file <- file.path(tempdir(), "ward_data.RDS")

test_that("Drake plan for Ward et al. databases works", {
    expect_error(data_plan <- build_ward_data_plan(data_file), NA)
    expect_equal(dim(data_plan), c(8, 2))
    expect_true(all(grepl("data_.+", data_plan$target)))
})

test_that("Drake plan for Ward et al. methods works", {
    expect_error(methods_plan <- build_ward_methods_plan(), NA)
    expect_equal(dim(methods_plan), c(42, 2))
    expect_true(all(grepl("MATSS::analysis_wrapper\\(.+\\)", methods_plan$command)))
})
