context("Build Plans")

data_file <- file.path(tempdir(), "ward_data.RDS")

test_that("Drake plan for Ward et al. databases works", {
    expect_error(data_plan <- build_ward_data_plan(data_file), NA)
    expect_equal(dim(data_plan), c(8, 2))
    expect_equal(data_plan$target, c("data_.salmon.", "data_.RAMlegacy_catch.", 
                                     "data_.RAMlegacy_ssb.", "data_.RAMlegacy_recperssb.", 
                                     "data_.Dorner2008.", "data_.LPI.",
                                     "data_.SprSum_Col_Chinook.", "data_.PugSound_Chinook."))
    expect_true(all(grepl("data_\\..+\\.", data_plan$target)))
})

test_that("Drake plan for Ward et al. methods works", {
    expect_error(methods_plan <- build_ward_methods_plan(), NA)
    expect_equal(dim(methods_plan), c(38, 2))
    expect_true(all(grepl("MATSS::analysis_wrapper\\(.+\\)", methods_plan$command)))
})
