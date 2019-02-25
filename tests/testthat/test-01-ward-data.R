context("Process Ward et al. Data")

data_file <- file.path(tempdir(), "ward_data.RDS")

test_that("Reshape Ward et al. data works", {
    expect_error(databases <- reshape_ward_data(data_file), NA)
    
    for (database_name in databases)
    {
        expect_error(temp_data <- get_ward_data(database_name, data_file), NA)
        expect_true(MATSS::check_data_format(temp_data))
    }
})

test_that("Drake plan for Ward et al. databases works", {
    expect_error(data_plan <- plan_ward_data(data_file), NA)
    expect_equal(dim(data_plan), c(8, 2))
    expect_true(all(grepl("data_\\..+\\.", data_plan$target)))
})
