context("Process Ward et al. Data")

data_file <- file.path(tempdir(), "ward_data.RDS")

test_that("Reshape Ward et al. data works", {
    expect_error(databases <- reshape_ward_data(ward_RDS_file = data_file), NA)
    
    for (database_name in databases)
    {
        expect_error(temp_data <- get_ward_data(database_name, data_file), NA)
        expect_true(MATSS::check_data_format(temp_data))
    }
})