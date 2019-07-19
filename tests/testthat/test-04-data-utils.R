context("Check Data Processing Utils")

test_that("compute_subset_range error checking works", {
    expect_error(compute_subset_range("asdf", 100), 
                 "`bounds` was not a recognized format.")
    expect_error(compute_subset_range(numeric(0), 100), 
                 "`bounds` was not a recognized format.")
    expect_error(compute_subset_range(c(-1, 1), 100), 
                 "`bounds` was not a recognized format.")
    expect_error(compute_subset_range(c(0, 1.1), 100), 
                 "`bounds` was not a recognized format.")
    expect_error(compute_subset_range(c(0.5, 0.4), 100), 
                 "`bounds` was not a recognized format.")
})

test_that("compute_subset_range works", {
    expect_equal(compute_subset_range(c(0, 1), 100), 
                 1:100)
    expect_equal(compute_subset_range(c(0, 1), 2), 
                 1:2)
    expect_equal(compute_subset_range(c(0, 1), 1), 
                 1)
    
    expect_equal(compute_subset_range(c(0, 0.5), 8), 
                 1:4)
    expect_equal(compute_subset_range(c(0.5, 1), 8), 
                 5:8)
    expect_equal(compute_subset_range(c(0, 0.5), 7), 
                 1:3)
    expect_equal(compute_subset_range(c(0.5, 1), 7), 
                 4:7)
    
    expect_equal(compute_subset_range(c(0, 1/3), 6), 
                 1:2)
    expect_equal(compute_subset_range(c(1/3, 2/3), 6), 
                 3:4)
    expect_equal(compute_subset_range(c(2/3, 1), 6), 
                 5:6)

    expect_equal(compute_subset_range(c(0, 1/3), 7), 
                 1:2)
    expect_equal(compute_subset_range(c(1/3, 2/3), 7), 
                 3:4)
    expect_equal(compute_subset_range(c(2/3, 1), 7), 
                 5:7)
    
    expect_equal(compute_subset_range(c(0, 1/3), 8), 
                 1:2)
    expect_equal(compute_subset_range(c(1/3, 2/3), 8), 
                 3:5)
    expect_equal(compute_subset_range(c(2/3, 1), 8), 
                 6:8)
})