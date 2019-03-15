context("Tests of Wrapper Functions")

test_that("forecast_wrapper works for simple functions", {
    # setup sample inputs
    data <- MATSS::get_sgs_data()
    fun <- function(ts) {
        tibble::tibble(n = NROW(ts), 
                       mean = mean(ts), 
                       sd = sd(ts))
    }
    
    # check if it worked properly and had the right size
    expect_error(output <- forecast_wrapper(data, fun), NA)
    expect_equal(dim(output), c(1, 4))
    
    # check results data.frame
    expect_error(results <- output$results[[1]], NA)
    expect_equal(dim(results), c(11, 4))
    expect_equal(results$id, names(data$abundance))
    expect_true(output$dataset == "data")
    expect_true(output$method == "fun")
    
    # check metadata
    expect_identical(output$metadata[[1]], data$metadata)
    
    # check digest
    expect_known_hash(digest::digest(output), "2eb0517caa")
})
