context("Check Permutation Entropy Functions")

test_that("embedd works", {
    ts <- as.numeric(1:10)
    expect_equal(embedd(ts, 3), matrix(c(1:8, 2:9, 3:10), ncol = 3))
  
    
    
      
})