context("Check Permutation Entropy Functions")

test_that("embedd works", {
    ts <- as.numeric(1:10)
    expect_equal(embedd(ts, 3), matrix(c(1:8, 2:9, 3:10), ncol = 3))
    expect_equal(embedd(ts, 1), matrix(1:10, ncol = 1))
    expect_error(embedd(ts, 11), "Insufficient observations for the requested embedding")
    
    expect_equal(embedd(ts, 3, as.embed = TRUE), matrix(c(3:10, 2:9, 1:8), ncol = 3))
    expect_equal(embedd(ts, 3, 3), matrix(c(1:4, 4:7, 7:10), ncol = 3))
    
    ts <- rnorm(10)
    expect_equal(embedd(ts, 3, indices = TRUE), matrix(c(1:8, 2:9, 3:10), ncol = 3))
})

test_that("word_distribution works", {
    ts <- as.numeric(Nile)
    x_emb <- embedd(ts, 3)
    expect_equal(word_distribution(x_emb, weighted = FALSE), 
                 c(17, 18, 14, 16, 20, 13) / 98)
    expect_equal(round(word_distribution(x_emb), 4), 
                 c(0.2129, 0.1580, 0.1006, 0.1514, 0.2047, 0.1724))
})

test_that("permutation entropy calculations work", {
    ts <- as.numeric(Nile)
    expect_equal(round(PE(ts, 2), 4), 0.9995)
    expect_equal(round(PE(ts, 3), 4), 0.9853)
    expect_equal(round(PE(ts, 4), 4), 0.9394)

    expect_equal(round(PE(ts, 2, weighted = FALSE), 4), 0.9993)
    expect_equal(round(PE(ts, 3, weighted = FALSE), 4), 0.9942)
    expect_equal(round(PE(ts, 4, weighted = FALSE), 4), 0.9684)
    
    expect_equal(round(PE(ts, 4, ties.method = "last"), 4), 0.9362)
    expect_equal(round(PE(ts, 4, ties.method = "average"), 4), 0.8023)
})
