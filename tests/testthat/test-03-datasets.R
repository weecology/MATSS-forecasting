context("Check Datasets")

test_that("LPI data is retrievable and works", {
    expect_error(LPI_data <- get_LPI_data(), NA)
    expect_true(MATSS::check_data_format(LPI_data))
    
    expect_known_hash(is.na(LPI_data$abundance), "e01cc3af74")
    LPI_data$abundance[is.na(LPI_data$abundance)] <- -9999
    expect_known_hash(LPI_data$abundance, "1501d0db1a")
    
    expect_known_hash(LPI_data$covariates, "9643eab6db")
    
    expect_known_hash(is.na(LPI_data$metadata$species_table), "6f07105f1d")
    LPI_data$metadata$species_table[is.na(LPI_data$metadata$species_table)] <- ""
    expect_known_hash(LPI_data$metadata$species_table, "263d6e653b")
    expect_known_hash(LPI_data$metadata$timename, "94a38c2bf6")
    
    expect_known_hash(LPI_data, "31ebd3ca9e")
})