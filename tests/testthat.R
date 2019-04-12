library(testthat)
library(MATSSforecasting)

if ("sample.kind" %in% names(formals(RNGkind)))
{
    suppressWarnings(RNGkind(sample.kind = "Rounding"))
}

test_check("MATSSforecasting")
