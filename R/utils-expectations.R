#' @name expect_forecasts
#' @title Check if object is in valid forecasts format
#' 
#' @param output the object of forecasts to be checked
#' @param columns the expecte columns for `output` (defaults to 
#'   `columns = c("observed", "predicted")`)
#' @param known_hash a known_hash for `output`
#' 
#' @export
#' 
expect_forecasts <- function(output, columns = c("observed", "predicted"), 
                             known_hash = "cca7c70d85")
{
    eval(bquote(testthat::expect_true(is.data.frame(.(output)))))
    eval(bquote(testthat::expect_true(all(.(columns) %in% names(.(output))))))
    eval(bquote(testthat::expect_equal(NCOL(.(output)), length(.(columns)))))
    
    if (!is.null(known_hash))
    {
        eval(bquote(testthat::expect_known_hash(.(output), .(known_hash))))
    }
}

#' @name expect_NA_warnings
#' @title Check if warnings are expected for a too-short time series
#' 
#' @param w the captured warnings
#' 
#' @export
#' 
expect_NA_warnings <- function(w)
{
    eval(bquote(testthat::expect_match(.(w), "Error in doTryCatch\\(return\\(expr\\), name, parentenv, handler\\): ")))
    eval(bquote(testthat::expect_match(.(w), "time series is not long enough")))
    eval(bquote(testthat::expect_match(.(w), "returning an NA object\\.")))
}
