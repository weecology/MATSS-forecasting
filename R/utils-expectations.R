#' @name expect_forecasts
#' @title Check if object is in valid forecasts format
#' 
#' @param forecasts the object of forecasts to be checked
#' @param columns the expecte columns for `forecasts` (defaults to 
#'   `columns = c("observed", "predicted")`)
#' 
#' @export
#' 
expect_forecasts <- function(forecasts, columns = c("observed", "predicted"))
{
    eval(bquote(testthat::expect_true(is.data.frame(.(forecasts)))))
    eval(bquote(testthat::expect_true(all(.(columns) %in% names(.(forecasts))))))
    eval(bquote(testthat::expect_equal(NCOL(.(forecasts)), length(.(columns)))))
}