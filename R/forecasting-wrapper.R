#' @title Apply a forecasting function to all time series in a dataset
#' 
#' @description This wrapper applies a forecasting function to all the time 
#'   series in a dataset, and then coerces the results and metadata into a 
#'   combined object.
#' 
#' @param dataset the dataset object (following the `MATSS` data format)
#' @param fun the forecasting function
#' @param ... extra params to be passed to the forecasting function
#' 
#' @return a tibble with two list-columns:
#'   \tabular{ll}{
#'     \code{results} \tab a combined results data.frame\cr
#'     \code{metadata} \tab the metadata component of the original dataset\cr
#'   }
#'   The results have all the columns returned from the forecasting function, 
#'     with additional columns for the variable that was forecast (pulled from 
#'     the original dataset), and a column for the forecast method
#' 
#' @examples
#' \dontrun{
#'   salmon <- readd(data_.salmon., cache = cache)
#'   xx <- forecast_wrapper(salmon, autoarima_ts)
#' }
#' 
#' @export
#' 
forecast_wrapper <- function(dataset, fun, ...)
{
    # Get the name of the dataset and the forecast method
    method_name <- all.vars(match.call()$fun)

    # Get the variable names
    var_names <- colnames(dataset$abundance)
    
    # Make the forecasts
    forecasts <- purrr::map(dataset$abundance, fun, ...)
    
    # Assemble the formatted output into a single tibble, inserting in the 
    #   variable names, the dataset name, and the forecast method
    results <- 
        purrr::pmap(list(forecasts = forecasts, 
                         var_names = var_names, 
                         method = method_name), 
                    function(forecasts, var_names, method) {
                        dplyr::mutate(forecasts, 
                                      id = var_names, 
                                      method = method)
                    }) %>%
        dplyr::bind_rows()
    
    # Extract the metadata from the original dataset
    metadata <- dataset$metadata

    # Return the combined results and metadata
    tibble::tibble(results = list(results), 
                   metadata = list(metadata))
}