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
#' @return a tibble with these columns:
#'   \tabular{ll}{
#'     \code{results} \tab a combined results data.frame\cr
#'     \code{metadata} \tab the metadata component of the original dataset\cr
#'     \code{dataset} \tab the name of the dataset (taken from the passed argument)\cr
#'     \code{method} \tab the name of the forecasting method (taken from the passed argument)\cr
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
    dataset_name <- all.vars(match.call()$dataset)
    method_name <- all.vars(match.call()$fun)
    
    # Make the forecasts
    forecasts <- purrr::map_dfr(dataset$abundance, fun, .id = "id")
    
    # Extract the metadata from the original dataset
    metadata <- dataset$metadata

    # Return the combined results and metadata
    tibble::tibble(results = list(forecasts), 
                   metadata = list(metadata), 
                   dataset = dataset_name, 
                   method = method_name)
}
