#' @title A wrapper function for forecasting methods
#' 
#' @description This function faciliates the forecasting approach in Ward et al. 
#'   2014: 
#'   \enumerate{
#'     \item fit the model to all but the last `num_ahead` time points
#'     \item make 1-step ahead forecasts, and feed each forecast in as the 
#'           observed, until `num_ahead` total forecasts are made
#'   }
#' 
#' @param fun a function for doing the forecasting. It should have arguments:
#'   \describe{
#'     \item{training}{the data for training the model}
#'     \item{observed}{the observed values to be forecasted}
#'     \item{...}{any optional arguments}
#'   } and should return a data.frame with at least the observed and predicted 
#'   values (other columns are optional, and may be specific to the forecasting 
#'   method)
#' @param ts the time series to forecast
#' @param num_ahead the number of points at the end of the time series to 
#'   forecast
#' @param ... arguments to pass to `fun`
#' 
#' @return a data.frame of the observed and predicted values, with any other 
#'   remaining values returned by `fun`. If and error occurs, then NA values for 
#'   the observed and predicted
#' 
make_forecasts <- function(fun, ts, num_ahead = 5, ...)
{
    tryCatch(
        {
            # trim NAs off of front and back
            idx <- is.finite(ts)
            ts_bounds <- range(which(idx))
            ts <- ts[seq(ts_bounds[1], ts_bounds[2])]
            
            # set up model
            num_points <- length(ts)
            training_subset <- seq_len(num_points - num_ahead)
            
            # make forecasts
            fun(training = ts[training_subset], 
                observed = tail(ts, num_ahead), 
                ...)
            
        }, error = function(e) {
            warning(e, "  returning a NA object")
            return(data.frame(observed = NA,
                              predicted = NA))
        })
}