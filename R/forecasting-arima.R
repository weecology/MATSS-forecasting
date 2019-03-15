#' @name arima_fracdiff_ts
#' @aliases randomwalk_ts
#' @title Make forward forecasts using AR / ARIMA models
#' 
#' @description These functions follow the forecasting approach in Ward et al. 
#'   2014: 
#'   \enumerate{
#'     \item fit the model to all but the last `num_ahead` time points
#'     \item make 1-step ahead forecasts, and feed each forecast in as the 
#'           observed, until `num_ahead` total forecasts are made
#'   }
#'   
#' `arima_fracdiff_ts` fits a step-wise fractionally differenced auto-arima 
#' 
#' @param ts a single time series
#' @param num_ahead the number of points at the end of the time series to forecast
#' 
#' @return a data.frame of the mean forecasts, the observed values, and the 
#'   lower and upper 95 CI levels (if an error occurs, then just NA values)
#' 
#' @export
#' 
arima_fracdiff_ts <- function(ts, num_ahead = 5)
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
            ts_model <- forecast::arfima(ts[training_subset]) 
            
            # make forecasts
            f <- forecast::forecast(ts_model, num_ahead, level = 95)
            
            # return
            data.frame(observed = as.numeric(tail(ts, num_ahead)), 
                       predicted = as.numeric(f$mean), 
                       lower_95 = as.numeric(f$lower), 
                       upper_95 = as.numeric(f$upper))
        }, error = function(e) {
            warning(e, "  returning a NA object")
            return(data.frame(observed = NA, 
                              predicted = NA, 
                              lower_95 = NA, 
                              upper_95 = NA))
        })
}

#' @rdname arima_fracdiff_ts
#' 
#' @description `randomwalk_ts` uses a random walk model
#' @param drift a boolean to allow for drift (by default, `drift = FALSE`)
#' 
#' @export
#' 
randomwalk_ts <- function(ts, num_ahead = 5, drift = FALSE)
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
            f <- forecast::rwf(ts[training_subset], num_ahead, drift = drift, level = 95)
            
            # return
            data.frame(observed = as.numeric(tail(ts, num_ahead)), 
                       predicted = as.numeric(f$mean), 
                       lower_95 = as.numeric(f$lower), 
                       upper_95 = as.numeric(f$upper))
        }, error = function(e) {
            warning(e, "  returning a NA object")
            return(data.frame(observed = NA, 
                              predicted = NA, 
                              lower_95 = NA, 
                              upper_95 = NA))
        })
}