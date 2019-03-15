#' @title Make forward forecasts using a fractionally-differences ARIMA
#' 
#' @description This function performs the forecasting according to the 
#'   approach in Ward et al. 2014, where the end of the time series is trimmed, 
#'   and a forecasting model is fit to the remainder of the data. This model is 
#'   then used to make 1-step ahead forecasts, projecting the forecasts forward 
#'   for the end of the time series. (i.e. the first forecast is fed into the 
#'   model to make the next forecast, and so forth).)
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

#' @export
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