#' @name arima_fracdiff_ts
#' @aliases randomwalk_ts
#' @title Make forecasts using AR / ARIMA
#' 
#' @description `arima_fracdiff_ts` fits a step-wise fractionally differenced 
#'   auto-arima 
#' 
#' @param num_ahead the number of points at the end of the time series to forecast
#' @inheritParams forecast_wrapper
#' @inheritParams forecast::forecast
#' 
#' @return a data.frame of the mean forecasts, the observed values, and the 
#'   lower and upper CI levels (if an error occurs, then just NA values)
#' 
#' @export
#' 
arima_fracdiff_ts <- function(ts, num_ahead = 5, level = 95)
{
    f <- function(training, observed, level)
    {
        # make forecasts
        ts_model <- forecast::arfima(training) 
        forecasts <- forecast::forecast(ts_model, num_ahead, level = level)

        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    forecast_wrapper(f, ts, num_ahead, level)
}

#' @rdname arima_fracdiff_ts
#' 
#' @description `randomwalk_ts` uses a random walk model
#' 
#' @inheritParams forecast::rwf
#' 
#' @export
#' 
randomwalk_ts <- function(ts, num_ahead = 5, drift = FALSE, level = 95)
{
    f <- function(training, observed, drift, level)
    {
        # make forecasts
        forecasts <- forecast::rwf(training, num_ahead, drift = drift, level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    forecast_wrapper(f, ts, num_ahead, drift, level)
}
