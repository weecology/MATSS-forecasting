#' @name ets_ts
#' @title Make forecasts using exponentially smoothed time series
#' 
#' @description Fit an exponentially smoothed time series and make forecasts. 
#'   Here, the frequency of the data is set a priori, as opposed to estimating 
#'   the parameter from the data
#' 
#' @param level the CI level to include
#' @inheritParams forecast_wrapper
#' @inheritParams stats::ts
#' 
#' @return a data.frame of the mean forecasts, the observed values, and the 
#'   lower and upper CI levels (if an error occurs, then just NA values)
#' 
#' @export
#' 
ets_ts <- function(ts, num_ahead = 5, level = 95, frequency = 1)
{
    f <- function(training, observed, level, frequency)
    {
        # make forecasts
        ts_model <- forecast::ets(ts(training, frequency = frequency))
        forecasts <- forecast::forecast(ts_model, num_ahead, level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    forecast_wrapper(f, ts, num_ahead, level, frequency)
}