#' @title Structural time series model
#'
#' @description Fit a time series model using \code{\link{StructTS}} and 
#'   make forecasts.
#'
#' @inheritParams stats::ts
#' @inheritParams forecast_iterated
#' @inheritParams forecast::forecast
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @export
#'
sts_ts <- function(timeseries, num_ahead = 5, level = 95, frequency = 1)
{
    f <- function(training, observed, level, frequency)
    {
        # make forecasts
        ts_model <- stats::StructTS(ts(training, frequency = frequency))
        forecasts <- forecast::forecast(ts_model, NROW(observed), level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    forecast_iterated(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                      level = level, frequency = frequency)
}

