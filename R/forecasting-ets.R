#' @name ets_ts
#' @title Exponentially smoothed time series model
#'
#' @aliases ets_one_step
#'
#' @description Fit a time series model using \code{\link[forecast]{ets}} and 
#'   make forecasts. The frequency of the data is set a priori, as opposed to 
#'   estimating the parameter from the data.
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
ets_ts <- function(timeseries, num_ahead = 5, level = 95, frequency = 1)
{
    f <- function(training, observed, level, frequency)
    {
        # make forecasts
        ts_model <- forecast::ets(ts(training, frequency = frequency))
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

#' @rdname ets_ts
#'
#' @description `ets_one_step` uses \code{\link[forecast]{ets}} to fit an 
#'   exponential-smoothing time series model and make a single one-step 
#'   forecast. 
#'
#' @export
#'
ets_one_step <- function(timeseries, level = 95)
{
    f <- function(training, observed, level, frequency)
    {
        # make forecasts
        ts_model <- forecast::ets(training)
        forecasts <- forecast::forecast(ts_model, 1, level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    hindcast(fun = f, timeseries = timeseries, level = level)
}
