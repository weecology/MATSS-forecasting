#' @name ets_ts
#' @aliases sts_ts
#' @title Make forecasts using an exponentially smoothed or structural time 
#'   series mdoel
#' 
#' @description Fit a time series model and make forecasts. The frequency of the 
#'   data is set a priori, as opposed to estimating the parameter from the data. 
#'   
#'   \code{ets_ts} fits an exponentially smoothed time series model.
#' 
#' @param level the CI level to include
#' @inheritParams make_forecasts
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
        forecasts <- forecast::forecast(ts_model, NROW(observed), level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    make_forecasts(fun = f, ts = ts, num_ahead = num_ahead, 
                   level = level, frequency = frequency)
}

#' @name sts_ts
#' @rdname ets_ts
#' 
#' @description \code{sts_ts} fits a structural model for a time series by 
#'   maximum likelihood.
#' 
#' @export
#' 
sts_ts <- function(ts, num_ahead = 5, level = 95, frequency = 1)
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
    
    make_forecasts(fun = f, ts = ts, num_ahead = num_ahead, 
                   level = level, frequency = frequency)
}