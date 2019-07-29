#' Forecase using a random walk model
#'
#' @description Fits a random walk model using \code{\link[forecast]{rwf}}
#'
#' @inheritParams forecast::rwf
#' @inheritParams forecast_iterated
#' @inheritParams forecast::forecast
#' 
#' @export
#'
randomwalk_ts <- function(timeseries, num_ahead = 5, level = 95,
                          drift = FALSE)
{
    f <- function(training, observed, level, drift)
    {
        # make forecasts
        forecasts <- forecast::rwf(training, NROW(observed), drift = drift, level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    forecast_iterated(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                             level = level, drift = drift)
}