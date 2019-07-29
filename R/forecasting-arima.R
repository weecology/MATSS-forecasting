#' @name arima_ts
#' @title Make forecasts using AR / ARIMA
#' @aliases arfima_ts, autoarima_one_step
#'
#' @rdname arima_ts
#'
#' @description `arima_ts` fits an arima model using \code{\link{arima}}
#'
#' @inheritParams stats::arima
#' @inheritParams forecast_iterated
#' @inheritParams forecast::forecast
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @export
#'
arima_ts <- function(timeseries, num_ahead = 5, level = 95,
                     order = c(1, 0, 0))
{
    f <- function(training, observed, level, order)
    {
        # make forecasts
        arima_model <- stats::arima(training, order = order)
        
        forecasts <- forecast::forecast(arima_model, NROW(observed), level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    forecast_iterated(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                             level = level, order = order)
}

#' @rdname arima_ts
#'
#' @description `arfima_ts` fits a step-wise fractionally differenced
#'   auto-arima using \code{\link[forecast]{arfima}}
#'
#' @export
#'
arfima_ts <- function(timeseries, num_ahead = 5, level = 95)
{
    f <- function(training, observed, level)
    {
        # make forecasts
        arima_model <- forecast::arfima(training)
        forecasts <- forecast::forecast(arima_model, NROW(observed), level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    forecast_iterated(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                                 level = level)
}

#' @rdname arima_ts
#'
#' @description `autoarima_one_step` use \code{\link[forecast]{auto.arima}} to 
#'   fit an ARIMA model. `auto.arima` does model selection on the ARIMA params
#'
#' @inheritParams hindcast
#' 
#' @export
#'
autoarima_one_step <- function(timeseries, level = 95, ...)
{
    f <- function(training, observed = NA, level)
    {
        # make forecasts
        arima_model <- forecast::auto.arima(training)
        forecasts <- forecast::forecast(arima_model, 1, level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    hindcast(fun = f, timeseries = timeseries, level = level, ...)
}