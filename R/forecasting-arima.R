#' @name arima_ts
#' @title Make forecasts using AR / ARIMA
#' @aliases randomwalk_ts, arima_fracdiff_ts
#' 
#' @rdname arima_ts
#' 
#' @description `arima_ts` fits an arima model
#' 
#' @param level the CI level to include
#' @inheritParams make_forecasts
#' @inheritParams forecast::forecast
#' 
#' @return a data.frame of the mean forecasts, the observed values, and the 
#'   lower and upper CI levels (if an error occurs, then just NA values)
#' 
#' @export
#' 
arima_ts <- function(ts, num_ahead = 5, order = c(1, 0, 0), level = 95)
{
    f <- function(training, observed, order, level)
    {
        # make forecasts
        arima_model <- forecast::arima(ts(training), order = order)
        
        forecasts <- forecast::forecast(arima_model, NROW(observed), level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    make_forecasts(fun = f, ts = ts, num_ahead = num_ahead, 
                   order = order, level = level)
}

#' @rdname arima_ts
#' 
#' @description `arima_fracdiff_ts` fits a step-wise fractionally differenced 
#'   auto-arima 
#' 
#' @export
#' 
arima_fracdiff_ts <- function(ts, num_ahead = 5, level = 95)
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
    
    make_forecasts(fun = f, ts = ts, num_ahead = num_ahead, 
                     level = level)
}

#' @rdname arima_ts
#' 
#' @description `randomwalk_ts` fits a random walk model
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
        forecasts <- forecast::rwf(training, NROW(observed), drift = drift, level = level)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    make_forecasts(fun = f, ts = ts, num_ahead = num_ahead, 
                     drift = drift, level = level)
}