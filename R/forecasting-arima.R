#' @name arima_ts
#' @title Make forecasts using AR / ARIMA
#' @aliases arfima_ts
#'
#' @rdname arima_ts
#'
#' @description `arima_ts` fits an arima model using \code{link{arima}}
#'
#' @inheritParams stats::arima
#' @inheritParams forecast_one_step_static
#' @inheritParams forecast::forecast
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @importFrom stats ts
#' @export
#'
arima_ts <- function(timeseries, num_ahead = 5, level = 95,
                     order = c(1, 0, 0))
{
    f <- function(training, observed, level, order)
    {
        # make forecasts
        arima_model <- stats::arima(ts(training), order = order)

        forecasts <- forecast::forecast(arima_model, NROW(observed), level = level)

        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }

    forecast_one_step_static(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   level = level, order = order)
}

#' @rdname arima_ts
#'
#' @description `arfima_ts` fits a step-wise fractionally differenced
#'   auto-arima using \code{link{arfima}}
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

    forecast_one_step_static(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                     level = level)
}

