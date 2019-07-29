#' @name lm_ts
#' @title Make forecasts using a Linear Model
#'
#' @description Fit a Linear Regression for forecasting the time series as a 
#'   linear function over time/years
#' 
#' @inheritParams forecast_iterated
#' @inheritParams forecast::forecast
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#' 
#' @export
#'
lm_ts <- function(timeseries, num_ahead = 5, level = 95)
{
    f <- function(training, observed, order, level)
    {
        # make forecasts
        t <- seq_len(length(training))
        lm_model <- stats::lm(training ~ t)
        
        # predict function requires a new list of predictor variables as newdata
        t_observed <- length(training) + seq_len(length(observed))
        forecasts <- stats::predict.lm(lm_model,
                                       newdata = list(t = t_observed),
                                       se.fit = TRUE)
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$fit),
                   lower_CI = as.numeric(stats::qnorm(0.5 - level/200, forecasts$fit, forecasts$se.fit)),
                   upper_CI = as.numeric(stats::qnorm(0.5 + level/200, forecasts$fit, forecasts$se.fit)))
    }
    
    forecast_iterated(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   order = order, level = level)
}