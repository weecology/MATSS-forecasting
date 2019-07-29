#' @title Naive one-step ahead forecast
#' @description This model forecasts the next step ahead as the current value, 
#'   and is a baseline model for comparison against.
#'
#' @inheritParams hindcast
#'
#' @return a data.frame of the forecasts and the observed values
#'
#' @export
#'
naive_one_step <- function(timeseries, ...)
{
    f <- function(training, observed = NA, level)
    {
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = utils::tail(training, 1),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    hindcast(fun = f, timeseries = timeseries, ...)
}