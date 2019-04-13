#' @name gam_ts
#' @title Make forecasts using a Generalized Additive Model
#'
#' @description Fit a Generalized Additive Model (GAM) with spline over
#'   time/years and use it to make forecasts
#'
#' @inheritParams make_forecasts
#' @inheritParams forecast::forecast
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @importFrom stats qnorm
#' @export
#'
gam_ts <- function(timeseries, num_ahead = 5, level = 95)
{
    f <- function(training, observed, order, level)
    {
        # make forecasts
        t <- seq_len(length(training))
        gam_model <- mgcv::gam(training ~ s(t)) # gam model with spline over time

        # predict function requires a new list of predictor variables as newdata
        t_observed <- length(training) + seq_len(length(observed))
        forecasts <- mgcv::predict.gam(gam_model,
                                       newdata = list("t" = t_observed),
                                       se.fit = TRUE)

        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$fit),
                   lower_CI = as.numeric(qnorm(0.5 - level/200, forecasts$fit, forecasts$se.fit)),
                   upper_CI = as.numeric(qnorm(0.5 + level/200, forecasts$fit, forecasts$se.fit)))
    }

    make_forecasts(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   order = order, level = level)
}
