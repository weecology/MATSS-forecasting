#' @name gausspr_ts
#' @title Make forecasts using gaussian process regression
#'
#' @description `gausspr_ts` fits a timeseries model using \code{\link[kernlab]{gausspr}}
#'
#' @inheritParams kernlab::gausspr
#' @inheritParams stats::ts
#' @inheritParams forecast_iterated
#' @inheritParams forecast::forecast
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @export
#'
gausspr_ts <- function(timeseries, num_ahead = 5, level = 95, 
                       frequency = 1, kernel = "rbfdot", kpar = "automatic")
{
    f <- function(training, observed, level, frequency, kernel, kpar)
    {
        t <- seq_len(length(training))
        gp_model <- kernlab::gausspr(stats::ts(training, frequency = frequency) ~ t, 
                                     kernel = kernel, kpar = kpar, 
                                     type = "regression", 
                                     variance.model = TRUE)

        # predict function requires a new list of predictor variables as newdata
        t_observed <- length(training) + seq_len(length(observed))
        forecasts <- data.frame(
            fit = kernlab::predict(gp_model, newdata = list(t = t_observed)), 
            se.fit = kernlab::predict(gp_model,  newdata = list(t = t_observed), 
                                      type = "sdeviation") - 
                mean(training, na.rm = TRUE) # correct for scaling constant in kernlab
        )

        # return
        return_forecasts(observed, forecasts, level)
    }

    forecast_iterated(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   level = level, frequency = frequency, kernel = kernel, 
                   kpar = kpar)
}
