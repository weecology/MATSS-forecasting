#' @name marss_ts
#' @title Make forecasts using a state space model
#' @aliases marss_rw_one_step
#' 
#' @description Fit a state-space model using \code{\link[MARSS]{MARSS}} and 
#'   make forecasts.
#' 
#' @param drift whether the state-space model should include drift or not
#' @inheritParams forecast_iterated
#' @inheritParams forecast::forecast
#' @inheritParams MARSS::MARSS
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @export
#'
marss_ts <- function(timeseries, num_ahead = 5, level = 95, drift = TRUE, 
                      silent = TRUE)
{
    f <- function(training, observed, level, 
                  method = "BFGS", model = NULL, silent = TRUE)
    {
        marss_model <- MARSS::MARSS(training, method = method, model = model, silent = silent)

        # check for drift component to include in forecasts
        model_param_U <- marss_model$par$U
        if (length(model_param_U) == 0)
        {
            drift_dev <- rep_len(0, length(observed))
        } else {
            drift_dev <- seq_along(observed) * as.vector(model_param_U)
        }
        
        forecasts <- data.frame(fit = tail(marss_model$states[1, ], n = 1) + drift_dev, 
                                se.fit = sqrt(seq_along(observed) * as.vector(marss_model$par$Q)))
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$fit),
                   lower_CI = as.numeric(qnorm(0.5 - level/200, forecasts$fit, forecasts$se.fit)),
                   upper_CI = as.numeric(qnorm(0.5 + level/200, forecasts$fit, forecasts$se.fit)))
    }
    
    if (!drift) # override any default model setting
    {
        model <- list(U = matrix(0, 1, 1))
    } else {
        model <- NULL
    }
    
    forecast_iterated(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   level = level, method = "BFGS", model = model, silent = silent)
}

#' @rdname marss_ts
#'
#' @description `marss_rw_one_step` uses \code{\link[MARSS]{MARSS}} to fit a 
#'   state-space model and make a single one-step forecast. 
#'
#' @export
#'
marss_rw_one_step <- function(timeseries, level = 95, drift = TRUE, silent = TRUE)
{
    f <- function(training, observed, level, 
                  method = "BFGS", model = NULL, silent = TRUE)
    {
        marss_model <- MARSS::MARSS(training, method = method, model = model, 
                                    silent = silent)
        
        # check for drift component to include in forecasts
        model_param_U <- marss_model$par$U
        if (length(model_param_U) == 0)
        {
            drift_dev <- rep_len(0, 1)
        } else {
            drift_dev <- as.vector(model_param_U)
        }
        
        forecasts <- data.frame(fit = tail(marss_model$states[1, ], n = 1) + drift_dev, 
                                se.fit = sqrt(as.vector(marss_model$par$Q)))
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$fit),
                   lower_CI = as.numeric(qnorm(0.5 - level/200, forecasts$fit, forecasts$se.fit)),
                   upper_CI = as.numeric(qnorm(0.5 + level/200, forecasts$fit, forecasts$se.fit)))
    }
    
    if (!drift) # override any default model setting
    {
        model <- list(B = matrix(1), U = matrix(0), Q = matrix("q"), 
                      Z = matrix(1), A = matrix(0), R = matrix("r"), 
                      x0 = matrix("mu"), tinitx = 0)
    } else {
        model <- list(B = matrix(1), U = matrix("u"), Q = matrix("q"), 
                      Z = matrix(1), A = matrix(0), R = matrix("r"), 
                      x0 = matrix("mu"), tinitx = 0)
    }
    
    hindcast(fun = f, timeseries = timeseries, level = level, 
             method = "BFGS", model = model, silent = silent)
}