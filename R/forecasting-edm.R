#' @name simplex_ts
#' @title Make forecasts using simplex projection
#'
#' @description Fit a simplex projection model using \code{\link[rEDM]{simplex}}
#'   and make forecasts.
#' 
#' @param E_list the values of E to try
#' @inheritParams make_forecasts
#' @inheritParams forecast::forecast
#' @inheritParams rEDM::simplex
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @export

simplex_ts <- function(timeseries, num_ahead = 5, level = 95, E_list = 1:7, 
                       silent = TRUE)
{
    f <- function(training, observed, level, E_list, silent = silent)
    {
        # fit simplex model
        training_model <- rEDM::simplex(training, E = E_list, silent = silent)
        best_E <- training_model$E[which.min(training_model$mae)]

        # make 1-step ahead forecasts and propagate those values to make 
        #   subsequent forecasts
        forecasts <- data.frame(fit = numeric(length(observed)), 
                                se.fit = numeric(length(observed)))
        input_ts <- training
        for (i in seq_along(observed))
        {
            out <- rEDM::simplex(time_series = c(input_ts, NA), 
                                 lib = c(1, length(input_ts)), 
                                 pred = c(length(input_ts) - best_E, length(input_ts) + 1), 
                                 E = best_E, 
                                 stats_only = FALSE, 
                                 silent = silent)
            pred_row <- out$model_output[[1]][best_E + 1, ]
            forecasts$fit[i] <- pred_row$pred
            forecasts$se.fit[i] <- sqrt(pred_row$pred_var)
            input_ts <- c(input_ts, pred_row$pred)
        }
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$fit),
                   lower_CI = as.numeric(qnorm(0.5 - level/200, forecasts$fit, forecasts$se.fit)),
                   upper_CI = as.numeric(qnorm(0.5 + level/200, forecasts$fit, forecasts$se.fit)))
    }
    
    make_forecasts(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   level = level, E_list = E_list, silent = silent)
}