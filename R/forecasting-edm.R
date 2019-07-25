#' @name simplex_ts
#' @aliases smap_ts
#' @title Make forecasts using simplex projection or S-maps
#'
#' @description `simplex_ts` fits a simplex projection model using 
#'   \code{\link[rEDM]{simplex}} and makes forecasts.
#' 
#' @param E_list the values of E to try
#' @inheritParams forecast_one_step_static
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
                                 lib = c(1, length(training)), 
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
    
    forecast_one_step_static(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   level = level, E_list = E_list, silent = silent)
}

#' @rdname simplex_ts
#' 
#' @description `smap_ts` fits an S-map model using 
#'   \code{\link[rEDM]{s_map}} and makes forecasts.
#'   
#' @param theta_list values of theta to test

smap_ts <- function(timeseries, num_ahead = 5, level = 95, E_list = 1:7, 
                    theta_list = c(0, 1e-04, 3e-04, 0.001, 0.003, 0.01, 0.03, 
                                   0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8), 
                    silent = TRUE)
{
    f <- function(training, observed, level, E_list, theta_list, silent = silent)
    {
        # fit simplex model
        training_model <- rEDM::simplex(training, E = E_list, silent = silent)
        best_E <- training_model$E[which.min(training_model$mae)]
        
        # fit s-map model
        training_model <- rEDM::s_map(training, E = best_E, theta = theta_list, 
                                      silent = silent)
        best_theta <- training_model$theta[which.min(training_model$mae)]
        
        # make 1-step ahead forecasts and propagate those values to make 
        #   subsequent forecasts
        forecasts <- data.frame(fit = numeric(length(observed)), 
                                se.fit = numeric(length(observed)))
        input_ts <- training
        for (i in seq_along(observed))
        {
            out <- rEDM::s_map(time_series = c(input_ts, NA), 
                               lib = c(1, length(training)), 
                               pred = c(length(input_ts) - best_E, length(input_ts) + 1), 
                               E = best_E, 
                               theta = best_theta, 
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
    
    forecast_one_step_static(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   level = level, E_list = E_list, theta_list = theta_list, 
                   silent = silent)
}
