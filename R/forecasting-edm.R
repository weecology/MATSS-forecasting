#' @name simplex_ts
#' @aliases smap_ts simplex_one_step smap_one_step
#' @title Make forecasts using simplex projection or S-maps
#'
#' @description `simplex_ts` uses \code{\link[rEDM]{simplex}} to fit a 
#'   simplex projection model and make forecasts.
#' 
#' @param E_list the values of E to try
#' @inheritParams forecast_iterated
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
    forecast_iterated(fun = simplex_helper, timeseries = timeseries, 
                      num_ahead = num_ahead, level = level, E_list = E_list, 
                      silent = silent)
}

#' @rdname simplex_ts
#' 
#' @description `smap_ts` uses \code{\link[rEDM]{s_map}} to fit an S-map model 
#'   and make forecasts.
#'   
#' @param theta_list values of theta to test
#' 
#' @export
smap_ts <- function(timeseries, num_ahead = 5, level = 95, E_list = 1:7, 
                    theta_list = c(0, 1e-04, 3e-04, 0.001, 0.003, 0.01, 0.03, 
                                   0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8), 
                    silent = TRUE)
{
    forecast_iterated(fun = smap_helper, timeseries = timeseries, 
                      num_ahead = num_ahead, level = level, E_list = E_list, 
                      theta_list = theta_list, silent = silent)
}

#' @rdname simplex_ts
#' 
#' @description `simplex_one_step` uses \code{\link[rEDM]{simplex}} to fit a 
#'   simplex projection model and make a single one-step forecast. 
#' 
#' @export
simplex_one_step <- function(timeseries, level = 95, E_list = 1:7, silent = TRUE)
{
    hindcast(fun = simplex_helper, timeseries = timeseries, 
             level = level, E_list = E_list, silent = silent)
}

#' @rdname simplex_ts
#' 
#' @description `smap_one_step` uses \code{\link[rEDM]{s_map}} to fit an S-map 
#'   model and make a single one-step forecast. 
#' 
#' @export
smap_one_step <- function(timeseries, level = 95, E_list = 1:7, 
                          theta_list = c(0, 1e-04, 3e-04, 0.001, 0.003, 0.01, 0.03, 
                                         0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8), 
                          silent = TRUE)
{
    hindcast(fun = smap_helper, timeseries = timeseries, level = level, 
             E_list = E_list, theta_list = theta_list, silent = silent)
}

simplex_helper <- function(training, observed, level, E_list, silent = silent)
{
    # fit simplex model
    training_model <- rEDM::simplex(training, E = E_list, silent = silent)
    best_E <- training_model$E[which.min(training_model$mae)]
    
    # make 1-step ahead forecasts and propagate those values to make 
    #   subsequent forecasts
    edm_forecast_loop(training, observed, level, silent, 
                      "simplex", E = best_E)
}

smap_helper <- function(training, observed, level, E_list, theta_list, silent = silent)
{
    # fit simplex model
    training_model <- rEDM::simplex(training, E = E_list, silent = silent)
    best_E <- training_model$E[which.min(training_model$mae)]
    
    # fit s-map model
    training_model <- rEDM::s_map(training, E = best_E, theta = theta_list, 
                                  silent = silent)
    best_theta <- training_model$theta[which.min(training_model$mae)]
    
    edm_forecast_loop(training, observed, level, silent, 
                      "s-map", E = best_E, theta = best_theta)
}

edm_forecast_loop <- function(training, observed, level = 95, silent, 
                              fun = c("simplex", "s-map"), E = 1, theta = 1)
{
    fun <- match.arg(fun)
    
    forecasts <- data.frame(fit = numeric(length(observed)), 
                            se.fit = numeric(length(observed)))
    input_ts <- training
    for (i in seq_along(observed))
    {
        out <- switch(fun, 
               "simplex" = rEDM::simplex(time_series = c(input_ts, NA), 
                             lib = c(1, length(training)), 
                             pred = c(length(input_ts) - E, length(input_ts) + 1), 
                             E = E, 
                             stats_only = FALSE, 
                             silent = silent), 
               "s-map" = rEDM::s_map(time_series = c(input_ts, NA), 
                                     lib = c(1, length(training)), 
                                     pred = c(length(input_ts) - E, length(input_ts) + 1), 
                                     E = E, 
                                     theta = theta, 
                                     stats_only = FALSE, 
                                     silent = silent))
        pred_row <- out$model_output[[1]][E + 1, ]
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
