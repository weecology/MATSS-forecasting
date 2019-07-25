#' @name ranfor_ts
#' @title Make forecasts using a random forest model
#'
#' @description Fit a random forest model with the output as the 1-step ahead 
#'   value and the predictors as the 5 previous lags. This is functionally 
#'   similar to the time-delay embedding approach with an embedding of 5, but 
#'   using \code{\link[randomForest]{randomForest}} to estimate the forecast 
#'   function.
#' 
#' @inheritParams forecast_one_step_static
#' @inheritParams forecast::forecast
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @export

ranfor_ts <- function(timeseries, num_ahead = 5, level = 95)
{
    f <- function(training, observed, level)
    {
        if (length(training) < 6)
        {
            stop("training data is not long enough for fixed embedding dimension of 5")
        }
        n <- length(training)
        ranfor_model <- randomForest::randomForest(y = training[6:n], 
                                                   x = cbind(training[1:(n-5)],
                                                             training[2:(n-4)],
                                                             training[3:(n-3)],
                                                             training[4:(n-2)],
                                                             training[5:(n-1)]))
        
        # we assume that the `observed`` portion of the time series immediately 
        #   follows the `training` portion of the time series
        newdata <- tail(training, n = 5)
        predicted <- numeric(length(observed))
        
        # make 1-step ahead forecasts and propagate those values to make 
        #   subsequent forecasts
        for (i in seq_along(observed))
        {
            predicted[i] <- randomForest:::predict.randomForest(ranfor_model, newdata = newdata)
            newdata <- c(tail(newdata, n = -1), predicted[i])
        }
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = predicted,
                   lower_CI = NA,
                   upper_CI = NA)
    }

    forecast_one_step_static(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   level = level)
}
