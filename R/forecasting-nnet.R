#' @name nnet_ts
#' @title Make forecasts using neural network time series model
#' 
#' @description `nnet_ts` fits a neural network time series model with the 
#'   specified parameters
#' 
#' @inheritParams tsDyn::nnetTs
#' @inheritParams make_forecasts
#' @inheritParams forecast::forecast
#' @param ... arguments to be passed to \code{\link[tsDyn]{predict.nlar}}
#' 
#' @return a data.frame of the mean forecasts, the observed values, and the 
#'   lower and upper CI levels (if an error occurs, then just NA values)
#' 
#' @export
#' 
nnet_ts <- function(timeseries, num_ahead = 5, level = 95, 
                    m = 1, size = 1, ...)
{
    f <- function(training, observed, order, level, ...)
    {
        # make forecasts
        nnet_model <- tsDyn::nnetTs(training, m = m, size = size)
        
        nnet_out <- tsDyn:::predict.nlar(nnet_model, n.ahead = num_ahead, 
                            ci = 0.5 + level/200, # adjust for implementation issue
                            ...)
        
        if (is.list(nnet_out) && all(c("pred", "se") %in% names(nnet_out)))
        {
            forecasts <- list(mean = nnet_out$pred, 
                              lower = nnet_out$se[, 1], 
                              upper = nnet_out$se[, 2]
            )
        } else {
            forecasts <- list(mean = nnet_out, lower = NA, upper = NA)
        }
        
        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$mean),
                   lower_CI = as.numeric(forecasts$lower),
                   upper_CI = as.numeric(forecasts$upper))
    }
    
    make_forecasts(fun = f, timeseries = timeseries, num_ahead = num_ahead, 
                   order = order, level = level, ...)
}
