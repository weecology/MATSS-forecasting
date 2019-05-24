#' @name lm_ts
#' @title Make forecasts using a Linear Model
#'
#' @description Fit a Linear Regression for forecasting the time series as a 
#'   linear function over time/years
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
                   lower_CI = as.numeric(qnorm(0.5 - level/200, forecasts$fit, forecasts$se.fit)),
                   upper_CI = as.numeric(qnorm(0.5 + level/200, forecasts$fit, forecasts$se.fit)))
    }
    
    make_forecasts(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   order = order, level = level)
}
# 
# # Simple linear regression model
# n.y = length(y) # length of time series
# y.train = y[1:(n.y-5)]
# y.test = y[(n.y-5+1):n.y]
# x = seq(1,length(y.train))
# 
# # Simple regression approach using lm() - observation error only
# lmfit = lm(y.train~x)
# # predict function for gam requires a new list of predictor variables as newdata
# #predicted[[2]] = predict(gamfit, newdata = list("x" = seq(1,n.y)))[(n.y-NAHEAD+1):n.y]
# pred = predict(lmfit, newdata = list("x" = seq(1,n.y)), se.fit=TRUE)
# predicted[which(dat$ID==w)[(n.y-NAHEAD+1):n.y],model.count] = pred$fit[(n.y-NAHEAD+1):n.y]
# #predictedSE[which(dat$ID==w)[(n.y-NAHEAD+1):n.y],model.count] = pred$se.fit[(n.y-NAHEAD+1):n.y]
# predictedSE[which(dat$ID==w)[(n.y-NAHEAD+1):n.y],model.count] = pred$residual.scale
# model.output[[w,model.count]] = lmfit  
# model.names[model.count] = "simple regression"    
