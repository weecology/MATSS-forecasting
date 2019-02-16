## original code
# autoarima <- arfima(y.train) 
# f = forecast(autoarima,NAHEAD)
# predicted[which(dat$ID==w)[(n.y-NAHEAD+1):n.y],model.count] = f$mean
# predictedSE[which(dat$ID==w)[(n.y-NAHEAD+1):n.y],model.count] = (f$upper[,2] - f$mean)/1.96 
# model.output[[w,model.count]] = autoarima
# model.names[model.count] = "ARIMA - with frac diff (arfima)"  

autoarima <- function(dataset, num_ahead = 5)
{
    num_points <- NROW(dataset$abundance)
    training_subset <- seq_len(num_points - num_ahead)
    
    # for each time series in dataset
    ts <- dplyr::pull(dataset$abundance, 1)
    
    
    y.train <- ts[training_subset]
    arima_model <- forecast::arfima(y.train) 
    
    f <- forecast::forecast(arima_model, num_ahead, level = 95)
    # 
    
    f$mean
    (f$upper - f$mean)/1.96
    
}


