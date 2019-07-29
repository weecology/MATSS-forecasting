#' @title Hindcasting with one-step forecasts
#'
#' @description This function performs hindcasting over a specified segment of 
#'   the time series. The hindcasting is intended to simulate the use of a 
#'   forecasting method in practice. For a given time series of length `m`, 
#'   suppose that the last `n` points are used for forecast evaluation, then 
#'   the procedure is:
#'   \enumerate{
#'     \item fit the model to the first `m - n` time points
#'     \item make a 1-step ahead forecast
#'     \item add the next observation to the training data, refit the model, 
#'       and make another forecast
#'     \item repeat for the `n` forecasts
#'   }
#'
#'   The value of `n` is determined in one of several ways, depending on the 
#'   `hindcast_selection` argument:
#'   \describe{
#'     \item{`last_n`}{make hindcasts for the last `last_n` points of the time 
#'       series}
#'     \item{`pred_frac`}{make hindcasts for the last `pred_frac` fraction of 
#'       the time series}
#'     \item{`pred_start`}{make hindcasts starting with the `pred_start` point 
#'       of the time series}
#'   }
#'
#' @param fun a function for doing the forecasting. It should have arguments:
#'   \describe{
#'     \item{training}{the data for training the model}
#'     \item{observed}{optional observation of the next point}
#'     \item{...}{any optional arguments}
#'   } and should return a data.frame with at least the predicted forecast (in 
#'   a column named `predicted`). Other columns are optional, and may be 
#'   specific to the forecasting method.
#' @param timeseries the time series to forecast
#' @param hindcast_selection the method for determining # of hindcasts
#' @param pred_frac the fraction of points at the end of the time series to 
#'   forecast
#' @param last_n the number of points at the end of the time series to
#'   forecast
#' @param from_start the index of the point of the time series at which to 
#'   begin forecasts
#' @param ... arguments to pass to `fun`
#'
#' @return a data.frame of the observed and predicted values, with any other
#'   remaining values returned by `fun`. If any error occurs, then NA values for
#'   the observed and predicted
#'
#' @export
hindcast <- function(fun, timeseries, 
                     hindcast_method = c("pred_frac", "last_n", "pred_start"), 
                     pred_frac = 1/3, last_n = 5, pred_start = 31, ...)
{
    tryCatch(
        {
            timeseries <- trim_NA(timeseries)
            
            # identify where to start forecasts
            hindcast_method <- match.arg(hindcast_method)
            last_n <- compute_hindcast_method(length(timeseries), 
                                              hindcast_method = hindcast_method, 
                                              pred_frac = pred_frac, 
                                              last_n = last_n, 
                                              pred_start = pred_start)
            
            # make forecasts
            #   for each 1-step ahead, give updated training data to fun
            #   note: this isn't the most efficient, since many forecasting 
            #     methods have updating procedures where it is computational 
            #     cheap to add an additional data point, but this procedure 
            #     here is generic
            training_end_idx <- seq(to = length(timeseries) - 1, length.out = last_n)
            purrr::map_dfr(training_end_idx, function(m) {
                fun(training = timeseries[1:m], 
                    observed = timeseries[m + 1], 
                    ...)
            })
        }, error = function(e) {
            warning(e, "  returning an NA object.")
            return(data.frame(observed = NA,
                              predicted = NA))
        })
}

#' @title Iterated one-step forecasting (no refitting)
#'
#' @description This function faciliates the forecasting approach in Ward et al.
#'   2014. It forecasts the last `num_ahead` points of the time series:
#'   \enumerate{
#'     \item fit the model to all but the last `num_ahead` time points
#'     \item make 1-step ahead forecasts for the `num_ahead` time points
#'   }
#'
#' @param fun a function for doing the forecasting. It should have arguments:
#'   \describe{
#'     \item{training}{the data for training the model}
#'     \item{observed}{the observed values to be forecasted}
#'     \item{...}{any optional arguments}
#'   } and should return a data.frame with at least the observed and predicted
#'   values (with the column names `observed` and `predicted`). Other columns 
#'   are optional, and may be specific to the forecasting method)
#' @param timeseries the time series to forecast
#' @param num_ahead the number of points at the end of the time series to
#'   forecast
#' @param ... arguments to pass to `fun`
#'
#' @return a data.frame of the observed and predicted values, with any other
#'   remaining values returned by `fun`. If any error occurs, then NA values for
#'   the observed and predicted
#'
#' @export
forecast_iterated <- function(fun, timeseries, num_ahead = 5, ...)
{
    tryCatch(
        {
            timeseries <- trim_NA(timeseries)
            
            # check length
            if (length(timeseries) <= 5)
                stop("time series is not long enough")
            
            # make forecasts
            fun(training = utils::head(timeseries, -num_ahead),
                observed = utils::tail(timeseries, num_ahead),
                ...)
            
        }, error = function(e) {
            warning(e, "  returning an NA object.")
            return(data.frame(observed = NA,
                              predicted = NA))
        })
}

trim_NA <- function(timeseries)
{
    # trim NAs off of front and back
    idx <- is.finite(timeseries)
    ts_bounds <- range(which(idx))
    timeseries <- timeseries[seq(ts_bounds[1], ts_bounds[2])]
}

compute_hindcast_method <- function(ts_length, 
                                       hindcast_method = "pred_frac", 
                                       pred_frac = NULL, last_n = NULL, pred_start = NULL)
{
    switch(hindcast_method, 
           pred_frac = {
               if (!is.null(pred_frac) && pred_frac >= 0 && pred_frac < 1)
               {
                   last_n <- floor(pred_frac * ts_length)
               } else {
                   stop("Bad value for `pred_frac`: ", pred_frac)
               }
           }, 
           last_n = {
               if (!is.null(last_n) && last_n >= 1 && last_n < ts_length)
               {
                   last_n <- last_n
               } else {
                   stop("Bad value for `last_n`: ", last_n)
               }
           }, 
           pred_start = {
               if (!is.null(pred_start) && pred_start >= 1 && pred_start <= ts_length)
               {
                   last_n <- ts_length - pred_start + 1
               } else {
                   stop("Bad value for `pred_start`: ", pred_start)
               }
           }, 
           stop("Unexpected value for `hindcast_method`: ", hindcast_method)
    )
}

