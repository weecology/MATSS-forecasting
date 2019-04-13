#' @name locreg_ts
#' @title Make forecasts using locally weighted regression
#'
#' @description `locreg_ts` fits a locally weighted regression. It first
#'   searches over a parameter space of nearest neighbors bandwidth, and
#'   polynomial degree, and then uses the best solution
#'
#' @inheritParams locfit::locfit
#' @inheritParams locfit::lp
#' @inheritParams make_forecasts
#' @inheritParams forecast::forecast
#'
#' @return a data.frame of the mean forecasts, the observed values, and the
#'   lower and upper CI levels (if an error occurs, then just NA values)
#'
#' @export
#'
locreg_ts <- function(timeseries, num_ahead = 5, level = 95,
                      nn = seq(from = 0.2, to = 3, by = 0.05),
                      deg = 1:3)
{
    f <- function(training, observed, level, nn, deg)
    {
        # loop over models
        params <- expand.grid(nn = nn, deg = deg)
        t <- seq_len(length(training))
        locmse <- function(nn, deg)
        {
            locreg <- locfit::locfit(training ~ locfit::lp(t, nn = nn, deg = deg))
            mean(locfit:::residuals.locfit(locreg)^2)
        }
        model_matrix <- params %>%
            dplyr::mutate(mse = purrr::pmap_dbl(., locmse)) %>%
            dplyr::arrange(mse)

        # select best model
        locreg <- locfit::locfit(training ~ locfit::lp(t,
                                                       nn = model_matrix$nn[1],
                                                       deg = model_matrix$deg[1]))

        # predict function requires a new list of predictor variables as newdata
        t_observed <- length(training) + seq_len(length(observed))
        forecasts <- locfit:::predict.locfit(locreg,
                                             newdata = list("t" = t_observed),
                                             se.fit = TRUE)

        # return
        data.frame(observed = as.numeric(observed),
                   predicted = as.numeric(forecasts$fit),
                   lower_CI = as.numeric(qnorm(0.5 - level/200, forecasts$fit, forecasts$se.fit)),
                   upper_CI = as.numeric(qnorm(0.5 + level/200, forecasts$fit, forecasts$se.fit)))
    }

    make_forecasts(fun = f, timeseries = timeseries, num_ahead = num_ahead,
                   level = level, nn = nn, deg = deg)
}
