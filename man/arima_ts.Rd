% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecasting-arima.R
\name{arima_ts}
\alias{arima_ts}
\alias{arfima_ts,}
\alias{autoarima_one_step}
\alias{arfima_ts}
\alias{arfima_one_step}
\title{Make forecasts using AR / ARIMA}
\usage{
arima_ts(timeseries, num_ahead = 5, level = 95, order = c(1, 0, 0))

arfima_ts(timeseries, num_ahead = 5, level = 95)

autoarima_one_step(timeseries, level = 95, ...)

arfima_one_step(timeseries, level = 95, ...)
}
\arguments{
\item{timeseries}{the time series to forecast}

\item{num_ahead}{the number of points at the end of the time series to
forecast}

\item{level}{Confidence level for prediction intervals.}

\item{order}{A specification of the non-seasonal part of the ARIMA
    model: the three integer components \eqn{(p, d, q)} are the AR order, the
    degree of differencing, and the MA order.}

\item{...}{arguments to pass to \code{fun}}
}
\value{
a data.frame of the mean forecasts, the observed values, and the
lower and upper CI levels (if an error occurs, then just NA values)
}
\description{
\code{arima_ts} fits an arima model using \code{\link{arima}}

\code{arfima_ts} fits a step-wise fractionally differenced
auto-arima using \code{\link[forecast]{arfima}}

\code{autoarima_one_step} uses \code{\link[forecast]{auto.arima}} to
fit an ARIMA model and make a single one-step forecast.

\code{arfima_one_step} uses \code{\link[forecast]{arfima}} to
fit an fractionally-differentiated ARIMA model and make a single one-step
forecast.
}
