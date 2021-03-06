% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecasting-gam.R
\name{gam_ts}
\alias{gam_ts}
\title{Make forecasts using a Generalized Additive Model}
\usage{
gam_ts(timeseries, num_ahead = 5, level = 95)
}
\arguments{
\item{timeseries}{the time series to forecast}

\item{num_ahead}{the number of points at the end of the time series to
forecast}

\item{level}{Confidence level for prediction intervals.}
}
\value{
a data.frame of the mean forecasts, the observed values, and the
lower and upper CI levels (if an error occurs, then just NA values)
}
\description{
Fit a Generalized Additive Model (GAM) using
\code{\link[mgcv]{gam}} with a spline over time/years
}
