#' @title Make a drake plan with all the forecasting mehtods in Ward et al. 2014
#'
#' @description We list out the different forecasting methods (and the parameter 
#'   settings we wish to try) in the form of a drake plan. Note the use of 
#'   \code{link[MATSS]{analysis_wrapper}} that takes a function that operates on
#'   a single time series, and wraps it to run on each time series within a 
#'   dataset.
#'
#' @return a drake plan (i.e. a tibble) with the forecasting methods.
#'
#' @export
#'
build_ward_methods_plan <- function()
{
    ## arima methods
    arima_methods <- drake::drake_plan(
        arima_001 = MATSS::analysis_wrapper(arima_ts, order = c(0, 0, 1)), 
        arima_002 = MATSS::analysis_wrapper(arima_ts, order = c(0, 0, 2)), 
        arima_011 = MATSS::analysis_wrapper(arima_ts, order = c(0, 1, 1)), 
        arima_012 = MATSS::analysis_wrapper(arima_ts, order = c(0, 1, 2)), 
        arima_100 = MATSS::analysis_wrapper(arima_ts, order = c(1, 0, 0)), 
        arima_101 = MATSS::analysis_wrapper(arima_ts, order = c(1, 0, 1)), 
        arima_102 = MATSS::analysis_wrapper(arima_ts, order = c(1, 0, 2)), 
        arima_110 = MATSS::analysis_wrapper(arima_ts, order = c(1, 1, 0)), 
        arima_111 = MATSS::analysis_wrapper(arima_ts, order = c(1, 1, 1)), 
        arima_112 = MATSS::analysis_wrapper(arima_ts, order = c(1, 1, 2)),     
        arima_200 = MATSS::analysis_wrapper(arima_ts, order = c(2, 0, 0)), 
        arima_201 = MATSS::analysis_wrapper(arima_ts, order = c(2, 0, 1)), 
        arima_202 = MATSS::analysis_wrapper(arima_ts, order = c(2, 0, 2)), 
        arima_210 = MATSS::analysis_wrapper(arima_ts, order = c(2, 1, 0)), 
        arima_211 = MATSS::analysis_wrapper(arima_ts, order = c(2, 1, 1)), 
        arima_212 = MATSS::analysis_wrapper(arima_ts, order = c(2, 1, 2)), 
        randomwalk = MATSS::analysis_wrapper(randomwalk_ts), 
        randomwalk_drift = MATSS::analysis_wrapper(randomwalk_ts, drift = TRUE), 
        autoarima = MATSS::analysis_wrapper(arima_fracdiff_ts)
    )
    
    ## ets methods (I think the frequency parameter doesn't actually do anything?)
    ets_methods <- drake::drake_plan(
        ets_1 = MATSS::analysis_wrapper(ets_ts, frequency = 1), 
        ets_2 = MATSS::analysis_wrapper(ets_ts, frequency = 2), 
        ets_3 = MATSS::analysis_wrapper(ets_ts, frequency = 3), 
        ets_4 = MATSS::analysis_wrapper(ets_ts, frequency = 4)
    )
    
    ## sts methods (I am not sure why Ward et al. only explore up to frequency = 2 
    ##   here, but up to frequency = 4 for ets; this appears to give different 
    ##   results for different values of the frequency parameter)
    sts_methods <- drake::drake_plan(
        sts_1 = MATSS::analysis_wrapper(sts_ts, frequency = 1), 
        sts_2 = MATSS::analysis_wrapper(sts_ts, frequency = 2)
    )
    
    ## spline methods
    spline_methods <- drake::drake_plan(
        gam = MATSS::analysis_wrapper(gam_ts)
    )
    
    ## neural network methods
    nnet_methods <- drake::drake_plan(
        nnet_1_1 = MATSS::analysis_wrapper(nnet_ts, m = 1, size = 1), 
        nnet_1_2 = MATSS::analysis_wrapper(nnet_ts, m = 1, size = 2), 
        nnet_2_1 = MATSS::analysis_wrapper(nnet_ts, m = 2, size = 1), 
        nnet_2_2 = MATSS::analysis_wrapper(nnet_ts, m = 2, size = 2), 
        nnet_3_1 = MATSS::analysis_wrapper(nnet_ts, m = 3, size = 1), 
        nnet_3_2 = MATSS::analysis_wrapper(nnet_ts, m = 3, size = 2)
    )  
    
    ## locally weighted regression methods
    locreg_methods <- drake::drake_plan(
        locreg = MATSS::analysis_wrapper(locreg_ts)
    )
    
    ## non-parametric regression methods
    npreg_methods <- drake::drake_plan(
        npreg = MATSS::analysis_wrapper(npreg_ts)
    )
    
    ## gaussian process regression methods
    gausspr_methods <- drake::drake_plan(
        gausspr_1 = MATSS::analysis_wrapper(gausspr_ts, frequency = 1), 
        gausspr_2 = MATSS::analysis_wrapper(gausspr_ts, frequency = 2), 
        gausspr_3 = MATSS::analysis_wrapper(gausspr_ts, frequency = 3), 
        gausspr_4 = MATSS::analysis_wrapper(gausspr_ts, frequency = 4)
    )
    
    ## random forest methods
    ranfor_methods <- drake::drake_plan(
        ranfor = MATSS::analysis_wrapper(ranfor_ts)
    )
    
    ## linear regression methods
    lm_methods <- drake::drake_plan(
        linreg <- MATSS::analysis_wrapper(lm_ts)
    )
    
    ## full list of methods
    methods <- dplyr::bind_rows(arima_methods, ets_methods, sts_methods, 
                         spline_methods, nnet_methods, locreg_methods, 
                         npreg_methods, gausspr_methods, ranfor_methods, 
                         lm_methods)
}

#' @title Make a drake plan with all the datasets in Ward et al. 2014
#'
#' @inheritParams get_ward_data
#'
#' @return a drake plan (i.e. a tibble) with the datasets
#'
#' @export
#'
build_ward_data_plan <- function(ward_RDS_file = here::here("analysis", "data", "ward_fish_data.RDS"))
{
    # if ward RDS database doesn't exist, create it
    if (!file.exists(ward_RDS_file))
    {
        ward_database_names <- reshape_ward_data(ward_RDS_file)
    } else {
        ward_database_names <- readRDS(ward_RDS_file) %>%
            dplyr::pull(database)
    }
    
    drake::drake_plan(
        data = target(
            get_ward_data(ward_database, drake::file_in(!!ward_RDS_file)),
            transform = map(ward_database = !!ward_database_names)
        )
    )
}