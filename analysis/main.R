library(MATSSforecasting)
library(MATSS)
library(tidyverse)
library(drake)

## make sure the package functions in MATSS and MATSSforecasting are loaded in 
##   as dependencies
expose_imports(MATSS)
expose_imports(MATSSforecasting)

## define the datasets
datasets <- plan_ward_data()

## arima methods
arima_methods <- drake_plan(
    arima_001 = analysis_wrapper(arima_ts, order = c(0, 0, 1)), 
    arima_002 = analysis_wrapper(arima_ts, order = c(0, 0, 2)), 
    arima_011 = analysis_wrapper(arima_ts, order = c(0, 1, 1)), 
    arima_012 = analysis_wrapper(arima_ts, order = c(0, 1, 2)), 
    arima_100 = analysis_wrapper(arima_ts, order = c(1, 0, 0)), 
    arima_101 = analysis_wrapper(arima_ts, order = c(1, 0, 1)), 
    arima_102 = analysis_wrapper(arima_ts, order = c(1, 0, 2)), 
    arima_110 = analysis_wrapper(arima_ts, order = c(1, 1, 0)), 
    arima_111 = analysis_wrapper(arima_ts, order = c(1, 1, 1)), 
    arima_112 = analysis_wrapper(arima_ts, order = c(1, 1, 2)),     
    arima_200 = analysis_wrapper(arima_ts, order = c(2, 0, 0)), 
    arima_201 = analysis_wrapper(arima_ts, order = c(2, 0, 1)), 
    arima_202 = analysis_wrapper(arima_ts, order = c(2, 0, 2)), 
    arima_210 = analysis_wrapper(arima_ts, order = c(2, 1, 0)), 
    arima_211 = analysis_wrapper(arima_ts, order = c(2, 1, 1)), 
    arima_212 = analysis_wrapper(arima_ts, order = c(2, 1, 2)), 
    randomwalk = analysis_wrapper(randomwalk_ts), 
    randomwalk_drift = analysis_wrapper(randomwalk_ts, drift = TRUE), 
    autoarima = analysis_wrapper(arima_fracdiff_ts)
)

## ets methods
ets_methods <- drake_plan(
    ets_1 = analysis_wrapper(ets_ts, frequency = 1), 
    ets_2 = analysis_wrapper(ets_ts, frequency = 2), 
    ets_3 = analysis_wrapper(ets_ts, frequency = 3), 
    ets_4 = analysis_wrapper(ets_ts, frequency = 4)
)

## sts methods (I am not sure why Ward et al. only explore up to frequency = 2 
##   here, but up to frequency = 4 for ets)
sts_methods <- drake_plan(
    sts_1 = analysis_wrapper(sts_ts, frequency = 1), 
    sts_2 = analysis_wrapper(sts_ts, frequency = 2)
)

## spline methods
spline_methods <- drake_plan(
    gam = analysis_wrapper(gam_ts)
)

## full list of methods
methods <- bind_rows(arima_methods, ets_methods, sts_methods, spline_methods)

## define the analyses (each method x dataset combination)
analyses <- build_analyses_plan(methods, datasets)

## define a report that summarize the autoarima analysis
reports <- drake_plan(
    autoarima_report = rmarkdown::render(
        knitr_in("reports/autoarima_report.Rmd")
    )
)

## create a master plan
pipeline <- bind_rows(datasets, methods, analyses, reports)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("output", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

## View the graph of the plan
if (interactive())
{
    config <- drake_config(pipeline, cache = cache)
    sankey_drake_graph(config, build_times = "none")  # requires "networkD3" package
    vis_drake_graph(config, build_times = "none")     # requires "visNetwork" package
}

## Run the pipeline
make(pipeline, cache = cache)

## Run the pipeline (parallelized)
# future::plan(future::multiprocess)
# make(pipeline, 
#      force = TRUE, 
#      cache = cache,
#      verbose = 2,
#      parallelism = "future",
#      jobs = 2,
#      caching = "master") # Important for DBI caches!