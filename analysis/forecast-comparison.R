####------------------------------------------------------------------------####
# July 2019
#   This is the central processing script for performing the analyses for a 
# forecasting comparison, as described in 
# https://github.com/weecology/MATSS-forecasting/issues/16
#
# Perform forecasts using the following procedure:
# 1. start forecasting for the last 1/3 of the time series
#    - train a 1-step ahead model on the first 2/3, make a forecast, note actual
#      observation
#    - re-train the model, including the new observation, make another 1-step 
#      ahead forecast
# 2. Repeat the procedure for a variety of models:
#    - linear autoregressive models (autoarima and arfima)
#    - nonlinear time-delay embedding (simplex, s-map, and GP)
#    - random walk / state-space (MARSS)
#    - linear / quadratic / splined functions of time
# 3. Compute time series properties:
#    - complexity (weighted permutation entropy)
#    - time series length and sampling frequency
#    - life history characteristics
# 4. Analysis?
####------------------------------------------------------------------------####

library(MATSSforecasting)
library(MATSS)
library(tidyverse)
library(drake)

# libraries for metadata
library(taxize)
library(traits)
library(rfishbase)

## make sure the package functions in MATSS and MATSSforecasting are loaded in 
##   as dependencies
expose_imports(MATSS)
expose_imports(MATSSforecasting)
options(drake_make_menu = FALSE) # disable check for `make` vs `r_make`

# file paths
raw_data_file <- system.file("extdata", "processed_data", "masterDat_2019-06-12.csv",
                             package = "MATSSforecasting", mustWork = TRUE)
processed_data_file <- here::here("analysis", "data", "ward_fish_data.RDS")

## preprocessing of the datasets
reshape_ward_data(data_file = raw_data_file, 
                  ward_RDS_file = processed_data_file)

## define the datasets
datasets <- bind_rows(
     drake_plan(
         data_LPI = get_LPI_data()
     ), 
    # build_datasets_plan(include_retriever_data = TRUE), 
    build_ward_data_plan(ward_RDS_file = processed_data_file)
)

## define the forecasting methods
methods <- build_methods_plan()

## define the analyses (each method x dataset combination)
analyses <- drake::drake_plan(
    analysis = target(fun(data),
                      transform = cross(fun = !!rlang::syms(methods$target[c(3,10)]),
                                        data = !!rlang::syms(datasets$target))
    ),
    results = target(bind_rows(analysis),
                     transform = combine(analysis, .by = fun)),
    full_results = target(
        bind_rows(results), 
        transform = combine(results)
    )
)

## define a report that summarize the autoarima analysis
reports <- drake_plan(
    forecasting_comparison = rmarkdown::render(
        knitr_in("reports/forecasting_comparison.Rmd")
    )
)

metadata <- drake_plan(

taxonomy = get_taxonomy(),
fish_metadata = pull_fishbase(taxonomy),
mammal_bird_herp_metadata = pull_traits(taxonomy)    
) 

## create a master plan
pipeline <- bind_rows(datasets, methods, analyses, metadata, reports)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("output", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)


## View the graph of the plan
if (interactive())
{
    config <- drake_config(pipeline, cache = cache)
    sankey_drake_graph(config, build_times = "none")  # requires "networkD3" package
    vis_drake_graph(config, build_times = "none", targets_only = TRUE)     # requires "visNetwork" package
}

## Run the pipeline
make(pipeline, cache = cache)

DBI::dbDisconnect(db)

## Run the pipeline (parallelized)
# future::plan(future::multiprocess)
# make(pipeline, 
#      force = TRUE, 
#      cache = cache,
#      verbose = 2,
#      parallelism = "future",
#      jobs = 2,
#      caching = "master") # Important for DBI caches!