library(MATSSforecasting)
library(MATSS)
library(tidyverse)
library(drake)

## make sure the package functions in MATSS and MATSSforecasting are loaded in 
##   as dependencies
expose_imports(MATSS)
expose_imports(MATSSforecasting)

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
    build_datasets_plan(include_retriever_data = TRUE), 
    build_ward_data_plan(ward_RDS_file = processed_data_file)
)

## define the forecasting methods
methods <- build_ward_methods_plan()

## define the analyses (each method x dataset combination)
analyses <- drake::drake_plan(
    analysis = target(fun(data),
                      transform = cross(fun = !!rlang::syms(methods$target),
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
    autoarima_report = rmarkdown::render(
        knitr_in("reports/autoarima_report.Rmd")
    ),
    forecasting_comparison = rmarkdown::render(
        knitr_in("reports/ward_2014_repro.Rmd")
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