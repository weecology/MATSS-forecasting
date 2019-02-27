library(MATSSforecasting)
library(tidyverse)
library(drake)

## define the datasets
datasets <- plan_ward_data()

## define the forecasting methods
methods <- drake_plan(
    autoarima = function(dataset) {forecast_wrapper(dataset, autoarima_ts)}
)

## define the analyses (each method x dataset combination)
analyses <- MATSS::build_analyses_plan(methods, datasets)

## define a report that summarize the autoarima analysis
reports <- drake_plan(
    autoarima_report = rmarkdown::render(
        knitr_in("analysis/autoarima_report.Rmd")
    )
)

## The entire pipeline
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