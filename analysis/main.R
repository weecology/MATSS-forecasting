# library(MARSS)
# library(nlts)
# library(kernlab)
# library(ltsa)
# library(timsac)
# library(randomForest)


library(MATSSforecasting)
library(MATSS)
library(tidyverse)
library(drake)

## make sure the package functions in MATSS and MATSSforecasting are loaded in 
##   as dependencies
expose_imports(MATSS)
expose_imports(MATSSforecasting)

## define the datasets
datasets <- build_ward_data_plan()

## define the forecasting methods
methods <- build_ward_methods_plan()

## define the analyses (each method x dataset combination)
analyses <- build_analyses_plan(methods, datasets)

## define a report that summarize the autoarima analysis
reports <- drake_plan(
    autoarima_report = rmarkdown::render(
        knitr_in("reports/autoarima_report.Rmd")
    ),
    forecasting_comparison = rmarkdown::render(
        knitr_in("reports/forecasting_comparison.Rmd")
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