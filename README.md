
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MATSSforecasting

## Overview

MATSSforecasting is a research compendium for investigationg different
approaches to forecasting ecological time series. It implements multiple
methods to forecasting single time series, as well as metrics of time
series complexity, with the goal of synthesizing the results to provide
guidance on forecasting methods.

## Running the code

Much of the underlying support code comprises the `MATSSforecasting`
package, while the analysis code is in the analysis folder. Here is the
suggested workflow for running the analysis.

1.  Download or clone the entire repository.
2.  Build and Install the package locally.
3.  Run `main.R` in the `analysis` folder.

Alternatively, you can install `MATSSforecasting` from github with:

``` r
# install.packages("devtools")
devtools::install_github("ha0ye/MATSS-forecasting")
```

and then obtain just the `analysis` subfolder, and run `main.R`.