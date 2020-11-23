WPE calculations check
================
Hao Ye
10/3/2019

# Setup

## ordinal\_TSA installation

1.  clone from <https://github.com/garlanjt/Ordinal_TSA>
2.  in command line, navigate to the folder, and then:

<!-- end list -->

    pip install .

3.  test the code

<!-- end list -->

    python min_working_example.py

## R code

# Analysis

## Generate data

``` r
set.seed(42)
nile <- as.numeric(Nile)
ss_m <- as.numeric(sunspot.month)
ss_y <- as.numeric(sunspot.year)
test <- c(1, 1, 0, 0, 1, 1, rnorm(100))
```

## Compute WPE

Josh’s cython implementation:

``` python
import ordinal_TSA
import numpy as np

ts = np.vstack(np.asarray(r.nile))
ordinal_TSA.permutation_entropy(ts, dim = 3, step = 1, w = 1)
```

    ## 0.9853334990868959

``` python
ts = np.vstack(np.asarray(r.ss_m))
ordinal_TSA.permutation_entropy(ts, dim = 3, step = 1, w = 1)
```

    ## 0.9746485154549505

``` python
ts = np.vstack(np.asarray(r.ss_y))
ordinal_TSA.permutation_entropy(ts, dim = 3, step = 1, w = 1)
```

    ## 0.5718737881018081

Frank’s R implementation:

``` r
library(MATSSforecasting)

PE(nile, weighted = TRUE, tie_method = "first", word_length = 3, tau = 1)
```

    ## [1] 0.9853335

``` r
PE(ss_m, weighted = TRUE, tie_method = "first", word_length = 3, tau = 1)
```

    ## [1] 0.9746485

``` r
PE(ss_y, weighted = TRUE, tie_method = "first", word_length = 3, tau = 1)
```

    ## [1] 0.5718738

## Tie-handling

Check how ties should be handled for consistency

``` python
import ordinal_TSA
import numpy as np

ts = np.vstack(np.asarray(r.test))
ordinal_TSA.permutation_entropy(ts, dim = 3, step = 1, w = 1)
```

    ## 0.9972149151169224

``` r
library(MATSSforecasting)

PE(test, weighted = TRUE, tie_method = "first", word_length = 3, tau = 1)
```

    ## [1] 0.9972149

``` r
PE(test, weighted = TRUE, tie_method = "last", word_length = 3, tau = 1)
```

    ## [1] 0.9974512

``` r
PE(test, weighted = TRUE, tie_method = "average", word_length = 3, tau = 1)
```

    ## [1] 0.7453637

``` r
PE(test, weighted = TRUE, tie_method = "random", word_length = 3, tau = 1)
```

    ## [1] 0.9972529

``` r
PE(test, weighted = TRUE, tie_method = "noise", word_length = 3, tau = 1, noise_amount = 5)
```

    ## [1] 0.7193993
