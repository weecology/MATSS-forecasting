#' @title Check time series for potentially problematic features
#' @description Some of the analysis methods have issues handling NA values, duplicated 
#' values or consecutive segments of identical values. This function spits out 
#' some warnings on those.
#'
#' @param x A time series of real numbers, represented as a numeric or integer 
#'   vector.
#' @return nothing
#' 
#' @examples
#' check_time_series(Nile)
#' 
#' @export
check_time_series <- function(x)
{
    n <- length(x)
    message("Many analysis methods assume equally spaced time series", 
            ", please be careful!")
    if (any(is.na(x)))
        warning("Your time series contains some NAs. Be careful, as some analyses ", 
                "/ functions deal well with NAs, and some don't.")
    if (any(duplicated(x)))
        warning("Your time series contain identical values, if there are many, ", 
                "and if they appear consecutively, this could cause problems.")
    if ( sum( (x[1:(n-1)] - x[2:n] ) == 0) > 0)
        warning("Your time series has some identical consecutive values; ", 
                "this could cause problems.")
    ## check for long series of zeros (same values?)
    invisible(NULL)
}

#' @title Embed a time series
#' @description Construct a time-delay embedding of an input time series, 
#'   returning a matrix
#'
#' @param x the time series, observed at regular intervals.
#' @param m the number of dimensions to embed x into.
#' @param d the time delay
#' @param indices logical; whether to return the raw values or the time series 
#'   indices
#' @param as.embed logical; should we return the embedded time series in the 
#'   order that embed() would?
#' @return matrix of the embedded time series
#' 
#' @export
embedd <- function(x, m, d = 1, indices = FALSE, as.embed = FALSE)
{
    n <- length(x) - (m - 1)*d
    X <- seq_along(x)
    if (n <= 0)
        stop("Insufficient observations for the requested embedding")
    out <- matrix(rep(X[seq_len(n)], m), ncol = m)
    out[,-1] <- out[,-1, drop = FALSE] + rep(seq_len(m - 1) * d, each = nrow(out))
    if (as.embed)
        out <- out[, rev(seq_len(ncol(out)))]
    if (!indices)
        out <- matrix(x[out], ncol = m)
    out
}

#' #' Weighted distribution of "word"s in a time series.
#' #' 
#' #' @param x A time series of real numbers.
#' #' @param word_length The word length, also known as the permutation order.
#' #' @param tau Time lag (not implemented).
#' #' @param tie_method The method for dealing with tied values; these are the methods used by the rank()
#' #' function of the base package. Use "average" to treat ties as the same value,
#' #' "first" to treat ties as different with first value being treated as larger than second,
#' #' "last" to treat ties as different with last value being treated as larger than second,
#' #' "random" to give ties random rank,
#' #' "noise" to add some noise to the time series to break ties.
#' #' @param noise_amount How much noise to add to the time series; only used for method "noise".
#' #' Random numbers from a uniform distribution with maximum 1 * 10^(-noise_amount-1) and minimum zero.
#' #'  @return The word distribution.
#' 

#' @title Compute the distribution of "word"s in a time series.
#' @description `weighted_word_distribution` is a wrapper that sets `weighted` 
#'   to `TRUE`
#' @aliases weighted_word_distribution
#'
#' @param x_emb A time series of real numbers.
#' @param weighted whether to weight by the variance in each word
#' @inheritParams base::rank
#' @return a table of the word distribution
#' 
#' @export
word_distribution <- function(x_emb, ties.method = "first", weighted = TRUE)
{
    words <- apply(x_emb, 1, 
                   function(x) paste(rev(rank(x, ties.method = ties.method)), collapse = "-"))
    if (weighted)
    {
        weights <- apply(x_emb, 1, function(x) var(x))
        return(aggregate(weights, list(words), sum)$x / sum(weights))
    } else {
        return(as.numeric(table(words)/nrow(x_emb)))
    }
}

#' @rdname word_distribution
weighted_word_distribution <- function(x_emb, ties.method)
{
    word_distribution(x_emb = x_emb, ties.method = ties.method, weighted = TRUE)
}

#' @title Calculate the permuation entropy of a times series
#' @aliases WPE
#' @description `WPE` is a wrapper that sets `weighted` to `TRUE`
#' @details Words containing NAs are ignored.
#'
#' @inheritParams embedd
#' @inheritParams word_distribution
#' @return The entropy of the distribution.
#' 
#' @examples
#' x <- rnorm(1000)
#' PE(x, 3)
#' @export
PE <- function(x, m, d = 1, ties.method = "first", weighted = TRUE)
{
    ## check if the time series contains low variance
    if (var(x, na.rm = TRUE) <= 2 * .Machine$double.eps)
    {
        warning("Time series variance is too small, so setting entropy to 0.")
        return(0)
    }
    
    x_emb <- embedd(x = x, m = m, d = d)
    x_emb <- na.omit(x_emb)
    wd <- word_distribution(x_emb, ties.method = ties.method, weight = weighted)
    
    if (ties.method == "average")
    {
        # with tied ranks allowed, the number of possible words increases
        denom <- log2(2 * factorial(m))
    } else {
        denom <- log2(factorial(m))
    }
    
    # Calculate the entropy (in bits) for a vector of word frequencies
    entropy <- function(wd) {  -sum(wd * log2(wd))  }
    entropy(wd) / denom
}

#' @rdname PE
WPE <- function(x, m, d, ties.method = "first")
{
    PE(x, m, d, ties.method, weighted = TRUE)
}