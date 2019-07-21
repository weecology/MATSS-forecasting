#' @title Compute the indices corresponding to a defined subset
#' 
#' @param bounds the vector with lower and upper bound, expressed as numeric
#'   fractions of the time series in \[0, 1\]
#' @param n the totla length of the vector to subset
#' 
#' @return the indices corresponding to the subset defined by `bounds` on a 
#'   vector of length n
#' 
#' @examples
#' compute_subset_range(c(0, 0.5), 10) # 1:5
#' compute_subset_range(c(0.5, 1), 10) # 6:10
#' 
#' @export
#' 
compute_subset_range <- function(bounds, n)
{
    if (!is.numeric(bounds) || length(bounds) < 2 || 
        min(bounds) < 0 || max(bounds) > 1 || bounds[1] >= bounds[2])
    {
        stop("`bounds` was not a recognized format.")
    }
    
    min_index <- min(floor(bounds[1] * n) + 1, n)
    max_index <- max(floor(bounds[2] * n), 1)

    return(seq(min_index, max_index))
}




