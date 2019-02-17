#' @importFrom utils data read.csv
#' 
#' ## quiets concerns of R CMD check re: variables used in NSE functions
if (getRversion() >= "2.15.1") utils::globalVariables(
    c("database", "dataset", "id", "map", "read.csv", "species", 
      "target", "value", "ward_database", "year"))