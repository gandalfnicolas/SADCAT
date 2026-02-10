#' @title Euclidean Distance Computation
#'
#' @description Compute Euclidean distance between a vector and columns of a matrix
#' @param vector Numeric vector
#' @param dictionaries Matrix where each column is a reference vector
#' @return Named numeric vector of Euclidean distances
#' @export euclideanize

euclideanize <- function(vector, dictionaries) {
  apply(dictionaries, 2, function(x)
    stats::dist(rbind(as.vector(unlist(x)), as.vector(unlist(vector)))))
}
