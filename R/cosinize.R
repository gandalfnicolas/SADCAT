#' @title Cosine Similarity Computation
#'
#' @description Compute cosine similarity between a vector and columns of a matrix
#' @param vector Numeric vector
#' @param dictionaries Matrix where each column is a reference vector
#' @return Named numeric vector of cosine similarities
#' @export cosinize

cosinize <- function(vector, dictionaries) {
  apply(dictionaries, 2, function(x)
    lsa::cosine(as.vector(unlist(x)), as.vector(unlist(vector))))
}
