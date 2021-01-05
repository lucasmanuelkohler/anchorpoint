#' calculates the CLF criterion
#' @param dist distance vector
#' @param eps shift for numerical stability (see Muthen paper)
#' @return criterion value
#' @export
clfCriterion <- function(dist,eps = 0.0001){
  sum(sqrt(sqrt(dist^2 + eps)))
}
