#' Calculates the CLF criterion employed by Asparouhov and Muthén
#' @param dist distance vector
#' @param eps shift for numerical stability
#' @return criterion value
#' @references
#' - Asparouhov, T. & Muthén, B. (2014). Multiple-group factor analysis alignment. Structural Equation Modeling: A Multidisciplinary Journal, 21:4, 495-508
clfCriterion <- function(dist,eps = 0.0001){
  sum(sqrt(sqrt(dist^2 + eps)))
}
