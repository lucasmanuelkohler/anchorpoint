#' Function which executes Wald test for given rm object and shift (with "min_dist" setting)
#' @param rm A list containing the two Rasch Model objects of group 0 and group 1
#' @param shift Shift in item parameters for the second group
#' @return A list containing the output of the function Wald_test():
#' - p: results from the test (p-values)
#' - vcov: the covariance matrices of the fit (from diftests function)
getWald <- function(rm,shift){
  out <- Wald_test(rm$rm1, rm$rm2, c_shift = shift, alias_method = "min_dist",  adjust = "none")
  class(out) <- "waldtest"
  return(out)
}
