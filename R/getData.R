#' @title Simulate data from unidimensional or multidimensional DGP
#' @description
#' Simulate data from unidimensional or multidimensional Rasch model and two groups of test takers.
#' @param nobs positive integer, number of observations
#' @param tlength interger > 0, test length (number of items)
#' @param DIFpercent percentage of DIF items in the test
#' @param type string specifying which DGP to use ("uni" or "multi")
#' @param ... additional arguments:
#' - For unidimensional DGP, check ?anchorpoint:::dgp_uni
#' - For multidimensional DGP, check ?anchorpoint:::dgp_multi
#' @return a list containing:
#' - DGP: simulated data
#' - RM: Rasch Model objects for the two groups of test takers
#' @export
#' @examples
#' # The number of observations
#' nobs = 20
#' # The number of items
#' tlength = 10
#' # The percentage of items to experience differential item functioning
#' DIFpercent = 0
#'
#' # Create data from a unidimensional DGP:
#' getData(nobs, tlength, DIFpercent, type = "uni")
getData <- function(nobs, tlength, DIFpercent, type = c("uni","multi"),...){

  nobs <- as.integer(nobs)
  tlength <- as.integer(tlength)
  DIFpercent <- as.numeric(DIFpercent)
  stopifnot("nobs must be a positive integer greater than 1"=nobs>1)
  stopifnot("tlength must be a positive integer greater than 1"=tlength>1)
  stopifnot("DIFpercent must be numeric and in the interval [0,1]"=!DIFpercent<0 && !DIFpercent>1 )

  type <- match.arg(type)
  DGP <- switch(type,
               #multidimensional
               multi={
                 dgp_multi(nobs = nobs, tlength = tlength, DIFpercent = DIFpercent,...)
               },
                #unidimensional
               uni={
                 dgp_uni(nobs = nobs, tlength = tlength, DIFpercent = DIFpercent,...)
               },
               {
                 #statements actually can not be reach, as match.arg would already fail..
                 stop("DGP type not valid.")
               }

  )
  return(list("DGP" = DGP,"RM" = raschFit(DGP$dat)))
}


