#' Simulate data from one dimensional or multidimensional DGP
#' @param nobs positive integer, number of observations
#' @param tlength positive integer, number of items
#' @param DIFpercent non-negative double <= 1, DIF percentage
#' @param type string specifying which DGP to use ("single" or "multi")
#' @param ... additional arguments:
#' - For single-dimensional DGP, check ?anchorpoint::dgp_single
#' - For multi-dimensional DGP, check ?anchorpoint::dgp_multi
#' @return a list containing
#' - DGP: simulated data
#' - RM: Rasch Model objects
#' @export
getData <- function(nobs, tlength, DIFpercent, type = c("single","multi"),...){

  nobs <- as.integer(nobs)
  tlength <- as.integer(tlength)
  DIFpercent <- as.numeric(DIFpercent)
  stopifnot("nobs must be a positive integer greater than 1"=nobs>1)
  stopifnot("tlength must be a positive integer greater than 1"=tlength>1)
  stopifnot("DIFpercent must be numeric and in the interval [0,1]"=!DIFpercent<0 && !DIFpercent>1 )

  type <- match.arg(type)
  DGP <- switch(type,
               #multi dimensional
               multi={
                 dgp_multi(nobs = nobs, tlength = tlength, DIFpercent = DIFpercent,...)
               },
                # single dimensional
               single={
                 dgp_single(nobs = nobs, tlength = tlength, DIFpercent = DIFpercent,...)
               },
               {
                 #statements actually can not be reach, as match.arg would already fail..
                 stop("DGP type not valid.")
               }

  )
  return(list("DGP" = DGP,"RM" = raschFit(DGP$dat)))
}


