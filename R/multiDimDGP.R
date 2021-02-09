#' Function that gives back a covariance matrix for n dimesnions
#' @param Nr.dim     integer - the number of dimensions
#' @param variances  numeric, positive, <= 1, (same for all dimensions) or Nr.dim-dimensional vector - variance of each dimension
#' @param covariances  numeric, positive, <= 1, (same for all dimensions) or choose(Nr.dim,2)-dimensional vector - covariances between dimensions
#' @return covariance matrix of dimension Nr.dim x Nr.dim
get_covmat <- function(Nr.dim,variances = 0.25,covariances = 0.125){
  lenCov <- length(covariances)
  if(Nr.dim > 0 && length(variances)!=0 && lenCov!=0){

    if(lenCov==choose(Nr.dim,2)){
      Cov_mat = matrix(0,nrow = Nr.dim,ncol = Nr.dim)
      L = l = 2
      k = 1
      for(i in seq(lenCov)){
        if(l > Nr.dim){
          L = L + 1
          l = L
          k = k + 1
        }
        Cov_mat[k,l] = covariances[i]
        l = l+1
      }
      Cov_mat  = Cov_mat + t(Cov_mat)
      diag(Cov_mat) <- variances
      return(Cov_mat)
    }else{
      return(ifelse(col(diag(Nr.dim))!=row(diag(Nr.dim)),covariances[1],variances))
    }
  }
  else{
    print("dimension must be positive!")
    return(NULL)
  }
}

#'@title Data generating process for multidimensional Rasch models
#'@description Data generating process for multidimensional Rasch models (only two dimensions are supported at the moment)
#'@param nobs              positive integer, number of total observations (default 1000) or positive integer vector vector of length 2, number of observations per group
#'@param tlength           interger > 0, test length (number of items)
#'@param DIFpercent        percentage of DIF items in the test
#'@param Nr.dim            positive integer, number of dimensions (default 2)
#'@param Theta             matrix of the underlying ability parameters (optional)
#'@param discriminations   binary matrix of size tlength x Nr.dim, item discrimination parameter matrix
#'@param difficulties      numeric vector of length tlength, item difficulty vector
#'@param DIF_mode          string, mode how DIF items are created, default: intersect.
#'@param d_distr           d_distr: parameters for normal distribution to generate item difficulties difficulties, default: mean = 0, sd = .2
#'@param MultiNorm         list with parameters for multivariate normal distribution to generate the abilities of the test takers in group 1 and group 2, respectively.
#'@param itemtype          type of items (default "dich" which corresponds to multidimensional Rasch model items)
#'@return list consisting of:
#' - dat: binary response matrix
#' - groups: group vector (factor),
#' - discriminations: binary matrix containing the item discrimination parameter matrix,
#' - difficulties: item difficulty vector,
#' - DIFindex: indicating which items were generated with DIF,
#' - Theta,
#' - DIFside: which group is favored, default focal group is favored
#' @export
#' @examples
#' # For examples, see ?getData.
#' @references
#' Credit: Data is generated using the function simdata from \pkg{mirt} (Version: 1.32.1):
#' - R. Philip Chalmers (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. Journal of Statistical Software, 48(6), 1-29.
dgp_multi <- function(nobs,tlength,DIFpercent,Nr.dim = 2,Theta = NULL,discriminations = NULL ,difficulties = NULL,DIF_mode = "intersect",
                      d_distr = list(mean = 0, sd = .2),MultiNorm = NULL,itemtype = 'dich'){

  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("mvtnorm", quietly = TRUE)) {
    stop("Package \"mvtnorm\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop("Package \"mirt\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(!is.null(MultiNorm)) Nr.dim <-  length(MultiNorm[[1]][[1]])
  #some validity checks
  stopifnot(tlength > 0)
  stopifnot(all(nobs > 0))
  stopifnot(all(DIFpercent >= 0))
  stopifnot(Nr.dim > 0)
  stopifnot(all(!is.na(discriminations)))
  stopifnot(all(!is.na(difficulties)))
  stopifnot(DIF_mode %in% c("intersect","disjoint"))
  stopifnot(names(d_distr) %in% c("mean","sd") && (is.numeric(d_distr$mean) & is.numeric(d_distr$sd) & d_distr$sd>0))

  if(length(nobs)==1) nobs = c(nobs/2,nobs/2)
  if(is.null(MultiNorm)) MultiNorm <- list(list(means  = c(-0.5,rep(-0.5,Nr.dim-1)),cov = c(1,0.5)), list(means = c(0.5,rep(0.5,Nr.dim-1)),cov = c(1,0.5)))

  #Check if dimensionalityof MultiNorm is correct
  stopifnot(all(c(c(Nr.dim,Nr.dim) == dim(MultiNorm[[1]][[2]])||length(MultiNorm[[1]][[2]])==2,
                  c(Nr.dim,Nr.dim) == dim(MultiNorm[[2]][[2]])|| length(MultiNorm[[2]][[2]])==2)))

  # Compute discriminations if not hardcoded
  if(is.null(discriminations)){
    discriminations <- getItemDiscrimination(dimensions = Nr.dim, DIFpercent = DIFpercent, tlength = tlength,DIF_mode = DIF_mode)
  }

  # Compute difficulties if not hardcoded
  if(is.null(difficulties)){
    difficulties = stats::rnorm(mean = d_distr$mean,sd = d_distr$sd,n = tlength)
  }

  if(is.null(Theta)){
    # Compute Theta if not hardcoded
    if(is.matrix(MultiNorm[[1]][[2]])){
      sigma1 = MultiNorm[[1]][[2]]
    }else{
      sigma1 = get_covmat(Nr.dim = Nr.dim,variances = MultiNorm[[1]][[2]][1],covariances = MultiNorm[[1]][[2]][2])
    }
    if(is.matrix(MultiNorm[[2]][[2]])){
      sigma2 = MultiNorm[[2]][[2]]
    }else{
      sigma2 = get_covmat(Nr.dim = Nr.dim,variances = MultiNorm[[2]][[2]][1],covariances = MultiNorm[[2]][[2]][2])
    }
    Theta1 <- mvtnorm::rmvnorm(nobs[1], mean = MultiNorm[[1]][[1]],sigma = sigma1)
    Theta2 <- mvtnorm::rmvnorm(nobs[2], mean = MultiNorm[[2]][[1]],sigma = sigma2)
    Theta <- rbind(Theta1,Theta2)
  }

  # Compute Data
  data = mirt::simdata(a = discriminations,d = difficulties,Theta = Theta,itemtype = itemtype,returnList = T)

  DIFindex = which(rowSums(as.matrix(discriminations[,2:Nr.dim]))>0)
  DIFside = rep(0,tlength)
  DIFside[DIFindex] = -1

  # Aggregate and return data
  return(list(dat = data.frame(i = I(data$data),groups = factor(c(rep(0,nobs[1]),rep(1,nobs[2])))),
              discriminations = discriminations, difficulties = difficulties,DIFindex = DIFindex,Theta = Theta,DIFside = DIFside))

}


