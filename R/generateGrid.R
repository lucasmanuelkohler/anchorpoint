#' This function generates the grid values.
#' @param beta1 Coefficients from first Rasch model (excluding first item, as from stats:coef call)
#' @param beta2 Coefficients from second Rasch model (excluding first item, as from stats:coef call)
#' @param grid_method a string, specifying the grid method that is used to generated the shifts for evaluation ("symmetric" and/or "sparse", abbreviations are accepted)
#' @param j.length positive integer, the granularity of the grid (default: 1000)
#' @return  A list with the selected grid methods each having two components:
#' 1. A vector 'c_grid' with the grid values
#' 2. A list of matrices 'betas_grid' for each grid value (length = j.length)
#' each matrix has three columns: coefficient from group 1 (beta1), shifted coefficient from group 2 (newbeta2) and distance between them (beta1-newbeta2).
#' The rows correspond to the items.
generateGrid <- function(beta1,beta2, grid_method=c('symmetric','sparse'), j.length=1000){

  ## Match the metod that the user specify to one of the three methods we have.
  grid_method <- match.arg(grid_method,several.ok = TRUE)

  ##  Find the grid values 'c_grid'
  res <- list()

  if('symmetric'%in%grid_method) { # symmetric
    res$symmetric$c_grid <- seq(from = (min(beta1)-max(beta2)),
                     to = max(beta1) - min(beta2), length.out=j.length)

    ## Determine the shifted betas (item parameters) for each grid value in the vector c_grid; beta+c_grid.
    newbeta_foc <- lapply(res$symmetric$c_grid,function(c_grid) beta2+c_grid )
    res$symmetric$betas_grid <- lapply(newbeta_foc, function(newbeta_foc) {cbind(beta1=beta1,newbeta2=newbeta_foc,dist=beta1-newbeta_foc)})

  }
  if('sparse'%in%grid_method){ # sparse
         # c. Timo's sparse grid based on proof: the maximum can occur at c_grid values where either
         #1. one pair of item parameters interlock, for item i i.e beta1i=beta2i+c_grid, so  c_grid = -(beta2i-beta1i)  (length=n)
         #2. or when the difference between two pairs is similar with different sign,
         ## i.e c_grid = -( (beta1i-beta2i) - (beta1j-beta2j) )/2 for al i!=j, (length = sum(n-1)=n(n-1)/2)
         ## Total number of the c_grid values that could have a max on it are n+ n(n-1)/2= n(n+1)/2
        # c_grid1 <- beta1 - beta2
        # c_grid2 <- combn(c_grid1,2,FUN = sum)/2
        #
        # names(c_grid1) <- NULL
        # names(c_grid2) <- NULL
        #
        # sparseGrid <- c(c_grid1,c_grid2)

        sparseGrid <- beta1 - beta2
        names(sparseGrid) <- NULL

        res$sparse$c_grid <- unique(sort(sparseGrid))
        stopifnot("sparse Grid is degenerate (only one element)" = length(res$sparse$c_grid)>1)


        ## Determine the shifted betas (item parameters) for each grid value in the vector c_grid; beta+c_grid.
        newbeta_foc <- lapply(res$sparse$c_grid,function(c_grid) beta2+c_grid )
        res$sparse$betas_grid <- lapply(newbeta_foc, function(newbeta_foc) {cbind(beta1=beta1,newbeta2=newbeta_foc,dist=beta1-newbeta_foc)})
  }

  ## Output: list of two components: 1. c_grid = vector of the grid values and
     # 2. a list where each c_grid have a matrix where for each item in the rows we have
     # three columns: beta1, shifted beta2 (newbeta2) and the distance between them (beta1-newbeta2)
  return(res)
}




