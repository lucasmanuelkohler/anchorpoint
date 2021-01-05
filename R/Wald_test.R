vcov.default <- function(object, ...) object$vcov

#' Function to create a item discrimination parameter matrix
#'@param rm1 Fitted Rasch Model object corresponding to the first group. Object is of class "raschmodel", produced by function \code{RaschModel.fit} of the package \pkg{psychotools}.
#'@param rm2 Fitted Rasch Model object corresponding to the first group. Object is of class "raschmodel", produced by function \code{RaschModel.fit} of the package \pkg{psychotools}.
#'@param c_shift The shift of the second group
#'@param alias_method character vector specifying the aliasing method. One of "constant4_MPT", "quasi_var", "min_dist".
#'@param alias_anchor_items integer in 1,...,#items. Default: NULL, will be chosen according to alias_method
#'@param adjust  p-value adjustment (multiple testing correction), Default: "none"
#'@return  list containing
#' - p: results from the test (p-values)
#' - vcov: the covariance matrices of the fit (from diftests function)

Wald_test <- function(rm1, rm2, c_shift, alias_method = c("constant4_MPT", "quasi_var", "min_dist"), alias_anchor_items = NULL, adjust = "none")
{

  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # All alias methods need at some point the multcomp package
  if (!requireNamespace("multcomp", quietly = TRUE)) {
    stop("Package \"multcomp\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  alias_method <- match.arg(alias_method)

  cf1 <- c(0, stats::coef(rm1))
  cf2 <- c(0, stats::coef(rm2))

  if(alias_method == "constant4_MPT"){ # for comparison with old results

    # the item that is first selected into MPT anchor is used for aliasing
    aliased_item <- alias_anchor_items[1]

    # in function diftests, the aliased item is used BOTH for
    # 1) moving the missing standard error to that item
    # 2) but also (inseparably from moving the s.e.) moves all item parameters
    #   by subtracting the original estimate of the aliased item in each group
    #   (usually a different value in each group)
    # therefore we need to change the shift for group 2 to accound for the
    # part of the shift that already happens in 2) internally in diftests
    my_offset <- c_shift - (cf1[aliased_item]-cf2[aliased_item])

    testresults <- diftests(rm1, rm2, anchor_items = aliased_item,
                            adjust = adjust, offset = my_offset)

    results <- rep(NA, length(colnames(rm1$data)))
    names(results) <- colnames(rm1$data)
    results[which(colnames(rm1$data) %in% names(testresults$test$test$pvalues))] <- testresults$test$test$pvalues
  }
  else if(alias_method == "quasi_var"){

    if (!requireNamespace("qvcalc", quietly = TRUE)) {
      stop("Package \"qvcalc\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    vc1 <- rbind(0, cbind(0, stats::vcov(rm1)))
    vc2 <- rbind(0, cbind(0, stats::vcov(rm2)))

    k <- length(cf1)

    cf <- c(cf1, cf2 + c_shift)
    vc <- diag(c(qvcalc::qvcalc(vc1, estimates = cf1)$qvframe$quasiVar,
                 qvcalc::qvcalc(vc2, estimates = cf2)$qvframe$quasiVar))

    ## collect in trivial "model object"
    mod <- list(coefficients = cf, vcov = vc)

    ## pairwise contrasts
    contr <- cbind(diag(k), -diag(k))

    ## test employed
    test <- if (adjust == "none")
      multcomp::univariate()
    else multcomp::adjusted(type = adjust)

    results <- summary(multcomp::glht(mod, linfct = contr), test = test)$test$pvalues[1:k]
    names(results) <- colnames(rm1$data)

  }else if(alias_method == "min_dist"){
    # compute distances to find item/items with minimum distance
    cf2_shifted <- cf2 + c_shift
    abs_dist <- abs(cf1 - cf2_shifted)

    # one randomly picked item from those items with minimum dist is used for aliasing
    candidates = which(abs_dist == min(abs_dist))
    aliased_item = ifelse(test = (length(candidates)>1), yes = sample(x = candidates, size =  1), no = candidates)

    # in function diftests, the aliased item is used BOTH for
    # 1) moving the missing standard error to that item
    # 2) but also (inseparably from moving the s.e.) moves all item parameters
    #   by subtracting the original estimate of the aliased item in each group
    #   (usually a different value in each group)
    # therefore we need to change the shift for group 2 to accound for the
    # part of the shift that already happens in 2) internally in diftests
    my_offset <- c_shift - (cf1[aliased_item]-cf2[aliased_item])

    testresults <- diftests(rm1, rm2, anchor_items = aliased_item,
                            adjust = adjust, offset = my_offset)

    results <- rep(NA, length(colnames(rm1$data)))
    names(results) <- colnames(rm1$data)
    results[which(colnames(rm1$data) %in% names(testresults$test$test$pvalues))] <- testresults$test$test$pvalues

  }else{print("Please specify a alias method");return(NULL)}

  return(list(p = results,vcov = testresults$test$vcov))
}




