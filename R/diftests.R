#' Extend diftests function of psychotools to include offset
#'@param obj1 - rasch model object 1
#'@param obj2 - rasch model object 2
#'@param anchor_items anchor items
#'@param adjust p-value adjustment, (default: "none")
#'@param offset offset
#'@return list containing test results, item paramters and covariance
diftests <- function (obj1, obj2, anchor_items, adjust, offset = 0)
{
    if (!requireNamespace("multcomp", quietly = TRUE)) {
        stop("Package \"multcomp\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    ip1 <- psychotools::itempar(obj1, ref = anchor_items)
    ip2 <- psychotools::itempar(obj2, ref = anchor_items)
    alias <- anchor_items[1]
    cf <- c(stats::coef(ip1)[-alias], stats::coef(ip2)[-alias] + offset)
    k <- length(cf)/2
    vc <- matrix(0, 2 * k, 2 * k)
    vc[1:k, 1:k] <- stats::vcov(ip1)[-alias, -alias]
    vc[-(1:k), -(1:k)] <- stats::vcov(ip2)[-alias, -alias]
    names(cf) <- colnames(vc) <- rownames(vc) <- c(paste(colnames(obj1$data)[-alias],
        1, sep = "_"), paste(colnames(obj2$data)[-alias], 2,
        sep = "_"))
    mod <- list(coefficients = cf, vcov = vc)
    class(mod) <- "raschmodel"
    contr <- cbind(diag(k), -diag(k))
    colnames(contr) <- names(cf)
    rownames(contr) <- colnames(obj1$data)[-alias]
    test <- if (adjust == "none")
        multcomp::univariate()
    else multcomp::adjusted(type = adjust)
    ftest <- summary(multcomp::glht(mod, linfct = contr), test = test)
    return(list(test = ftest, itempars = cf, vcovs = vc))
}

