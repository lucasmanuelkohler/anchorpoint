#' Plot function
#' hand over location_picker = TRUE, to identify specific points in the plot
#' to terminate the function, press any mouse button other than the first (X11 device) or press ESC key (quartz)
#' see ?identify for help
#' @param x anchorpoint object as produced by the function \code{anchorpoint}
#' @param ask logical, ask for next plot. Default = TRUE
#' @param location_picker logical, use location picker. Default FALSE.
#' @param ...  additional parameters for plot function as for standard plot function (e.g. col)
#' @export
plot.anchorpoint <- function(x,ask = T,location_picker = FALSE,...) {
  if (!inherits(x, "anchorpoint"))
    stop("use only with \"anchorpoint\" objects")


  if(ask & (length(x$criterion) > 1 || length(x$grid) > 1)){
    oask <- grDevices::devAskNewPage(TRUE)
    on.exit(grDevices::devAskNewPage(oask))
  }

  out <- list()
  temp <- x$all_results
  grDevices::dev.hold()
  for(grid in names(temp)){
    for(criterion in names(temp[[grid]])){
      out[[grid]][[criterion]] <- plotCriterion(temp[[grid]][[criterion]],names = list(grid = grid,criterion = criterion),location_picker,...)
    }
  }
  grDevices::dev.flush()

  class(out) <- "plot.anchorpoint"
  if (length(out)>0) return(out)
}

#' Print function for plot.anchorpoint
#' @param x  plot.anchorpoint object
#' @param ... further arguments passed to or from other methods.
#' @export
print.plot.anchorpoint <- function(x,...) {
  if (!inherits(x, "plot.anchorpoint"))
    stop("use only with \"plot.anchorpoint\" objects")

  for(grid in names(x)){
    for(criterion in names(x[[grid]])){
      nam <- c("shift", "scaled criterion value", "criterion value", "grid position", "anchor item")[1:length(x[[grid]][[criterion]])]
      cat(paste("Location pick(s) for",grid,"grid with",criterion,"measure:",sep = " "),"\n")
      print(x[[grid]][[criterion]])
      cat("\n")
    }
  }
}

#' Print function summarizes the
#' @param x anchorpoint object as produced by the function \code{anchorpoint}
#' @param ... further arguments passed to or from other methods (e.g. digits for rounding).
#' @export
print.anchorpoint <- function(x,...) {
  if (!inherits(x, "anchorpoint"))
    stop("use only with \"anchorpoint\" objects")

  temp <- x$global_optimum
  for(grid in names(temp)){
    for(criterion in names(temp[[grid]])){
      nam <- c("shift", "criterion value", "anchor item")[1:length(temp[[grid]][[criterion]])]
      cat(paste("Global optimum for",grid,"grid with",criterion,"measure:",sep = " "),"\n")
      print(structure(unlist(temp[[grid]][[criterion]])),...)
      cat("\n")
    }
  }
  cat("For local optima use plot(anchorpoint.obj, location_picker = TRUE) or summary(anchorpoint.obj)")
}

#' Print function for summary.anchorpoint
#' @param x  summary.anchorpoint object
#' @param ... further arguments passed to or from other methods.
#' @export
print.summary.anchorpoint <- function(x, ...) {
  if (!inherits(x, "summary.anchorpoint"))
    stop("use only with \"summary.anchorpoint\" objects")

  temp <- x$global_optimum
  for(grid in names(temp)){
    for(criterion in names(temp[[grid]])){

      nam <- c("shift", "criterion value", "grid position", "anchor item")[1:length(x$global_optimum[[grid]][[criterion]])]

      cat(paste("Global optimum for",grid,"grid with",criterion,"measure:",sep = " "),"\n")
      print(structure(as.data.frame(x$global_optimum[[grid]][[criterion]]), names = nam),...)

      nam <- c("shift", "criterion value", "anchor item")[1:length(x$all_results[[grid]][[criterion]])]

      cat("\n")
      cat(paste("All results for",grid,"grid with",criterion,"measure:",sep = " "),"\n")

      print(structure(as.data.frame(x$all_results[[grid]][[criterion]]), names = nam),...)
      cat("\n")
      invisible(x)

    }
  }
}

#' Summary function
#' @param object anchorpoint object as produced by the function \code{anchorpoint}
#' @param ...  additional arguments affecting the summary produced.
#' @export
summary.anchorpoint <- function(object,...) {
  if (!inherits(object, "anchorpoint"))
    stop("use only with \"anchorpoint\" objects")

  ans <- object[1:2]
  class(ans) <- "summary.anchorpoint"
  ans
}

#' Print function for WaldtestpV object
#' @param x  Wald test object
#' @param ... further arguments passed to or from other methods.
#' @export
print.WaldtestpV <- function(x,...) {
  if (!inherits(x, "WaldtestpV"))
    stop("use only with \"WaldtestpV\" objects")

  if("shift" %in% names(x)){
    cat(paste("Wald test p-values for shift = ",x$shift,":",sep = ""),"\n")
    print(x$pv,...)
    cat("\n")
  }else{
    for(grid in names(x)){
      for(criterion in names(x[[grid]])){
        cat(paste("Wald test p-values for",grid,"grid with",criterion,"measure:",sep = " "),"\n")
        print(x[[grid]][[criterion]],...)
        cat("\n")
      }
    }
  }

}

#' Function to get Wald test p-value results
#' @param object anchorpoint object as produced by the function \code{anchorpoint}
#' @param shift  shift in item parameters for the second group, default NULL (for global optimum), else numeric (for user-defined shift)
#' @param ...  further arguments for signif(x,...) (digits)
#' @examples
#' # Load the SPISA data set (general knowledge quiz - more information at ?SPISA)
#' library("psychotree")
#' data("SPISA")
#'
#' # Fit the Rasch Models for the two groups females and males
#' fit <- raschFit(SPISA, resp.mat.name='spisa', group.name='gender')
#'
#' # Rasch Model fit for the first and second group
#' rm1 <- fit$rm1
#' rm2 <- fit$rm2
#'
#' # Fit an Anchorpoint object
#' ap_object <- anchorpoint(rm1,rm2,select = "Gini Index", grid = "sparse")
#'
#' # Obtain the Wald test p-values
#' WaldtestpV(ap_object)
#' @export
WaldtestpV <- function(object, shift = NULL,...) {

  out<- list()
  if(!is.null(shift) && is.numeric(shift)){
    out$pv <- signif(getWald(object$rm,shift = shift)$p,...)
    out$shift <- shift
  }else{
    temp <- object$global_optimum
    for(grid in names(temp)){
      for(criterion in names(temp[[grid]])){
        out[[grid]][[criterion]] <- signif(getWald(object$rm,shift =  if(is.null(shift) | !is.numeric(shift)) temp[[grid]][[criterion]]$shift else shift)$p,...)
      }
    }
  }
  class(out) <- "WaldtestpV"
  out
}

#' @title Function to produce anchorpoint objects
#' @description Function to conduct the anchor point selection method of Strobl et al. (2021)
#' @param rm1 Fitted Rasch Model object for the first group of test takers
#' @param rm2 Fitted Rasch Model object for the second group of test takers
#' @param select a string, specifying the criterion that is evaluated ("CLF Criterion" or "Gini Index", abbreviations are accepted)
#' @param grid a string, specifying the method that is used to generate the grid of possible shifts to be evaluated
#' @return an anchorpoint object containing:
#' - list with results for global optimum (single grid value and criterion value)
#' - list with all results (all grid values and criterion values)
#' - string with used criteria
#' - string with used grid methods
#' - list with Rasch Model objects for both groups of test takers
#' @export
#' @examples
#' # Load the SPISA data set (general knowledge quiz - more information at ?SPISA)
#' library("psychotree")
#' data("SPISA")
#'
#' # Fit the Rasch Models for the two groups females and males
#' fit <- anchorpoint::raschFit(SPISA, resp.mat.name='spisa', group.name='gender')
#'
#' # Rasch Model fit for the first and second group
#' rm1 <- fit$rm1
#' rm2 <- fit$rm2
#'
#' # Fit an Anchorpoint object
#' ap_object <- anchorpoint(rm1,rm2,select = "Gini Index", grid = "sparse")
#'
#' # inspect the Anchorpoint object
#' # The print function summarizes the Global Optimum for the selected methods
#' print(ap_object)
#'
#' # The summary function summarizes the Global Optimum for the selected methods
#' # and shows all the other results
#' summary(ap_object)
#'
#' # The plot function shows the criterion plot (criterion value vs. shifts).
#' plot(ap_object)
#'
#' # To extract the criterion value and shift for a specific position on the plot,
#' # set location_picker = TRUE and execute the command.
#' # Then, click on the desired positions and press ESCAPE.
#' plot(ap_object, location_picker = TRUE)
#' @references
#' - Strobl, C., Kopf, J., Kohler, L., von Oertzen, T. & Zeileis, A. (2021). Anchor point selection: An approach for anchoring without anchor items. Applied Psychological Measurement, to appear.
anchorpoint <- function(rm1, rm2, select = c("CLF Criterion","Gini Index"), grid = c("symmetric", "sparse")){

  select <- match.arg(select, several.ok = TRUE)
  grid <- match.arg(grid, several.ok = TRUE)

  resultList <- getCriterionRes(rm1 = rm1,rm2 = rm2,shift = NULL,grid = grid,select = select)

  global_optimum <- lapply(resultList$res,function(resGrid){
    lapply(resGrid[select],function(resCriterion){
      list(
        shift = resCriterion$criterion_values$cshift$shift,
        criterion_value = resCriterion$criterion_values$cshift$value,
        index = resCriterion$criterion_values$cshift$idx
        )
    })
  })

  results_list <- lapply(resultList$res,function(resGrid){
    lapply(resGrid[select],function(resCriterion){
      list(shift = resGrid$shifts,
           criterion_value = resCriterion$criterion_values$values)
    })
  })

  getAliasedItems <- function(rm1,rm2,shift){
    cf1 <- c(0, stats::coef(rm1))
    cf2 <- c(0, stats::coef(rm2))

    lenGrid <- length(shift)
    aliased_items <- rep(0,lenGrid)

    for(i in 1:lenGrid){
      cf2_shifted <- cf2 + shift[i]
      abs_dist <- abs(cf1 - cf2_shifted)

      # one randomly picked item from those items with minimum dist is used for aliasing
      candidates = which(abs_dist == min(abs_dist))
      aliased_items[i] = ifelse(test = (length(candidates)>1), yes = sample(x = candidates, size =  1), no = candidates)
    }
    return(aliased_items)
  }

  if("sparse" %in% names(results_list)){
    for(i in 1:length(results_list$sparse)){
      aliased_items <- getAliasedItems(rm1,rm2,resultList$res$sparse$shifts)
      global_optimum$sparse[[i]]$anchor_item <- as.integer(aliased_items[resultList$res$sparse[[i]]$criterion_values$cshift$idx])
      results_list$sparse[[i]]$anchor_item <- aliased_items
    }
  }
  out <- list(global_optimum = global_optimum, all_results = results_list,criterion = select,grid = grid,rm = list(rm1 = rm1,rm2 = rm2))
  class(out) <- "anchorpoint"
  return(out)
}

#########
#Generate Grid and criterion values
#########

#' Function to evaluate criterion values and obtain test results for a given grid and method
#' @param grid  The grid values: output of the "generateGrid.R" function
#' @param shift  the desired shift
#' @param getTestResults  logic, whether test should be applied
#' @param rm  list containing the two Rasch Model corresponding two group 0 and 1
#' @param metric  criterion to evaluate as a function
#' @return a list containing the criterion evaluated at grid points and the result of the Wald test
get_results <- function(grid,shift,getTestResults,rm,metric){

  out <- lapply(metric,function(metric){
    criterion_values = list(values = unlist(lapply(grid$betas_grid,metric)))
    if(is.null(shift)){

      # to catch and handle strange inputs resulting in all almost zero criterion values during the simulation
      if(all(abs(criterion_values$values)<1e-5)){
        idx = which(
          unlist(lapply(X = grid$betas_grid,FUN = function(x){
            if(abs(x[1,3])<1e-2){
              x[,3] = rep(0,length(x[,3]))
              return(1)
            }else return(0)
          }))
          ==1)
        len_argmax = length(idx)
        if(len_argmax>1){
          if(!len_argmax%%2) shift = mean(c(grid$c_grid[[idx[len_argmax/2]]],grid$c_grid[[idx[len_argmax/2+1]]]))
          else shift = grid$c_grid[[idx[ceiling(len_argmax/2)]]]
        }else shift = grid$c_grid[[idx]]

      }else{
        idx = which.max(criterion_values$values)
        shift = grid$c_grid[[idx]]
        value = criterion_values$values[idx]
      }

    }else{
      if(length(shift)==1){
        idx = which.min(abs(grid$c_grid-shift))
        value = criterion_values$values[idx]
      }
      else print("not a valid shift")
    }

    TestResults = NA
    if(getTestResults){
      if(!all(is.na(rm))){
        TestResults = getWald(rm,shift = shift)$p
      }else warning("Need rasch model object to perform Wald test. Set argument getTestResults in criterion_plot() to FALSE to avoid warning message.")
    }
    criterion_values$cshift = data.frame(shift = shift,idx = idx,value = value)

    return(list(criterion_values = criterion_values,TestResults = TestResults))
  })
  out$shifts = grid$c_grid
  return(out)
}

#' Criterion function
#' @param rm1 Fitted Rasch Model object corresponding to the first group. Object is of class "raschmodel", produced by function \code{raschmodel} of the package \pkg{psychotools}.
#' @param rm2  Fitted Rasch Model object corresponding to the second group. Object is of class "raschmodel", produced by function \code{raschmodel} of the package \pkg{psychotools}.
#' @param select criterion: Gini Index or CLF Criterion
#' @param grid  grid method: symmetric or sparse
#' @param shift  desired shift. if NULL, then the criterion maximizing is used. Can also be numeric to get desired shift. Caution: must be within grid!
#' @return a list which contains:
#' - a list with the results (grid values, criterion values, information about the optima)
#' - a rm object,
getCriterionRes <- function(rm1, rm2, select = c("Gini Index","CLF Criterion"), grid = c("symmetric","sparse"), shift = NULL){

  select <- match.arg(select, several.ok = TRUE)
  grid <- match.arg(grid, several.ok = TRUE)

  metrics <- list('Gini Index' = function(x) return(ifelse(test = all(x[,3]==0),yes = 0,no = ineq::Gini(abs(x[,3])))),
                    'CLF Criterion' = function(x) -1*clfCriterion(x[,3],eps = 0)
                    )

  rm <- list(rm1 = rm1,rm2 = rm2)
  cf1 = c(0,stats::coef(rm$rm1))
  cf2 = c(0,stats::coef(rm$rm2))
  resultList <- list()

  #not allowing shift outside the grid.
  #however, as both the shiny app and the anchorpoint function don't allow grid values outisde, this only applies when the getCriterionRes function is used manually.
  x_range <- list(sparse = c(min(cf1 - cf2),max(cf1 - cf2)),symmetric = c((min(cf1)-max(cf2)), max(cf1) - min(cf2)))

  # correct unfeasible input - only necessary when the function getCriterionRes is used directly (as for example in the shiny app)
  if(!all((feasibleRight <- shift<sapply(x_range,function(x)x[2]))&(feasibleLeft <- shift>sapply(x_range,function(x)x[1])))){

    out <- sapply(grid, function(x){
      return(x_range[[x]][!c(feasibleLeft[x],feasibleRight[x])])
    })

    for(i in 1:length(out)){
      if(length(out[[i]])>0){
        warning(paste("shift outside",names(out)[i], "grid - changed to closest possible feasible shift",sep = " "))
        #if both are unfeasible, the edge point agreeing with both grids should be chosen... not yet implemented.
        shift = out[[i]]
      }
    }

  }

  Grid_list <- generateGrid(beta1 = cf1,beta2 = cf2, grid_method = grid)
  resultList$res <- lapply(Grid_list,FUN = get_results,shift = shift, getTestResults = TRUE,rm = rm,metric = metrics[select])

  resultList$rm <- rm
  return(resultList)
}

#########
#Plot functions
#' Function to produce criterion plot
#' @param object anchorpoint object as produced by the function \code{anchorpoint}
#' @param names list, with criterion and grid: names of the methods used.
#' @param location_picker use location picker
#' @param lty line type
#' @param col color
#' @param cex.axis cex.axis
#' @param cex.lab cex.lab
#' @param cex.main cex.main
#' @param cex cex
#' @return selected points with additional information
plotCriterion <- function(object,names,location_picker = FALSE,lty = 1,col = 1,cex.axis = 1,cex.lab = 1, cex.main = 1,cex = 1){
  results_list <- object
  #0-1 scaling
  scaler <- function(dat){
    shift = min(dat)
    scale = (max(dat)-shift)
    return((dat-shift)/scale)
  }

  criterion_values <- results_list$criterion_value
  criterion_values_scaled  <- scaler(criterion_values)

  grid_list <- results_list$shift
  margin <- c(-1,1)*0.025
  x_range <- range(results_list$shift) + margin

  plot(x=NULL,y=NULL, xaxt="n",yaxt="n",xlim=x_range, ylim=c(0,1) + margin, ylab = "Normalized criterion value", xlab = "Shift c",main =  paste("Criterion:",names$criterion, "-", "Grid:",names$grid,sep = " "),cex.main = cex.main, cex.lab = cex.lab)
  graphics::axis(side=1, at=seq(floor(x_range[1]),ceiling(x_range[2]),0.1), cex.axis = cex.axis)
  graphics::axis(side=2, at=seq(0,1,0.1), cex.axis = cex.axis)

  graphics::lines(x = grid_list, y = criterion_values_scaled,type = 'l', "Normalized criterion value", xlab = "Shift c", lty = 1, col = col)
  graphics::legend("topright",legend = names$criterion,lty = lty,col = col)

  if(location_picker){
    cat("Move the cross to the point whose position is wanted. \n")
    #Selection of grid points on plot
    points <- graphics::identify(x = grid_list,y = criterion_values_scaled,tolerance = 0.5,labels = paste("c  = ",round(grid_list,2),sep = ""),offset = 0,cex = cex)
    lenP <- length(points)
    if(lenP){
      graphics::segments(x0 = grid_list[points],x1 = grid_list[points],y0 = rep(-.1,lenP),y1 = criterion_values_scaled[points],col = scales::alpha(col,.25),lty = 2)
      out <- cbind(grid_list[points],criterion_values_scaled[points], criterion_values[points], points, results_list$anchor_item[points])
      colnames(out) <- c("shift", "scaled criterion value", "criterion value", "grid position", "anchor item")[1:dim(out)[2]]
      rownames(out) <- paste("pick", 1:lenP,sep = " ")
      return(as.data.frame(out))
    }
  }
}

#' Function to produce graphical test plot
#' @param object anchorpoint object as produced by the function \code{anchorpoint}
#' @param shift shift in item parameters for the second group, default NULL (for global optimum), else numeric (for user-defined shift)
#' @param highlight positive integer(s), numbers of the items to be highlighted
#' @param alpha   significance level for DIF test
#' @param testColors  list with colors for the items:
#' - "not significant" = "darkgreen"
#' - "significant" = "red3"
#' - "anchor item" = "black"
#' @param TestResults  Waldtest object from anchorpoint::getWald. If NULL, then they are computed within the function. Default: NULL.
#' @param ask logical, ask for next plot. Default = TRUE
#' @param ...   further arguments for plot() like lty, cex.axis, cex.main, cex.lab etc.
#' @export
#' @examples
#' # Load the SPISA data set (general knowledge quiz - more information at ?SPISA)
#' library("psychotree")
#' data("SPISA")
#'
#' # Fit the Rasch Models for the two groups females and males
#' fit <- raschFit(SPISA, resp.mat.name='spisa', group.name='gender')
#'
#' # Rasch Model fit for the first and second group
#' rm1 <- fit$rm1
#' rm2 <- fit$rm2
#'
#' # Fit an Anchorpoint object
#' ap_object <- anchorpoint(rm1,rm2,select = "Gini Index", grid = "sparse")
#'
#' # Use the Anchorpoint object to get the graphical test
#' graphicalTest(ap_object)
#' @references
#' Credit: Part of the code is adapted from the function \code{plotGOF} of the package \pkg{eRm} (Version:  (Version: 1.32.1).).
graphicalTest <- function(object,shift = NULL,highlight = NULL,alpha = 0.05,
                          testColors = list("not significant"="darkgreen","significant"="red3","anchor item"="black"),TestResults = NULL,ask = TRUE,...){

  if (!inherits(object, "anchorpoint"))
    stop("use only with \"anchorpoint\" objects")

  # Do plot with a specific shift
  plt <- function(shift,...){
    stopifnot("Shift must be numeric" = is.numeric(shift))
    if (!inherits(TestResults, "Waldtest")) TestResults <- getWald(object$rm,shift = shift)
    beta1 <- c(0, stats::coef(object$rm$rm1))
    cf2 <- c(0, stats::coef(object$rm$rm2))
    beta2 <- cf2 + shift

    len <- length(beta1)

    results <- rep(NA, len)
    names(results) <- colnames(object$rm$rm1$data)

    aliased_item <- which(is.na(TestResults$p))

    #standard approach (extracting variance estimation from rasch model fit & calculating p-values with test statistic: diff(b1,b2)/sqrt(var1_hat + var2_hat) ~ N(0,1)
    # (thats how it was implemented - as we deal with unknown variances, a t-test would probably more suitable..)
    standard <- function(){
      vars <- diag(TestResults$vcov)

      v1 = rep(0,length(beta1))
      v1[seq(1:length(beta1))[-aliased_item]] <- vars[1:length(beta1)-1]

      v2 = rep(0,length(beta1))
      v2[seq(1:length(beta1))[-aliased_item]] <- vars[-(1:length(beta1)-1)]

      return(list(s1 = sqrt(v1), s2 = sqrt(v2), pvalues = TestResults$p))
    }

    res <- standard()
    print(res)

    results[seq(1:len)[-aliased_item]] <- res$pvalues[-aliased_item]

    names(testColors) <- c("not significant","significant","anchor item")
    if(!is.list(testColors)) testColors = as.list(testColors)

    colVec = rep(testColors$`not significant`,length(beta1))
    colVec[which(results<alpha)] <- testColors$significant
    colVec[which(is.na(results))] <- testColors$`anchor item`
    z <- stats::qnorm(alpha/2.0, lower.tail = FALSE)

    setRange <- max(diff(range(beta1)+c(-1,1)*stats::var(beta1)/2),diff(range(beta2)+c(-1,1)*stats::var(beta2)/2))
    rangeBeta1 <- min(beta1) + diff(range(beta1))/2 +c(-1,1)*setRange/2
    rangeBeta2 <- min(beta2) + diff(range(beta2))/2 +c(-1,1)*setRange/2

    ci1u <- beta1 + z*res$s1
    ci1l <- beta1 - z*res$s1
    ci2u <- beta2 + z*res$s2
    ci2l <- beta2 - z*res$s2

    pchVector = rep(20,length(beta1))
    pchVector[aliased_item] <- 18

    plot(beta1,beta2,xlim = rangeBeta1,ylim = rangeBeta2,pch = pchVector,col = scales::alpha(colVec, 0.8),main = "Graphical Test Plot",xlab = "Item Parameters group 0",ylab = "Item Parameters group 1",...)
    graphics::abline(coef = c(0,1))
    graphics::text(x = beta1,y = beta2,labels =  1:length(beta1), cex= 0.5,pos = 4,col=colVec)

    # simple ellipse to replace the function ellipse() from the car package
    simple_ellipse <- function(center, a, b, n = 200L, border_col, col){
      angle_t <- seq(0, 2*pi, length.out = n)[-1L]
      graphics::polygon(center[1L] + a * cos(angle_t), center[2L] + b * sin(angle_t), lwd = 1.0, border = border_col,col = col)
    }

    highlight <- if(is.null(highlight)) numeric(0)
    else if(is.character(highlight)) as.numeric(highlight)
    else if(highlight == "significant") which(results<alpha)

    for(i in 1:length(beta1)){

      graphics::segments( x0 = c(beta1[i], ci1l[i]), y0 = c(ci2l[i], beta2[i]),
                          x1 = c(beta1[i], ci1u[i]), y1 = c(ci2u[i], beta2[i]),
                          col = scales::alpha(colVec[i], 0.4),lwd = 1,...)

      simple_ellipse( center = c(beta1[i],beta2[i]),
                      a = abs(diff(c(ci1u[i],ci1l[i])))/2,
                      b = abs(diff(c(ci2u[i],ci2l[i])))/2,
                      n = 200L, border_col = scales::alpha(colVec[i], 0.9), col = if(i %in% highlight) scales::alpha(colVec[i], 0.4) else NA)

    }
  }

  if(is.null(shift)){
    if(ask & (length(object$criterion) > 1 || length(object$grid) > 1)){
      oask <- grDevices::devAskNewPage(TRUE)
      on.exit(grDevices::devAskNewPage(oask))
    }
    temp <- object$all_results
    for(grid in names(temp)){
      for(criterion in names(temp[[grid]])){
        plt(object$global_optimum[[grid]][[criterion]]$shift,...)
      }
    }
  }else plt(shift,...)
}

#' Function to produce a shift plot
#' @param object anchorpoint object as produced by the function \code{anchorpoint}
#' @param shift shift in item parameters for the second group, default NULL (for global optimum), else numeric (for user-defined shift)
#' @param testColors  list with colors for the items:
#' - "not significant" = "darkgreen"
#' - "significant" = "red3"
#' - "anchor item" = "black"
#' @param testPCH  list with pch for the items (for color blind people):
#' - "not significant" = 21
#' - "significant" = 22
#' - "anchor item" = 23
#' @param addLegend  logic, add a legend to the plot, default: False
#' @param highlight  positive integer(s), numbers of the items to be highlighted
#' @param digits  positive integer, controls rounding of the shift in title
#' @param cex.legend  numeric,  controls size of legend
#' @param TestResults  Waldtest object from anchorpoint::getWald. If NULL, then they are computed within the function. Default: NULL.
#' @param ask logical, ask for next plot. Default = TRUE
#' @param ...  additional graphics arguments
#' @examples
#' # Load the SPISA data set (general knowledge quiz - more information at ?SPISA)
#' library("psychotree")
#' data("SPISA")
#'
#' # Fit the Rasch Models for the two groups females and males
#' fit <- raschFit(SPISA, resp.mat.name='spisa', group.name='gender')
#'
#' # Rasch Model fit for the first and second group
#' rm1 <- fit$rm1
#' rm2 <- fit$rm2
#'
#' # Fit an Anchorpoint object
#' ap_object <- anchorpoint(rm1,rm2,select = "Gini Index", grid = "sparse")
#'
#' # Use the Anchorpoint object to get the shift plot
#' shiftPlot(ap_object)
#' @export
shiftPlot <- function(object, shift = NULL,
                      testColors = list("not significant"="darkgreen","significant"="red3","anchor item"="black"),
                      testPCH = list("not significant"=21,"significant"=22,"anchor item"=23),# colors for no significant DIF, significant DIF and anchor item
                       addLegend = TRUE,highlight = NULL,digits = 3,cex.legend  = .5,TestResults = NULL,ask = TRUE,...){

  if (!inherits(object, "anchorpoint"))
    stop("use only with \"anchorpoint\" objects")

  # Do plot with a specific shift
  plt <- function(shift,...){
    stopifnot("Shift must be numeric" = is.numeric(shift))
    if(is.null(names(testColors))) names(testColors) <- list("not significant","significant","anchor item")
    cf1 = c(0,stats::coef(object$rm$rm1))
    cf2 = c(0,stats::coef(object$rm$rm2))

    if(length(highlight)>0){
      highlight = as.integer(highlight)
      range_violation = which((highlight<1)|(highlight>length(cf1))|is.na(highlight))
      if(length(range_violation)>0){
        warning("Some invalid items to be highlighted got excluded!")
        highlight = highlight[-range_violation]
      }
    }

    if (!inherits(TestResults, "Waldtest")) TestResults <- getWald(object$rm,shift = shift)

    col = rep(testColors$`not significant`,length(cf2))
    col[significant <- which(TestResults$p<=0.05)] = testColors$significant
    col[aliaseditem <- which(is.na(TestResults$p))] = testColors$`anchor item`

    cf2_shifted = cf2 + shift
    ylimits = range(c(range(cf2_shifted),range(cf1)))
    #ylimits[1] <- ylimits[1] - diff(ylimits)*0.1

    if(length(highlight)>0){

      x0 = highlight-0.4
      x1 = highlight+0.4
      y0 = ylimits[1]-.5
      y1 = ylimits[2]+.5

      plot(cf1,
           xlab = "Item", ylab = "Parameter",
           ylim = ylimits,
           xaxt="n",col = 1,
           panel.first = c(graphics::abline(v = highlight, lty = 2, col = 'grey'),
                           graphics::rect(x0,y0,x1,y1, col= grDevices::rgb(1,1,0,alpha=0.25),border = NA)),
           main = paste("Shifted item parameters at c = ",round(shift,digits = digits),sep = " "),...)

      graphics::text(highlight+0.25, ylimits[1]-0.1,labels = highlight,cex = 0.6)
    }else{
      plot(cf1, xlab = "Item", ylab = "Parameter",
           ylim = ylimits,
           xaxt="n",col = 1,
           main = paste("Shifted item parameters at c = ",round(shift,digits = digits),sep = " "),...)
    }

    pchVector = rep(testPCH$`not significant`,length(cf2))
    pchVector[significant] <- testPCH$significant
    pchVector[aliaseditem] <- testPCH$`anchor item`


    graphics::points(cf2_shifted,col=col,pch = pchVector,cex=.8,  bg=col)
    graphics::axis(side=1, at=seq(1,length(cf1)))
    if(addLegend) graphics::legend("top",legend = names(testColors),col = unlist(testColors),pch = unlist(testPCH),pt.bg = unlist(testColors),bty = 'n',ncol=3,  inset =  -.1,xpd = T,cex = cex.legend)
  }

  if(is.null(shift)){
    if(ask & (length(object$criterion) > 1 || length(object$grid) > 1)){
      oask <- grDevices::devAskNewPage(TRUE)
      on.exit(grDevices::devAskNewPage(oask))
    }
    temp <- object$all_results
    for(grid in names(temp)){
      for(criterion in names(temp[[grid]])){
        plt(object$global_optimum[[grid]][[criterion]]$shift,...)
      }
    }
  }else plt(shift,...)
}

