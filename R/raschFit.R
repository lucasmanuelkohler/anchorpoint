#' Fits Rasch models for the reference group 0 and the focal group 1
#' @param data - data.frame - simulated or a real data. Must contain:
#' - response item matrix (matrix), binary (0/1) input.
#' - group (vector), the group of the test takers.
#' @param resp.mat.name string vector, the name of the response matrix in 'data' input with 'i' as a default (as dgp).
#' @param group.name string vector, the group name in the data frame 'data' (as dgp).
#' @examples
#' # Load the SPISA data set (general knowledge quiz - more information at ?SPISA)
#' library("psychotree")
#' data("SPISA")
#'
#' # Fit the Rasch Models for the two groups females and males
#' fit <- raschFit(SPISA, resp.mat.name='spisa', group.name='gender')
#' @export
#' @return two objects of class "raschmodel", produced by function \code{RaschModel.fit} of the package \pkg{psychotools}.
raschFit <- function(data, resp.mat.name='i', group.name='groups'){

  if (!requireNamespace("psychotools", quietly = TRUE)) {
    stop("Package \"psychotools\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Initially, we check if the groups vector is of a factor class or not. Then we fit rasch model for each group.
  if(class(data[,group.name])=='factor'){
    rm <- lapply(levels(data[,group.name]), function(i)
      psychotools::RaschModel.fit(I(as.matrix(data[resp.mat.name]))[data[,group.name] == i, ]))
  }else {
    print("The groups variable in the data should be of a 'factor' class.")
    return(NULL)
  }

  ## Return either
    # if rm is succeeded to be created: two rasch model objects; rm1 and rm2.
    # if rm is failed to be created: return a message 'Fails to fit the Rasch model'.
  if(!(inherits(try(rm), "try-error"))) return(list(rm1=rm[[1]],rm2=rm[[2]]))
  print('Fails to fit the Rasch model')
  return(NULL)
}

#' Function to check user-specific Input for the right format
#' @param manuelInput manual Data input as list with response matrix and grouping vector
#' @param resp.var  name of the binary response matrix
#' @param group.var name of the binary grouping vector
#' @return Data ready for anchorpoint::raschFit function
checkInput <- function(manuelInput,resp.var,group.var){
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(is.list(manuelInput[,resp.var]))
    manuelInput = data.frame("i" = I(do.call(cbind,unname(manuelInput[,resp.var]))),"groups" = as.factor(manuelInput[,group.var]))
  else
    manuelInput = data.frame("i" = I(as.matrix((manuelInput[,resp.var]))),"groups" = as.factor(manuelInput[,group.var]))

  #remove NA rows: not necessary for response (the rasch model can handle them), but necessary for group memberships
  manuelInput <- manuelInput[stats::complete.cases(manuelInput$groups),]

  responseLevels = apply(stats::na.omit(manuelInput$i),MARGIN = 2,FUN = unique)
  check = which(apply(responseLevels,MARGIN = 2,function(x){!all(c(0,1)%in%x)}))
  if(all(apply(responseLevels,MARGIN = 2,length)==2) && any(check)){
    for(i in check){
      manuelInput$i[,i][which(manuelInput$i[,i]==responseLevels[1,i])] = 0
      manuelInput$i[,i][which(manuelInput$i[,i]==responseLevels[2,i])] = 1
    }
    class(manuelInput$i) <- "numeric"
  }

  groupLevels = levels(manuelInput$groups)

  if(length(groupLevels)==2 && !all(c(0,1)%in%groupLevels)) levels(manuelInput$groups) = c(0,1)


  idCheck = which(apply(X = manuelInput$i,MARGIN = 2,function(x){
    all(x==manuelInput$groups)
  }))

  if(any(idCheck)) manuelInput$i = manuelInput$i[,-idCheck]

  return(manuelInput)

}

