#' Data generating process for unidimensional rasch model
#' @param nobs number of observations per group
#' @param tlength interger > 0, test length (number of items)
#' @param DIFpercent percentage of DIF items in the test
#' @param DIFpattern "balanced": DIF balanced over groups "favorref","favorfoc": all DIF items favor one group
#' @param DIFeffect data generating process for DIF effect:
#' - normal:    item parameter differences are drawn at random from a normal distribution with mean DIFamount and sd = 0.1, like in Wang et al. (2012)
#' - uniform:   item parameter differences are drawn at random from the vector \code{[DIFamount-0.4,DIFamount-0.2,DIFamount,DIFamount+0.2,DIFamount+0.4]}
#' - constant:  item parameter differences are defined as \code{DIFamount} for all items
#' @param DIFamount magnitude of DIF
#' @param ability should the groups differ in mean ability? (default is TRUE)
#' @param sigmaable positive numeric vector of length two, standard deviations for person parameter distributions in the two groups (default is c(1,1))
#' @param itemref numeric vector of length tlength (if shorter, then sampling with replacement is used), item difficulty parameter for reference group like in Wang et al. (2012)
#' @return list containing:
#' - dat: binary response matrix
#' - DIFindex: indicating which items were generated with DIF
#' - DIFside: which group is favored per item (-1 focal, 1 reference) default: focal group is favored for all items
#' - itemref: item difficulty parameter for reference group
#' - itemfoc: item difficulty parameter for focal group
#' - groups: group vector (factor),
#' @export
#' @examples
#' # For examples, see ?getData.
#' @references
#' - Wang WC, Shih CL, Sun GW (2012). “The DIF-Free-Then-DIF Strategy for the Assessmentof Differential Item Functioning.”Educational and Psychological Measurement,72(4), 687–708
dgp_uni <- function(nobs,tlength,DIFpercent,
                       DIFpattern = "balanced",DIFeffect = "constant",DIFamount = 0.6,ability = TRUE,sigmaable=c(1,1),


                itemref=c(-2.522,-1.902,-1.351,-1.092,-0.234,
                          -0.317,0.037,0.268,-0.571,0.317,
                          0.295,0.778,1.514,1.744,1.951,
                          -1.152,-0.526,1.104,0.961,1.314,
                          -2.198,-1.621,-0.761,-1.179,-0.610,
                          -0.291,0.067,0.706,-2.713,0.213,
                          0.116,0.273,0.840,0.745,1.485,
                          -1.208,0.189,0.345,0.962,1.592) #Hartmann:[itemrefIndex], # Julia:[1:tlength]

)
{
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(ability==TRUE){
    muable=c(0,-1)
  } else {
    muable=c(0,0)
  }

  if(length(nobs)>1){
    nref = nobs[1]
    nfoc = nobs[2]
  }else{
    nref = nfoc = nobs/2
  }

   #1. If we would like to simulate data based on more than 40 items, then we sample with replacement from our itemref
  if(tlength>40){
    itemref = itemfoc = sample(x = itemref,size = tlength,replace = TRUE)
  } else{
   #2. Otherwise, we sample 'tlength'-time without replacement from the all itemref 40 items.
    itemref = itemfoc = sample(x = itemref,size = tlength,replace = FALSE)
  }

  nDIF <- DIFpercent*tlength         # number of DIF items (not necessarily whole number)
  nDIF <- floor(nDIF) + stats::rbinom(n=1,size=1, prob=nDIF %% 1)

  DIFside = rep(0,tlength)
  DIFindex = numeric(0)

  if(nDIF){
    DIFindex <- 1:nDIF

    #what kind of DIF: normal, uniform or constant
    tempDIFeffect <- switch(DIFeffect,
           #use normal DIF effect
           normal={
             stats::rnorm(nDIF, mean = DIFamount, sd = 0.1)
           },
           #use uniform DIF effect
           uniform={
             sample(c(DIFamount-0.4,DIFamount-0.2,DIFamount,DIFamount+0.2,DIFamount+0.4), size=nDIF,replace=TRUE)
           },
           #use constant DIF effect
           {
             if(DIFeffect != "constant") warning("DIF effect not valid. 'constant' DIF effect is used.")
             rep(DIFamount, times=nDIF)
           }
    )

    #pattern of DIF: balanced (both sided), favorref (positive), favorfoc (negative)
    DIFside_temp <- switch(DIFpattern,
                            #use balanced DIF pattern
                            balanced={
                              DIFside_temp <- rep(c(-1,1),each=nDIF/2) # for DIF Balanced tells if DIF is negative or positive
                              if(!nDIF %%2==0) DIFside_temp = c(DIFside_temp,sample(c(-1,1),size=1))
                              DIFside_temp
                            },
                            #favor reference group
                            favorref={
                              rep(1,nDIF)
                            },
                            #favor focal group (and default)
                            {
                              if(DIFpattern != "favorfoc") warning("DIF pattern not valid. 'favorfoc' DIF pattern is used.")
                              rep(-1,nDIF)
                            }
    )

    itemfoc[DIFindex] <- itemfoc[DIFindex] + DIFside_temp*tempDIFeffect # calculating item parameter for DIF items
    DIFside[DIFindex] = DIFside_temp

  }
  # item parameter matrix including tlength items and twice nobs observations
  itemmatrix <- rbind(matrix(data=itemref, ncol=length(itemref), nrow=nref, byrow=TRUE),
                      matrix(data=itemfoc, ncol=length(itemfoc), nrow=nfoc, byrow=TRUE))

  # person parameter matrix including twice nobs observations with muable, sigmaable
  thetapar <- c(stats::rnorm(nref, mean = muable[1], sd = sigmaable[1]), stats::rnorm(nfoc, mean = muable[2], sd = sigmaable[2]))

  # building binary response matrix
  eta <- exp(thetapar - itemmatrix)
  U <- matrix(stats::rbinom((nref+nfoc)*tlength,1,prob=(eta/(1+eta))),
              nrow=(nref+nfoc), ncol=tlength)

  # building data set
  data_out <- data.frame(i=I(as.matrix(U)), groups=as.factor(c(rep(0,times=nref),rep(1,times=nfoc))),
                     itempar=itemmatrix, thetapar=thetapar)

  return(list(dat=data_out,i=I(as.matrix(U)), DIFindex=DIFindex, DIFside = DIFside, itemref=itemref, itemfoc=itemfoc, groups=c(rep(0,times=nref),rep(1,times=nfoc))))
}
