#' Function to create a item discrimination parameter matrix
#' @param dimensions  integer specifying the number of dimensions used in dgp (currently only 2 are allowed)
#' @param DIFpercent  percentage of DIF items in the test
#' @param tlength     interger > 0, test length (number of items)
#' @param DIF_mode    character vector specifying the mode how to create the matrix:
#' - "intersect":       all items load on the first, length*DIFpercent items also on the second
#' - "disjoint":        ceiling(tlength*DIFpercent) items load on the first, the rest on the second, where ceiling rounds the number up to the next integer
#' @return A binary item discrimination parameter matrix of dimension: tlength x dimensions
getItemDiscrimination <- function(dimensions,DIFpercent,tlength,DIF_mode = c("intersect","disjoint")){
  if(dimensions!=1 && dimensions == 2){ #currently only two dimensions supported
    return(
      switch(DIF_mode,
              #disjoint: tlength*DIFpercent items load on the first, the rest (1-tlength*DIFpercent) on the second
              disjoint={
                cbind(rep(c(1,0),times=c(floor(tlength*(1-DIFpercent)),ceiling(tlength*DIFpercent))),rep(c(0,1),times=c(floor(tlength*(1-DIFpercent)),ceiling(tlength*DIFpercent))))
              },
             #intersect: all items load on the first, length*DIFpercent items also on the second (and default)
              {
                if(DIF_mode != "intersect") warning("DIF mode not valid. 'intersect' is used.")
                cbind(rep(1,tlength),rep(c(1,0),times=c(ceiling(tlength*DIFpercent),floor(tlength*(1-DIFpercent)))))
              }
      )
    )
  }else return(rep(1,tlength))
}
