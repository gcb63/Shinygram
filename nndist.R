# nndist.R, gcb, 18 May 2017

nndist <- function(xy) {
  nd <- nrow(xy)
  nnd <- rep(0,nd)
  for (i in 1:nd) {
    dst <- sqrt((xy[,1]-xy[i,1])^2+(xy[,2]-xy[i,2])^2)
    nnd[i] <- min(dst[dst>0])
  }
  return(nnd)
}