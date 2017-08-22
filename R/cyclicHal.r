#' @export
#' 
#' @title Cyclic sampling by the Halton sequence
#' 
#' @description Draws a cyclic sample using the Halton sequence
#' 
#' 
cyclicHal <- function(J, b=c(3,5), plot.it=TRUE){
  
  
  if(plot.it){
    plot.halGrid(J,b,label=F)
  }
  
  # Pick random start in the center box. To do this, 
  # both bases must be odd (and co-prime)
  n.boxes <- prod(b^J) 
  centerOffset <- floor(n.boxes/2)
  m <- floor( runif( 2, 0, maxU()) )*n.boxes + centerOffset
  
  
  J2 <- 1*J
  cycle <- expand.grid(0:J2[1], 0:J2[2])
  samp <- matrix(NA,1+nrow(cycle),2)
  
  samp[1,] <- halton(1, 2, start=m, bases=b)
  if(plot.it) points(samp[1,1], samp[1,2], pch=17, cex=1, col=1)
  
  for( i in 1:nrow(cycle)){
    add.cycle <- prod(b^cycle[i,])
    m2 <- floor( runif( 2, 0, maxU()) )*n.boxes
    m2 <- floor( runif( 1, 0, maxU()))
    samp[i+1,] <- halton(1, 2, m+add.cycle*m2, bases=b)
    if(plot.it) points(samp[i+1,1], samp[i+1,2], pch=16, cex=2, col=1)
    #print(unlist(c(cycle[i,], add.cycle)))
    #readline(">>")
  }
  
  samp
}


# J = c(2,1)
# b <- c(3,5)
# 
# samp1 <- cyclicHal(J,b)
