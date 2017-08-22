require(sp)
require(rgeos)
require(ggplot2)

#create a point
pt<- SpatialPoints(coords = matrix(c(0,0), ncol = 2))
plot(pt)
ptBuff<- gBuffer(pt, width = 10)
plot(ptBuff)
plot(pt, add=T)

#nubmer of random samples
n <- 10
#create a distance function
getDist<- function(inPt, myPts, minDist = 2){
  #test distance between new point and eisting point
  dist<- gDistance(inPt, myPts, byid = TRUE)
  return(min(dist[,1]) >= minDist)
}


rpts<- data.frame()
#create random sample for a single plot
for(i in 1:n){
  if(nrow(rpts) > 0){
    rpts<- 
  }
  else{
    p<- spsample(ptBuff, n = 1, type = "random")
    rpts<- 
  }
}

#create random samples within the circle
rPrts<- spsample(ptBuff, n = 10, type = "random")
#plot(rPrts, add = TRUE, col = "red")
text(x= coordinates(rPrts)[,1], y= coordinates(rPrts)[,2], 1:10)
lines(x = c(0,0), y=c(0,-10), lty = 2)
text(x = 0, y = -11, "10m")


#debug stuff
inPt<- pt
myPts<- rPrts
