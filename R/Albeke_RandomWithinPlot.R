require(sp)
require(rgeos)
require(ggplot2)
require(stringr)

#get functions
source("./R/getDist.r")



#create a point
pt<- SpatialPoints(coords = matrix(c(0,0), ncol = 2))
plot(pt)
ptBuff<- gBuffer(pt, width = 5)
plot(ptBuff)
plot(pt, add=T)

#nubmer of random samples
n <- 10
#minimum distance between locations, in meters
dist<- 2

#create a vector of plot labels
lbl<- paste("B_", str_pad(1:26, 2, pad = "0"), sep="")
#create output for global point storage
outDF<- data.frame()
#loop through each label and create a plot for addition to a datasheet
for(j in 1:length(lbl)){
  rpts<- data.frame()
  #create random sample for a single plot
  for(i in 1:n){
    #testing to determine if points have been previously created
    if(nrow(rpts) > 0){
      #Points exist, create a new random point, repeat until we create a point
      #first create break value
      z<- 0
      repeat{
        p<- spsample(ptBuff, n = 1, type = "random", iter = 10)
        #test if outside of minimum distance
        if(getDist(p, SpatialPoints(coords = rpts[, c("X", "Y")])) == TRUE){
          #store the new random point
          rpts<- rbind(rpts, data.frame(PtID = i,
                                        X = coordinates(p)[1,1],
                                        Y = coordinates(p)[1,2]))
          #set break value
          z<- 1
        }#close if minDist
        #test if distance criteria passes and break out of the repeat
        if(z != 0) break
      }#close repeat
      
    }#close if nrow
    else{
      #First random point, can be anywhere
      p<- spsample(ptBuff, n = 1, type = "random", iter = 10)
      #Create the schema for the remainder of the points
      rpts<- data.frame(PtID = i,
                        X = coordinates(p)[1,1],
                        Y = coordinates(p)[1,2])
    }#close else
  }#close i
  #create the circular plot of subsample locations
  png(filename = paste("D:/Avdata/WyoBiome/FieldSheets/BrooklynLake/plots/Plot_", lbl[j], ".png", sep = ""), pointsize = 30, width = 720, height = 720)
  par(mgp = c(0,2,2), mar = c(0,0,0,0))
  #the circle
  plot(ptBuff)
  #the subplot locations
  text(x= rpts$X, y= rpts$Y, rpts$PtID)
  #radius line
  lines(x = c(0,-4), y=c(0,-4), lty = 2)
  #radius text
  text(x = -4, y = -4, "5m")
  dev.off()
  #now append the random points to a DF so we can store the locations for later use
  outDF<- rbind(outDF, data.frame(rpts, Plot = lbl[j], PNGFile = paste("D:/Avdata/WyoBiome/FieldSheets/BrooklynLake/plots/Plot_", lbl[j], ".png", sep = "")))
}#close j

#Save the locations to a text file
write.table(outDF, file = "D:/Avdata/WyoBiomeFieldSheets/BrooklynLake/plots/BrooklynLake_SubplotPoints.txt", sep = "\t", row.names = FALSE)

