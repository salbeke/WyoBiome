# 
# UW cyclic lake sampling script
#

source("cyclicHal.r")

library(SDraw)
library(rgdal)

# some constants
buff.width <- 200  # meters, added to bbox of each lake
J = c(2,1)  # prod(J+1) must equal 6 = samples per anchor
b <- c(3,5)  # Hal bases, must be odd and co-prime
nCycleSamps <- 10  # this draws nCycleSamps * prod(J+1) points.  Hopefully, 6 are out of the lake

set.seed(838383)

snowyLakes <- readOGR(".", "Biome_SnowyLakes")

pts.coords <- NULL

for(j in 1:length(snowyLakes)){

  lake <- snowyLakes[j,]
  lake.line <-as(lake,"SpatialLinesDataFrame")
  
  bb <- bbox(lake) + matrix(c(-1,-1,1,1)*buff.width,2)
  dx <- diff(bb[1,])/2
  dy <- diff(bb[2,])/2
  
  anchors <- sdraw(lake.line,4)

  # start a plot
  plot(lake.line, xlim=bb[1,], ylim=bb[2,])
  abline(h=bb[2,])
  abline(v=bb[1,])
  lake.name <- data.frame(snowyLakes)$GNIS_Nm[j]
  title(main = lake.name)
  points(anchors)
  
  # loop over anchors
  for(anc in 1:length(anchors)){
    
    # The cyclic sample on unit square, centered on (0,0)
    samp <- NULL
    for(i in 1:nCycleSamps){ 
      samp <- rbind(samp, cyclicHal(J,b,plot.it = F) - 0.5)
    }
  
    # Scale and shift cyclic points
    pts <- t(t(samp)*c(dx,dy) + coordinates(anchors)[anc,])
    
    # Make a spatial object so can clip
    dimnames(pts)<- list(NULL, c("x","y"))
    pts <- SpatialPoints(pts, proj4string = CRS(proj4string(tmp2)))
  
    # clip out lake
    ind <- over(pts,lake)
    ind <- is.na(ind$OBJECTI)
    pts <- pts[ind,]
    
    # back to data frame
    pts.lake <- data.frame(coordinates(pts), pointID=1:length(pts), ancID=anc, anc.x=coordinates(anchors)[anc,1],
                                 anc.y=coordinates(anchors)[anc,2], lake=lake.name)

    pts.coords <- rbind( pts.coords, pts.lake)

    # plot with all the points
    points(pts, pch=14+anc, col=anc)
  }  

  dev.copy(png, filename=paste0(lake.name,".png"), width=7, height=7, units="in", res=300);dev.off(dev.cur()) 
  
}

# write out and plot the first 6 from each anchor

pts.sp <- SpatialPointsDataFrame(pts.coords[,c("x","y")], 
                              data=pts.coords, proj4string = CRS(proj4string(snowyLakes)))
pts.sp.top6 <- pts.sp[pts.sp$pointID<=6,]

write.csv(data.frame(pts.sp), file="UWCyclicLakeSample01.csv", row.names=FALSE)
write.csv(data.frame(pts.sp.top6), file="UWCyclicLakeSampleTop6_01.csv", row.names=FALSE)

# ----- Plot of the top 6
for( j in 1:length(snowyLakes)){
  lake <- snowyLakes[j,]
  bb <- bbox(lake) + matrix(c(-1,-1,1,1)*buff.width,2)
  
  # start a plot
  plot(lake, xlim=bb[1,], ylim=bb[2,])
  lake.name <- data.frame(snowyLakes)$GNIS_Nm[j]
  title(main = lake.name)

  tmp <- pts.sp.top6[pts.sp.top6$lake == lake.name,]
  points(tmp, pch=tmp$ancID+14, col=tmp$ancID+1)

  dev.copy(png, filename=paste0(lake.name,"_Top6.png"), width=7, height=7, units="in", res=300);dev.off(dev.cur()) 
  
}
