# 
# UW cyclic lake sampling script
#

source("./R/cyclicHal.r")

library(SDraw)
library(rgdal)

# some constants
buff.width <- 200  # meters, added to bbox of each lake
J = c(2,1)  # prod(J+1) must equal 6 = samples per anchor
b <- c(3,5)  # Hal bases, must be odd and co-prime
nCycleSamps <- 10  # this draws nCycleSamps * prod(J+1) points.  Hopefully, 6 are out of the lake

set.seed(838383)

snowyLakes <- readOGR("D:/Avdata/WyoBiome", "Biome_SnowyLakes_vGlacier")

pts.coords <- NULL

for(j in 1:nrow(snowyLakes)){
  #grab current lake
  lake <- snowyLakes[j,]
  #convert to lines
  lake.line <- as(lake,"SpatialLinesDataFrame")
  #expand the bounding box by assigned buffer width
  bb <- bbox(lake) + matrix(c(-1,-1,1,1)*buff.width,2)
  dx <- diff(bb[1,])/2
  dy <- diff(bb[2,])/2
  #create 4 random points along the lake boundary
  anchors <- sdraw(lake.line,4)

  # # start a plot
  # plot(lake.line, xlim=bb[1,], ylim=bb[2,])
  # abline(h=bb[2,])
  # abline(v=bb[1,])
  lake.name <- data.frame(snowyLakes)$LakeCode[j]
  # title(main = lake.name)
  # points(anchors)
  # 
  # loop over anchors
  for(anc in 1:nrow(anchors)){
    
    # The cyclic sample on unit square, centered on (0,0)
    samp <- NULL
    for(i in 1:nCycleSamps){ 
      samp <- rbind(samp, cyclicHal(J,b,plot.it = F) - 0.5)
    }
  
    # Scale and shift cyclic points
    pts <- t(t(samp)*c(dx,dy) + coordinates(anchors)[anc,])
    
    # Make a spatial object so can clip
    dimnames(pts)<- list(NULL, c("x","y"))
    pts <- SpatialPoints(pts, proj4string = CRS(proj4string(snowyLakes)))
  
    # clip out lake
    ind <- over(pts,lake)
    ind <- is.na(ind$LakeCode)
    pts <- pts[ind,]
    
    # back to data frame
    pts.lake <- data.frame(coordinates(pts), pointID=1:length(pts), ancID=anc, anc.x=coordinates(anchors)[anc,1],
                                 anc.y=coordinates(anchors)[anc,2], lake=lake.name)

    pts.coords <- rbind( pts.coords, pts.lake)

    # plot with all the points
    points(pts, pch=14+anc, col=anc)
  }#close anc  

  #dev.copy(png, filename=paste0(lake.name,".png"), width=7, height=7, units="in", res=300);dev.off(dev.cur()) 
  
}#close j

#check out Glacier
plot(snowyLakes[3,])
glc<- pts.coords[pts.coords$lake == "GLC",]
glc<- SpatialPointsDataFrame(coords = glc[ c("x", "y")], data = glc, proj4string = CRS(proj4string(snowyLakes)))
plot(glc, add = T, col = glc@data$ancID)

#write out the data.frame
write.table(pts.coords, file = "D:/Avdata/WyoBiome/WyoBiome/data/PlotSampleLocations_GlacierCombo.txt", sep = "\t", row.names = FALSE)


#Read in the table from disk
pts<- read.table("D:/Avdata/WyoBiome/WyoBiome/data/PlotSampleLocations_GlacierCombo.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#now filter for Brooklyn Lake, to be sample 8/24/2017
ptsBrk<- pts[pts$lake == "BRK" & pts$pointID < 13,]
#Make spatially aware
ptsBrk<- SpatialPointsDataFrame(coords = ptsBrk[, c("x", "y")], data = ptsBrk, proj4string = CRS(proj4string(snowyLakes)))
#Create new CRS
gcsWGS84<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#reproject into GCS
ptsBrk<- spTransform(ptsBrk, gcsWGS84)
#create an ID column of anchor and id
ptsBrk@data<- data.frame(name = paste("B", ptsBrk@data$ancID, "_", ptsBrk@data$pointID, sep = ""), ptsBrk@data)
#save as GPX
writeOGR(ptsBrk, dsn = "D:/Avdata/WyoBiome/WyoBiome/data/BrooklynLakePlotPts.gpx", layer = "waypoints", driver = "GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer = TRUE)



#12 pts per anchor, Brooklyn is first
#Barcode= 4 phylosphere, 4 organic, 4 mineral (S for soil, P phylo 0001) "B_P0001" or "B_S0001"
#Edit datasheet to have phylosphere table and barcode, delete from the soil




# # write out and plot the first 6 from each anchor
# 
# pts.sp <- SpatialPointsDataFrame(pts.coords[,c("x","y")], 
#                               data=pts.coords, proj4string = CRS(proj4string(snowyLakes)))
# pts.sp.top6 <- pts.sp[pts.sp$pointID<=6,]
# 
# write.csv(data.frame(pts.sp), file="UWCyclicLakeSample01.csv", row.names=FALSE)
# write.csv(data.frame(pts.sp.top6), file="UWCyclicLakeSampleTop6_01.csv", row.names=FALSE)
# 
# # ----- Plot of the top 6
# for( j in 1:length(snowyLakes)){
#   lake <- snowyLakes[j,]
#   bb <- bbox(lake) + matrix(c(-1,-1,1,1)*buff.width,2)
#   
#   # start a plot
#   plot(lake, xlim=bb[1,], ylim=bb[2,])
#   lake.name <- data.frame(snowyLakes)$GNIS_Nm[j]
#   title(main = lake.name)
# 
#   tmp <- pts.sp.top6[pts.sp.top6$lake == lake.name,]
#   points(tmp, pch=tmp$ancID+14, col=tmp$ancID+1)
# 
#   dev.copy(png, filename=paste0(lake.name,"_Top6.png"), width=7, height=7, units="in", res=300);dev.off(dev.cur()) 
#   
# }
