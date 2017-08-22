require(sp)
require(rgdal)

setwd("D:/Avdata/WyoBiome")
#get the sample locations from Trent
pts<- read.csv("UWCyclicLakeSample01.csv")

top6<- read.csv("UWCyclicLakeSampleTop6_01.csv")
gcsWGS84<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#get the lakes
lake<- readOGR(dsn = getwd(), layer = "Biome_SnowyLakes")

#make pts spatial
pts<- SpatialPointsDataFrame(coords = pts[,1:2], data = pts, proj4string = CRS(proj4string(lake)))
top6<- SpatialPointsDataFrame(coords = top6[,1:2], data = top6, proj4string = CRS(proj4string(lake)))
#save as shape
writeOGR(pts, dsn = getwd(), layer = "Biome_SnowyLakes_Allpts", driver = "ESRI Shapefile")
writeOGR(top6, dsn = getwd(), layer = "Biome_SnowyLakes_Top6pts", driver = "ESRI Shapefile")

top6.ll<- spTransform(top6, gcsWGS84)
pts.ll<- spTransform(pts, gcsWGS84)
writeOGR(top6.ll, dsn = "D:/Avdata/WyoBiome/SnowyLakesTop6.gpx", layer = "waypoints", dataset_options="GPX_USE_EXTENSIONS=yes", driver="GPX", overwrite_layer = T)
writeOGR(pts.ll, dsn = "D:/Avdata/WyoBiome/SnowyLakesAll.gpx", layer = "waypoints", dataset_options="GPX_USE_EXTENSIONS=yes", driver="GPX", overwrite_layer = T)

dat<- data.frame(top6.ll)
dat$x<- round(dat$x.2,5)
dat$y<- round(dat$y.2,5)
dat<- dat[,1:7]
write.table(dat, "SnowyLakesTop6.txt", row.names = FALSE, sep = "\t")
