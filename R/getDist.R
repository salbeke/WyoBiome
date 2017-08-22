#' Get minimum distance between two sets of points for minimum sampling distance
#'
#' @param inPt SpatialPoint to be tested against existing points
#' @param cPts SpatialPoints of existing random locations that meet the minimum distance requirement
#' @param minDist number describing the minimum distance, in meters or other Cartesian map unit between inPt and cPts. Default is 2 m
#' @return Logic value describing if the minimum distance of the input point exceeds distance to existing points
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export

require(rgeos)
#create a distance function
getDist<- function(inPt, cPts, minDist = 2){
  #NEED TO TEST FOR SP AND SAME PRJ and MUST USE METERS
  #test distance between new point and eisting point
  dist<- gDistance(inPt, cPts, byid = TRUE)
  return(min(dist[,1]) >= minDist)
}