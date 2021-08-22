source("Modules/KMLwrite_function.r")

AgePostPredict=function (labelInput1=labelInput,
                         Species1=Species,
					     errorProcessing=F){

  
      library(spatialEco)
      library(sp)
      library(spatialEco)
      library(raster)
      library(dplyr)
      library(geosphere)
   
     Rookery_polygonPTH=paste0(labelInput1,"\\Polygons\\Rookery\\Rookery.shp")
     kmlPathSave=paste0(labelInput1,"\\Predict\\",basename(labelInput1),"_", Species1,"_AgeLatLon.kml")
	 PTH_TableGeoAge=paste0(labelInput1,"\\Predict\\",basename(labelInput1),"_", Species1,"_AgeLatLon.csv")
	 
	 ProbAge_PTH=paste0(labelInput1,"\\Predict\\",basename(labelInput1),"_", Species1,"_ProbAge.csv")


 TableGeoAge1= read.csv(PTH_TableGeoAge)
 TableGeoAge1=TableGeoAge1[is.na(TableGeoAge1$lat)==F,]
 ProbAge=read.csv(ProbAge_PTH)
#################################################################################
  if (file.exists(Rookery_polygonPTH)==T) {
  
   coords <- data.frame(lat= TableGeoAge1$lon, lon=TableGeoAge1$lat)   
    data   <- data.frame(age= TableGeoAge1$age)   # data
    crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string = crs)
	 Rookery_polygon=shapefile(Rookery_polygonPTH)
     proj4string(Rookery_polygon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"	
     PointsRookery <- point.in.poly(Points,Rookery_polygon)
	 PointsRookery1 <<- as.data.frame(PointsRookery)
	 
	 PointsRookery2=NULL
	  for (y in 1:length (PointsRookery1$age)) {
        row1=PointsRookery1[y,]
		row1$CorrectAge=row1$age
	      if (is.na(row1$FID)==F) {                      # it is mean the point insite rookery polygons
	         if (row1$age=="An")   {row1$CorrectAge= "TF"}
	         if (row1$age=="Sa")   {row1$CorrectAge= "F"}
	 }
         if (is.na(row1$FID)==T) {                      # it is mean the point insite haulout polygons
	         if (row1$age=="F")   {row1$CorrectAge= "Sa"}
	 }
	 PointsRookery2=rbind(PointsRookery2,row1)
	 
	 }
	 
	 PointsRookery3 <<- data.frame(lon=PointsRookery2$coords.x1,lat=PointsRookery2$coords.x2,age= PointsRookery2$CorrectAge)
	 
  KMLwrite(Img3=PointsRookery3,kmlPathSave)	 
#################################################################################ERROR PROCESING
  if (errorProcessing==T) {
      ProbAge1=ProbAge[ProbAge$preds < 0.90,]
	  dirTo=paste0(labelInput1,"\\Error"); if (dir.exists(dirTo)==F){ dir.create(dirTo)}
	  dirTo=paste0(dirTo,"\\AgeError"); if (dir.exists(dirTo)==F){unlink(dirTo); dir.create(dirTo)}
          for (i in 1:length(ProbAge1$preds)) {
          from=paste0(ProbAge1$imgPth[i])
          FolderTo=paste0(dirTo,"\\",ProbAge1$name[i])
          if (dir.exists(FolderTo)==F) {dir.create(FolderTo)}
          to=paste0(FolderTo,"\\",basename(from))
          file.copy(from,to)
}
}
}
}
################################
AgePostPredict()