###########################################################################################################################
library(sp)
library(spatialEco)
library(raster)
library(dplyr)
library(geosphere)
#########################################################################################################
date=  basename(labelInput)
pthTable=paste0(labelInput,"\\Predict\\", date,"Points.csv")
pthTableNew=paste0(labelInput,"\\Predict\\",date, "_CountDist.csv")

tablePoints= read.csv(pthTable)
tablePoints2<<-data.frame(sex=tablePoints$sex, lat=tablePoints$lat,lon=tablePoints$lon,link=tablePoints$link, west=tablePoints$west,east=tablePoints$east,south=tablePoints$south,north=tablePoints$north, north50=tablePoints$north50,south50=tablePoints$south50,west50=tablePoints$west50,east50=tablePoints$east50,date=tablePoints$date)
imgList=unique(tablePoints2$link)
####################################################################################################################
for (i in 1:length(imgList))  {
  img=paste0(imgList[i])
  listAnimals = tablePoints2[tablePoints2$link== img,] 
  #tablePoints2$F1024=0
  #tablePoints2$F512=0
  Limit512=unique(data.frame(west=listAnimals$west,east=listAnimals$east,south=listAnimals$south,north=listAnimals$north))
  Poligon512 <- as(raster::extent(Limit512$west,Limit512$east,Limit512$south,Limit512$north), "SpatialPolygons")
  proj4string(Poligon512) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  ###########################################################################################
  coords <- data.frame(lat= listAnimals$lat, lon=listAnimals$lon)   # coordinates
  data   <- data.frame(sex= listAnimals$sex)   # data
  crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") # proj4string of coords
  Points <- SpatialPointsDataFrame(coords = coords,
                                   data = data, 
                                   proj4string = crs)
  ###############################################################  FILTER POINTS FOR THE IMAGE
  ModelIN <- point.in.poly(Points,Poligon512) 
  MaskPoint=as.data.frame(ModelIN)
  imagePoint=MaskPoint[is.na(MaskPoint[,2]) == FALSE,]
  imagePoint=data.frame(sex=imagePoint$sex,lat=imagePoint$coords.x1,lon=imagePoint$coords.x2)
  SexList=as.vector(unique(imagePoint$sex))
  if (length(SexList) > 0 ) {
   tablePoints2$All512[tablePoints2$link==imgList[i]]=length(imagePoint$sex)
  }
  #######################################################################################
}
table3=data.frame(date=tablePoints2$date, 
                  link=tablePoints2$link,
				  All512=tablePoints2$All512)
table3=table3[is.na(table3$All512)==F,]				  
table4=unique(table3)
write.csv(table4, pthTableNew)

