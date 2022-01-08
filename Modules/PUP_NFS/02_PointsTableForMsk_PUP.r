
library(parallel)
library(doParallel)
library(foreach)
library(sp)
library(spatialEco)
library(raster)
library(rgdal)
labelInput
####################################################
  date1=substr(basename(labelInput),1,15)
  #############################################################
  ObserverPointDIR=paste0(labelInput,"\\Observer count")
  if(dir.exists(ObserverPointDIR)==F){ObserverPointDIR=paste0(labelInput,"\\Observer_count")}

  PATHPoints=list.files(ObserverPointDIR,full.names=T,pattern=".shp|kml")
  Points <-readOGR(PATHPoints) 
  Points=spTransform(Points,CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  
  pthTable=paste0(labelInput,"\\Predict\\Pup_Img_Ext_",date1,".csv") # i can use only image in predic folder
  pthPointsTable=paste0(labelInput,"\\Predict\\Pup_PointsOnImg_",date1,".csv")
  
  pthHouloutImg=paste0(labelInput, "\\", "Predict", "\\","PUP")
  listImgHoul=list.files(pthHouloutImg)

  table0=read.csv(pthTable)

  table1=table0[table0[,1] %in% listImgHoul,]
  

  ################################################################################################################################################
  cl <- makePSOCKcluster(detectCores (logical=F)-1) 
  clusterEvalQ(cl, {
    library(sp)
    library(spatialEco)
    library(raster)					  
  })
  registerDoParallel(cl)
  TableAnimalPoints=NULL
 TableAnimalPoints <- foreach(y = 1:length(table1[,1]), .combine=rbind) %dopar% {	 
 #   for (y in 1:length(table1[,1])) {
 
    selectRow=table1[y,]
    Poligon <- as(raster::extent(selectRow$west,selectRow$east,selectRow$south,selectRow$north), "SpatialPolygons")
    proj4string(Poligon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
	
	pts = Points[!is.na(over(Points,as(Poligon,"SpatialPolygons"))),]
	
	 
	 
    if (length(pts[,1]) != 0) {                
     imagePoint1=data.frame(sex=  pts$Description, lat= pts$coords.x1, lon= pts$coords.x2)					
      PointTable2=cbind(imagePoint1,selectRow)
	  TableAnimalPoints=rbind(TableAnimalPoints,PointTable2)
    }
  }
  ##################################
  stopCluster(cl)
  write.csv(TableAnimalPoints, pthPointsTable,row.names=F)
  
