
library(parallel)
library(doParallel)
library(foreach)
library(sp)
library(spatialEco)
library(raster)
####################################################
  date=basename(labelInput)
  #############################################################
  ObserverPointDIR=paste0(labelInput,"\\Observer count")
  if(dir.exists(ObserverPointDIR)==F){ObserverPointDIR=paste0(labelInput,"\\Observer_count")}

  PATHPoints=list.files(ObserverPointDIR,full.names=T,pattern=".shp") 
  Points <-shapefile(PATHPoints) 
  Points=spTransform(Points,CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  
  pthTable=paste0(labelInput,"\\", date,"_table.csv")  # i can use only image in predic folder
  pthPointsTable=paste0(labelInput,"\\", date,"Points.csv")
  
  pthHouloutImg=paste0(labelInput,"\\Predict\\Haulout")
  listImgHoul=list.files(pthHouloutImg)

  table0=read.csv(pthTable)
  table0$linlEx=paste0(table0$date,"_",table0$imgName)
  table1=table0[table0$linlEx %in% listImgHoul,]
  
  
  ############################################################
    NSdif50=(table1$north-table1$south)/2 
    table1$north50=table1$north+NSdif50  
    SNdif50=(table1$north-table1$south)/2
    table1$south50=table1$south-SNdif50 
    WEdiff50= (table1$east-table1$west)/2
    table1$west50=table1$west-WEdiff50 
    EWdiff50= (table1$east-table1$west)/2 
    table1$east50=table1$east+EWdiff50 
  ####################################################################################################
  table2= data.frame(link=table1$link,west=table1$west, east=table1$east,south=table1$south, north=table1$north,  north50=  table1$north50, south50= table1$south50,west50= table1$west50, east50= table1$east50,date=table1$date)
  ################################################################################################################################################
  cl <- makePSOCKcluster(detectCores ()-2) 
  clusterEvalQ(cl, {
    library(sp)
    library(spatialEco)
    library(raster)					  
  })
  registerDoParallel(cl)
  TableAnimalPoints=NULL
 TableAnimalPoints <- foreach(y = 1:length(table2[,1]), .combine=rbind) %dopar% {	 
 #   for (y in 1:length(table2[,1])) {
    selectRow=table2[y,]
    Poligon <- as(raster::extent(selectRow$west50,selectRow$east50,selectRow$south50,selectRow$north50), "SpatialPolygons")
    proj4string(Poligon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    ModelIN <- point.in.poly(Points,Poligon) # 
    MaskPoint <-  as.data.frame(ModelIN)
    imagePoint=MaskPoint[is.na(MaskPoint[,3]) == FALSE,]  
	 imagePoint=imagePoint[is.na(imagePoint[,4]) == FALSE,] 
    if (length(imagePoint[,1]) != 0) {                
     imagePoint1=data.frame(sex=  imagePoint$LAYER, lat= imagePoint$coords.x1, lon= imagePoint$coords.x2)					
      PointTable2=cbind(imagePoint1,selectRow)
#	  TableAnimalPoints=rbind(TableAnimalPoints,PointTable2)
    }
  }
  ##################################
  stopCluster(cl)
  write.csv(TableAnimalPoints, pthPointsTable)
  
