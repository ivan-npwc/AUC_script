
library(parallel)
library(doParallel)
library(foreach)
library(sp)
library(spatialEco)
library(raster)
library(rgdal)
####################################################
  date1=substr(basename(labelInput),1,15)
  date=date1 #substr(date1,1,nchar(date1)-4)
  #############################################################
  ObserverPointDIR=paste0(labelInput,"\\Observer count")
  if(dir.exists(ObserverPointDIR)==F){ObserverPointDIR=paste0(labelInput,"\\Observer_count")}

  
  PATHPoints=list.files(ObserverPointDIR,full.names=T,pattern=".shp|kml")
    if (length(PATHPoints)>1){stop("Only one observer count must be")}
  Points <-readOGR(PATHPoints)   

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
  
  
#   pthPointsTable1=paste0(labelInput,"\\", date,"Points1.csv")
 #  pthPointsTable2=paste0(labelInput,"\\", date,"Points2.csv")
 #  pthPointsTable3=paste0(labelInput,"\\", date,"Points3.csv")
#   pthPointsTable4=paste0(labelInput,"\\", date,"Points4.csv")
#   pthPointsTable5=paste0(labelInput,"\\", date,"Points5.csv")
  
  
  ################################################################################################################################################
 # cl <- makePSOCKcluster(detectCores (logical=F)-1) 
 # clusterEvalQ(cl, {
 #   library(sp)
 #   library(spatialEco)
 #   library(raster)					  
 # })
 # registerDoParallel(cl)
  TableAnimalPoints=NULL
 #TableAnimalPoints <- foreach(y = 1:length(table2[,1]), .combine=rbind) %dopar% {	
 options(warn=-1)
    for (y in 1:length(table2[,1])) {
    selectRow=table2[y,]
    Poligon <- as(raster::extent(selectRow$west50,selectRow$east50,selectRow$south50,selectRow$north50), "SpatialPolygons")
    proj4string(Poligon) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
 
   pts = Points[!is.na(over(Points,as(Poligon,"SpatialPolygons"))),]
   
   
  
    if (length(pts)> 0) {    
	pts1=data.frame(pts)
     imagePoint1=data.frame(sex=  pts1[,2], lat= pts1$coords.x1, lon= pts1$coords.x2)					
      PointTable2=cbind(imagePoint1,selectRow)
	  TableAnimalPoints=rbind(TableAnimalPoints,PointTable2)
    }
	
#	if (y>10000 & y<10002){ print("10001");write.csv(TableAnimalPoints, pthPointsTable1)}
#	if (y>20000 & y<20002){ print("20001");write.csv(TableAnimalPoints, pthPointsTable2)}
#	if (y>30000 & y<30002){ print("30001");write.csv(TableAnimalPoints, pthPointsTable3)}
#	if (y>40000 & y<40002){ print("40001");write.csv(TableAnimalPoints, pthPointsTable4)}
#	if (y>50000 & y<50002){ print("50001");write.csv(TableAnimalPoints, pthPointsTable5)}
	

  }
  
  ##################################
	 options(warn=0)
#  stopCluster(cl)
  write.csv(TableAnimalPoints, pthPointsTable)
  
