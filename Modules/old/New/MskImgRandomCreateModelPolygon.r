
library(parallel)
library(doParallel)
library(foreach)
library(sp)
library(spatialEco)
library(raster)
library(magick)   
date=basename(labelInput)

#TablePoints=read.csv(paste0(labelInput, "\\",date, "Points.csv"))
ObserverCountPth=paste0( labelInput, "\\Observer count\\",date,".shp")
PthModelPolygon=paste0(labelInput, "\\Polygons\\Model\\Model.shp")
TableImgPTH=paste0(labelInput,"\\",date,"_table.csv")

ObserverCount = shapefile(ObserverCountPth) 
ModelPolygon=shapefile(PthModelPolygon)
TableImg=read.csv(TableImgPTH)

ObserverCount=sp::spTransform(ObserverCount,CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))  #+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
ModelPolygon=sp::spTransform(ModelPolygon,CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

PolOverPoins=over(ObserverCount,ModelPolygon) 
PolOverPoins1=unique(PolOverPoins$id)

MdlPlWthAnml=  ModelPolygon[ModelPolygon$id==PolOverPoins1,] # Model polygons with animals



   ImgBoundary=NULL
   
    for (i in 1:length(TableImg$link)) {
	   row1=TableImg[i,]
      first=data.frame(lat=row1$west,lon=row1$south)
	  second=data.frame(lat=row1$west,lon=row1$north)
	  third=data.frame(lat=row1$east,lon=row1$north)
	  forth=data.frame(lat=row1$east,lon=row1$south)
	  PreFin=rbind(first,second,third,forth)
	  PreFin$link=row1$link
	  ImgBoundary=rbind(PreFin,ImgBoundary)
}

     coords <- data.frame(lat= ImgBoundary$lat, lon=ImgBoundary$lon)   # coordinates
     data   <- data.frame(link= ImgBoundary$link)   # data
     crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")  #+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
     ImgsBorder <- SpatialPointsDataFrame(coords = coords,
                                   data = data, 
                                   proj4string = crs)

   ImgsOverPlgs=over(ImgsBorder,MdlPlWthAnml)
   ImgBoundary$id=ImgsOverPlgs$id
   ImgForMdlPlgn=ImgBoundary[is.na(ImgBoundary$id)==F,]
   ImgForMdlPlgn=data.frame(link=ImgForMdlPlgn$link, id=ImgForMdlPlgn$id)
   ImgForMdlPlgn1= merge(ImgForMdlPlgn,TableImg,by="link",all.x = T)
     





	 
	  Nplgns=unique(ImgForMdlPlgn1$id)
	  
	  
      for (i in 1:length(Nplgns)) {   
       ImgsForPol=  ImgForMdlPlgn1[ImgForMdlPlgn1$id== Nplgns[i],]
       TMPImgsForPol=ImgsForPol
	   
	   
	   
          while (length(TMPImgsForPol$link)<1) {
   
 
           link =   paste0(TMPImgsForPol$link[1])
           nearRow= TableImg[TableImg$link==link,]
     
	      leftName=data.frame(Direction="leftName",link=nearRow$leftName)
	      upName=data.frame(Direction="upName",link=nearRow$upName)
	      rightName=data.frame(Direction="rightName",link=nearRow$rightName)
	      downName=data.frame(Direction="downName",link=nearRow$downName)
          finNearTmp=rbind(leftName,	upName,rightName,downName) 
	 
	     needAdd= finNearTmp$link[finNearTmp$link %in% ImgsForPol$link][1]
	     needAdd1=finNearTmp[finNearTmp$link==needAdd,]
		   if (ImgsForPol$west[1] <0) {
		   westIMgs=  ImgsForPol$link[  min(ImgsForPol$west)
		   southLon=min(ImgsForPol$south)
		   
		   }
		   
		   
		   
		 NEED TO FINDE UPER BOTTON LOOP
   
   link
   needAdd1

   
   
}   }
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   




TableImg=TableImg[TableImg$All512>0,]
imgList=unique(TableImg$link)
imgList=imgList[is.na(imgList)==F]
TablePoints=TablePoints[TablePoints$link %in% imgList,]
   SortTable=data.frame(link=TablePoints$link, west=TablePoints$west,east=TablePoints$east, south=TablePoints$south,north=TablePoints$north)
   SortTable=unique(SortTable)
   
   ImgBoundary=NULL
    for (i in 1:length(SortTable$link)) {
	   row1=SortTable[i,]
      first=data.frame(lat=row1$west,lon=row1$south)
	  second=data.frame(lat=row1$west,lon=row1$north)
	  third=data.frame(lat=row1$east,lon=row1$north)
	  forth=data.frame(lat=row1$east,lon=row1$south)
	  PreFin=rbind(first,second,third,forth)
	  PreFin$link=row1$link
	  ImgBoundary=rbind(PreFin,ImgBoundary)
}
###########################################
#need to sort images for model polygons,  what is sort... what use for sort ....
#then combine them for each polygons into big image
# create big mask
# random crop img-msk
#also need create table near e level.....
  coords <- data.frame(lat= ImgBoundary$lat, lon=ImgBoundary$lon)   # coordinates
  data   <- data.frame(link= ImgBoundary$link)   # data
  crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")  #+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
  ImgsBorder <- SpatialPointsDataFrame(coords = coords,
                                   data = data, 
                                   proj4string = crs)

								   
	Polyg=	unique(ModelPolygon$id)	
	SortImgForModel3=NULL
	
	  for (i in 1: length(Polyg)){
	  
	Pol = ModelPolygon[ModelPolygon$id==	Polyg[i],]	
	
       SortImgForModel <- point.in.poly(ImgsBorder,Pol)
       SortImgForModel1=data.frame(SortImgForModel)
	   SortImgForModel2=SortImgForModel1[is.na(SortImgForModel1$left)==F,]
	     if (length(SortImgForModel2$left)>0) {SortImgForModel3=rbind(SortImgForModel2,SortImgForModel3)}		 
}






############################
SortImgForModel4=data.frame(link=SortImgForModel3$link, id=SortImgForModel3$id)
NunberPol=unique(SortImgForModel4$id)


  for (i in 1: length(NunberPol)) {
   Polyg=  NunberPol[i]
   SortPol=  SortImgForModel4[SortImgForModel4$id==Polyg,]    # list img for curent polygon
   SortPol1=SortPol
   
 #     StartImg=paste0(SortPol1$link[1])
 #     NearRef=TableNear[TableNear$link==StartImg,]
#	  pthBase=paste0(BaseTilesPth, NearRef$fold,"\\",basename(as.character(NearRef$fLevelImgPath)))
#	  StartImg1=image_read(pthBase)
	  
    while (length(SortPol1$link)<1) {
    
   row1=SortPol1[1,]
   img=   paste0(row1$link)
   nearRow= TableNear[TableNear$link==img,]
     
	 leftName=data.frame(Direction="leftName",link=nearRow$leftName)
	 upName=data.frame(Direction="upName",link=nearRow$upName)
	 rightName=data.frame(Direction="rightName",link=nearRow$rightName)
	 downName=data.frame(Direction="downName",link=nearRow$downName)
     finNearTmp=rbind(leftName,	upName,rightName,downName) 
	 
	needAdd= finNearTmp$link[finNearTmp$link %in% SortPol$link][1]
	 needAdd1=finNearTmp[finNearTmp$link==needAdd,]
     
	  NearRef=TableNear[TableNear$link==paste0(needAdd1$link),]
	  pthNear=paste0(BaseTilesPth, NearRef$fold,"\\",basename(as.character(NearRef$fLevelImgPath)))
	  ImgNear=image_read(pthNear)
	  
	  if (needAdd1$Direction=="leftName") {
       TempImg=image_append(c(ImgNear,StartImg1))


}
}
}
}
########################################################################################################
TableImg=TableImg[TableImg$All512>0,]
imgList=unique(TableImg$link)
imgList=imgList[is.na(imgList)==F]

cl <- makePSOCKcluster(4) 
clusterEvalQ(cl, {	
  library(magick)
})
registerDoParallel(cl)
foreach(i = 1:length(imgList)) %dopar% {	
#    for (i in 1:length(imgList)) {
  img=imgList[i]
  points=  data.frame(TablePoints[TablePoints$link==img,])
  lat=points$lat
  lon=points$lon
  
  xlim<<-unique(c(points$west50,points$east50))
  ylim<<-unique(c(points$south50,points$north50))
  
  cex=  paste0(points$sex)
  cex=  gsub("An", "1.5", cex) 
  cex=  gsub("TN", "1.5", cex) 
  cex=  gsub("TF", "1.5", cex) 
  cex=  gsub("Sa", "0.9", cex)
  cex=  gsub("SA", "0.9", cex)
  cex=  gsub("AF",  "0.7", cex)
  cex=  gsub("F",  "0.7", cex) 
  cex=  gsub("J",  "0.7", cex) 	
  cex=  gsub("U",  "0.6", cex)
  cex=  gsub("P",  "0.25", cex)
  cex=as.numeric(cex)
  ###################
  fig <- image_graph(width = 1084, height = 1084, res =720)
  par(mai=c(0,0,0,0),bg=NA,fig=c(0,1,0,1),bty ="n") 
  plot(lat,lon,xlim=xlim,ylim=ylim, col="red",
       pch=16,cex=cex,bg=16,axes=F,frame.plot=F, ann=F, xaxt='n', yaxt='n')	 
  dev.off()
  fig=image_crop(fig,"1024x1024+30+30+30+30")
   #imgN=gsub("png","gif", img)
  pathImgSave=paste0(maskDir, "\\", date, "_",img)
  image_write(fig, path = pathImgSave, format = "png")   
}
stopCluster(cl)


