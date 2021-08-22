library(sp)
library(EBImage)
library(raster)

Species="NFSAdult"
labelInput="G:\\NFS_2020\\2020_138_OPP\\20200724_085354"

ImgOcFin=NULL
  date1= basename(labelInput)
  RefTablePTH=paste0(labelInput,"\\", basename(labelInput), "_table.csv")
  ObserverCountDir=paste0(labelInput,"\\Observer count")
  ObserverCountPTH=list.files(ObserverCountDir,full.names=T,pattern="shp")
  Model_polygonDir=paste0(labelInput, "\\Polygons\\Model")
  Model_polygonPTH=list.files(Model_polygonDir,full.names=T,pattern=".shp")
  predsDir=paste0(labelInput,"\\Predict\\Preds")
  listPreds=list.files(predsDir,full.names=T,pattern=Species)  
  RefTable1=read.csv(RefTablePTH)
  RefTable=data.frame(west=RefTable1$west,east=RefTable1$east,south=RefTable1$south,north=RefTable1$north,link=paste0(date1,"_", RefTable1$link))
if (length(Model_polygonPTH)+ length(ObserverCountPTH) !=2 ){stop("Problem with Model polygon or observer count")} 

  ObserverCount=shapefile(ObserverCountPTH)
  Model_polygon=shapefile(Model_polygonPTH)
  PRJ=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  ObserverCount=spTransform(ObserverCount,PRJ)
  Model_polygon=spTransform(Model_polygon,PRJ)
#------------------------------------------ create table exists all imgs with geo info
   CombinationOne=data.frame(lat= RefTable$west, lon=RefTable$south,link= RefTable$link)
   CombinationTo=data.frame(lat= RefTable$west, lon=RefTable$north,link= RefTable$link)
   CombinationThry=data.frame(lat= RefTable$east, lon=RefTable$south,link= RefTable$link)
   CombinationFor=data.frame(lat= RefTable$east, lon=RefTable$south,link= RefTable$link)
   PointsPorderImage=rbind(CombinationOne,CombinationTo,CombinationThry,CombinationFor)
    coords <- data.frame(lat= PointsPorderImage$lat, lon=PointsPorderImage$lon)   
    data   <- data.frame(link= PointsPorderImage$link)   # data
    PointsImg <- SpatialPointsDataFrame(coords = coords,data = data, proj4string = PRJ)
         vectorSequence=Model_polygon$HEXID
         ImgsInModelPol=NULL
              for (p in 1: length(vectorSequence)) {
               Model_polygon_one=Model_polygon[Model_polygon$HEXID == vectorSequence[p],]
               ImgInPol <- point.in.poly(PointsImg,Model_polygon_one) 
               ImgInPol1=as.data.frame(ImgInPol)
               ImgInPol2=ImgInPol1[is.na(ImgInPol1[,2]) == FALSE,]
               ImgInPol3=data.frame(link=unique(ImgInPol2$link))
               ImgsInModelPol=rbind(ImgInPol3,ImgsInModelPol)
  } 
#------------------------------------------------
  for (f in 1:length(listPreds)) {
       Species=strsplit(basename(listPreds[f]),split = "_")[[1]][2]
       pth_resultBlob <<- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv") 
       pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\",Species, "_BlobTable_", date1, "_tmp.csv")
       PredsRDS=readRDS(listPreds[f])
       listImageBl=PredsRDS$listImageBl
       preds=PredsRDS$preds
      dim(preds)=c(PredsRDS$DimPreds)
#------------------------------------------------------------
#resultBlob_tmp=NULL
# i=4
#resultBlob_tmp <- foreach(i = 1:length(listImageBl),.combine=rbind) %dopar% {
   for (i in 1: length (listImageBl)) {
     name=basename(listImageBl[i])
	  if (name %in% ImgsInModelPol$link) {
	 RefSort=RefTable[RefTable$link==name,]
     img_pth=listImageBl[i]
     mask0=preds[i, , , ]
	 mask0=as.Image(mask0)
	  img=resize(mask0,1024,1024) 
       nmask = thresh(img, 18, 18, 0.009)  
       nmask1 <- fillHull(nmask)
      # nmask2 = opening(nmask1, makeBrush(17,shape='diamond') ) # shape='Gaussian', sigma=50
	   eierode = erode(nmask1, makeBrush(11, shape='diamond'))
	   eidilat = dilate(eierode, makeBrush(13, shape='diamond'))
	   nmask3 = fillHull(eidilat)	   
       nmask4 = bwlabel(nmask3)
	   max(nmask4)
  if (max(nmask4)!=0) {   
		   oc=ocontour(nmask4)
#####################################################################		   
ImgOc=NULL
for (o in 1: length(oc)){
	            pre=data.frame(x=as.numeric(oc[o][[1]][,1]),y=as.numeric(oc[o][[1]][,2]),id=paste0(name,"#",o),img=name, blob=o)
	            ImgOc=rbind(pre,ImgOc)}	   
#		 origImgPTH= paste0(labelInput,"\\Predict\\Haulout\\",name) 
#		  OrigImg=readImage(origImgPTH)
#		  plot(OrigImg)		  
#		for (y in 1:length(oc)) {points(oc[[y]], col=2,cex=0.1)}		
#-------------------------------------------------------------
    NSdifStep=((RefSort$north-RefSort$south)*2)/1024 
    WEdifStep= ((RefSort$east-RefSort$west)*2)/1024
    NSdif50=(RefSort$north-RefSort$south)/2 
    RefSort$north50=RefSort$north+NSdif50  
    SNdif50=(RefSort$north-RefSort$south)/2
    RefSort$south50=RefSort$south-SNdif50 
    WEdiff50= (RefSort$east-RefSort$west)/2
    RefSort$west50=RefSort$west-WEdiff50 
    EWdiff50= (RefSort$east-RefSort$west)/2 
    RefSort$east50=RefSort$east+EWdiff50 
    Lonlim=c(RefSort$west50,RefSort$east50)
    Latlim=c(RefSort$south50,RefSort$north50)
#------------------------------------------------------------
    ImgOc$lon=((1024-ImgOc$y)*NSdifStep)+RefSort$south50 
    ImgOc$lat=(ImgOc$x*WEdifStep)+RefSort$west50
	ImgOcFin=rbind(ImgOcFin,ImgOc)
print(paste0("IMGS ",i, "/", length(listImageBl), "  PREDS  ",f,"/",length(listPreds)  ))	
}
}
}
}


############################################################	
coordinates(ImgOcFin) <- ~ lat+lon
    srPolygons=list()
	srPolygonsData=list()
	ListPol=unique(ImgOcFin$id)
for(s in 1:length(ListPol)){
srPolygons[[s]]=Polygons(list(Polygon(ImgOcFin[ImgOcFin$id==ListPol[s],])),paste0(ListPol[s]))
#srPolygonsData[[s]]=data.frame(img=ImgOcFin$img[s],blob=ImgOcFin$blob[s])
print (paste0("Processing ", s,"/",length(ListPol), " blobs"))}

SpP=SpatialPolygons(srPolygons);proj4string(SpP) <-PRJ
#srPolygonsData=as.data.frame(do.call(abind, c(srPolygonsData, list(along = 1))))
#row.names(srPolygonsData)=paste0(srPolygonsData$img,"#",srPolygonsData$blob)
#PolygonsData=data.frame(img=unique(ImgOcFin$img))
#SpPDF=SpatialPolygonsDataFrame(SpP,srPolygonsData)
#-------------------------------------------------------------------
for (h in 1:length(ImgOcFin$img)) {
   imgName=as.character(ImgOcFin$img[h])
  TableExten= RefTable[RefTable$link==imgName,]
  TableExten1=NULL
TableExten1$lat[1]=TableExten$west;TableExten1$lon[1]=TableExten$south
TableExten1$lat[2]=TableExten$west;TableExten1$lon[2]=TableExten$north
TableExten1$lat[3]=TableExten$east;TableExten1$lon[3]=TableExten$north
TableExten1$lat[4]=TableExten$east;TableExten1$lon[4]=TableExten$south
TableExten1=data.frame(TableExten1);TableExten1$img=imgName
coordinates(TableExten1) <- ~ lat+lon
ImgPol=Polygon(TableExten1)

ImgPol1=SpatialPolygons(list(ImgPol),imgName)
proj4string(ImgPol1) <-PRJ



}
















#--------------------------------------------------------------------------------------------------------------
	srPolygons=list()
	srPolygonsData=list()
	for(i in 1:length(out3[,1]))
	{
		srPolygons[[i]]=Polygons(list(Polygon(cbind(as.numeric(c(out3[i,1],out3[i,1],out3[i,2],out3[i,2])),as.numeric(c(out3[i,3],out3[i,4],out3[i,4],out3[i,3]))))),paste0(out3[i,6],"/",out3[i,5]))
		srPolygonsData[[i]]=data.frame(dx=abs(as.numeric(out3[i,2])-as.numeric(out3[i,1])),dy=abs(as.numeric(out3[i,4])-as.numeric(out3[i,3])))
	}

	SpP=SpatialPolygons(srPolygons)
	srPolygonsData=do.call(abind, c(srPolygonsData, list(along = 1)))

	rows=rownames(sp::coordinates(SpP))
	rownames(srPolygonsData)=rows
	srPolygonsData=as.data.frame(srPolygonsData)

	data1=as.data.frame(rownames(srPolygonsData))
	rownames(data1)=rows
	SpP_data=SpatialPolygonsDataFrame(SpP,data1)

	SpP_data=subset(SpP_data,srPolygonsData[,1]<=quantile(srPolygonsData[,1],0.975))
	
	proj4string(SpP_data) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
	SpP_data=spTransform(SpP_data,CRS(crs))
	SpP_data
	}