
                  
                    labelInput
					date1=substr(basename(labelInput),1,15)
                    Species= "NFSPup256"
          
					PthTable=paste0(labelInput,"\\", basename(date1), ".csv")
					readPth=paste0(labelInput, "\\Predict\\", Species,"_BlobTable_",basename(date1), ".csv" )
					 crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
					savePth<<-paste0(labelInput, "\\Predict\\", Species,"_BlobTable_GEO_",basename(date1), ".csv" )
					DistanceCalculate = F
					
					
					
		 PTHweight = paste0(System_data,"\\weights\\NFSpup\\",listValue$NFS_Pup_weight_pth)
          kmlPathSave1=paste0(labelInput,"\\Predict\\","#",basename(PTHweight),"#",date1,".kml")
        if (file.exists(kmlPathSave1)==F){
					
					
  result_geo=NULL

  BlobTable<-read.csv(readPth)
#  BlobTable=BlobTable[BlobTable$m.cx != 0,]
   
  dimModel=256
  indexImgSize=   1024/dimModel
  IndexBorderLeft=0.25*dimModel
  IndexBorderRight=0.75*dimModel
  
  tableBlob1=read.csv(PthTable)
#  tableBlob1$link=paste0(tableBlob1$date, "_", tableBlob1$link)
  
  BlobTable=BlobTable[BlobTable$m.cx >= IndexBorderLeft,]
  BlobTable= BlobTable[BlobTable$m.cx <= IndexBorderRight,] 
  # filter only central part
  BlobTable=BlobTable[BlobTable$m.cy >= IndexBorderLeft,] 
  BlobTable= BlobTable[BlobTable$m.cy <= IndexBorderRight,]
  
  BlobTable$m.cx=(BlobTable$m.cx*indexImgSize) #  from 256*256 dim to 1024*1024 dim
  BlobTable$m.cy=(BlobTable$m.cy*indexImgSize) 
  tmp_m.cx=BlobTable$m.cx
  tmp_m.cy=BlobTable$m.cy
  BlobTable$m.cx=tmp_m.cy
  BlobTable$m.cy=tmp_m.cx

  BlobTable<<-BlobTable
  BloblistImg<<-unique(BlobTable$img)
  
  options(warn=-1)
  for (j in 1:length(BloblistImg)) { 
    imgN<-paste0(BloblistImg[j])

    BlobImgTable<-BlobTable[BlobTable$img==imgN,]

    refTable<-tableBlob1[tableBlob1$imgName==imgN,]
	
    NSdifStep=((refTable$north-refTable$south)*2)/1024 
    WEdifStep= ((refTable$east-refTable$west)*2)/1024
	
    NSdif50=(refTable$north-refTable$south)/2 
    refTable$north50=refTable$north+NSdif50  
    SNdif50=(refTable$north-refTable$south)/2
    refTable$south50=refTable$south-SNdif50 
    WEdiff50= (refTable$east-refTable$west)/2
    refTable$west50=refTable$west-WEdiff50 
    EWdiff50= (refTable$east-refTable$west)/2 
    refTable$east50=refTable$east+EWdiff50 
    Lonlim=c(refTable$west50,refTable$east50)
    Latlim=c(refTable$south50,refTable$north50)
    BlobImgTable$lat=((1024-BlobImgTable$m.cy)*NSdifStep)+refTable$south50 
    BlobImgTable$lon=(BlobImgTable$m.cx*WEdifStep)+refTable$west50
   
   Points <- SpatialPointsDataFrame (data.frame(BlobImgTable$lon,BlobImgTable$lat), data.frame(BlobImgTable))
    proj4string(Points) <- crs
	Points =spTransform(Points,CRS(crs))
    BlobImgTable<-as.data.frame(Points)   
      result_geo<-rbind(result_geo,BlobImgTable)
    }
	
options(warn=0)	
	

    result_geo1 = data.frame(img=result_geo$img, lat=result_geo$lat, lon=result_geo$lon,s.radius.max=result_geo$s.radius.max,s.area=result_geo$s.area)
    write.csv(result_geo1, savePth,row.names = F)
##################################################################
if (DistanceCalculate==T) {
  listImg=unique(result_geo1$img)
  result_geo2=NULL
  for (z in 1:length(listImg)) {
    img=listImg[z]
    PointsForImg=result_geo1[result_geo1$img==img,]
    x <- PointsForImg$lon  
    y <- PointsForImg$lat
 
    xy <- SpatialPointsDataFrame(
      matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
      proj4string=CRS(crs))
	 xy =spTransform(xy,CRS(crs)) 
    mdist <- distm(xy)
	mdist1=data.frame(mdist)
	
    
    for (y in 1:length(PointsForImg[,1])){
      x1=mdist1[,y]
      x1=x1[order(x1)]
      x1=x1[2:length(x1)]
      if (length(x1)<3) {ThreeDist=mean(x1)
      } else {ThreeDist=mean(x1[1:3])}
      PointsForImg$ThreeDist[y]=ThreeDist  #aeverege distance from the animal to 3 other near animals  
      PointsForImg$MinDist[y]=min(x1)      #min distance from the animal to 1 other near animal
      PointsForImg$Count[y]=length(x1)+1    # count animals on the foto
    }
    result_geo2=rbind(result_geo2,PointsForImg)
  }
  result_geo2$DimModel=dimModel
  write.csv(result_geo2, savePth,row.names = F)
  }

##############################################################################  
}
	 
	 
	 