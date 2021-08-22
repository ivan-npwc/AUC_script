distanceCalculate <- function() {
  result_geo=NULL
  date1=basename(labelInput)
  PthTable=paste0(labelInput,"\\", date1, "_table.csv")
  readPth=paste0(labelInput, "\\Predict\\", sex,"_BlobTable_",date1, ".csv" )
  table1=read.csv(PthTable)
  BlobTable<-read.csv(readPth)
  savePth_PRE=paste0(labelInput,"\\", date1, "_distance_PRE.csv")
  table1$link1=paste0(table1$date, "_", table1$link)
  
  BlobTable$m.cx_orig=BlobTable$m.cx
  BlobTable$m.cy_orig=BlobTable$m.cy 
  BlobTable$m.cx=BlobTable$m.cy_orig
  BlobTable$m.cy=BlobTable$m.cx_orig
  BlobTable$m.cx=(BlobTable$m.cx*4)
  BlobTable$m.cy=(BlobTable$m.cy*4) 
  
  BloblistImg<<-unique(BlobTable$img)
  for (j in 1:length(BloblistImg)) { 
    imgN<-paste0(BloblistImg[j])
    BlobImgTable<-BlobTable[BlobTable$img==imgN,]
    refTable<-table1[table1$link1==imgN,]
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
    proj4string(Points) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    BlobImgTable<-as.data.frame(Points)   
    result_geo<-rbind(result_geo,BlobImgTable)}
  result_geo1<<-data.frame(img=result_geo$img, 
                           lat=result_geo$lat, 
						   lon=result_geo$lon,
						   s.radius.max=result_geo$s.radius.max,
						   s.area=result_geo$s.area,
						   m.cx= result_geo$m.cx,
						   m.cy= result_geo$m.cy)
  write.csv(result_geo1, savePth_PRE,row.names = F)
  #showNotification("Done") 
  
  #########################################################################
  listImg=unique(result_geo1$img)
  result_geo2=NULL
  for (z in 1:length(listImg)) {
    img=listImg[z]
    PointsForImg=result_geo1[result_geo1$img==img,]
    
    
    
    x <- PointsForImg$lon  
    y <- PointsForImg$lat     
    xy <- SpatialPointsDataFrame(
      matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    mdist <- distm(xy)
    mdist1=data.frame(mdist)
    for (y in 1:length(PointsForImg[,1])){
      
      x1=mdist1[,y]
      x1=x1[order(x1)]
      x1=x1[2:length(x1)]
      if (length(x1)<2) {ThreeDist=mean(x1)
      } else {ThreeDist=mean(x1[1:3])}
      PointsForImg$ThreeDist[y]=ThreeDist
    }
    result_geo2=rbind(result_geo2,PointsForImg)
  }}

  
  result_geo2=result_geo2[result_geo2$m.cx >= 64 & result_geo2$m.cx <= 192,] # filter only central part
  result_geo2=result_geo2[result_geo2$m.cy >= 64 & result_geo2$m.cy <= 192,]
  
  
distanceCalculate()