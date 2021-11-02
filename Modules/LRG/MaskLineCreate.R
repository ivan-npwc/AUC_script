    library(sp)
	
	
	 labelInput
     Species
	 
	  date1=   substr(basename(labelInput),1,15)
	  
	  MesurDir=paste0(labelInput,"\\Polygons\\Animal_measurements")
	  MesurPTH=list.files(MesurDir, full.names=T, pattern="shp")
	  MsrD=paste0(labelInput,"\\Predict\\",Species,"_Measurements")
	  ImageDir=paste0(MsrD,"\\Image")
	  MskDir=paste0(MsrD,"\\Mask"); dir.create(MskDir,showWarnings = F)
	  pthTable=paste0(labelInput,"\\", date1,"_table.csv")
      pthtblB=paste0(MsrD,"\\", Species,"_BlobMTable.csv")
	  crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
	  
	  
	  
	  table0=read.csv(pthTable)
	  lns=shapefile(MesurPTH)
	  tblImage=read.csv(pthtblB)
	  listImg=unique(tblImage$name)
	  
     proj4string(lns) <- crs
    table0$link=paste0(table0$date,"_",table0$link)
	table1= table0[table0$link %in% listImg,]
	  ##############################
	NSdif50=(table1$north-table1$south)/2 
    table1$north50=table1$north+NSdif50  
    SNdif50=(table1$north-table1$south)/2
    table1$south50=table1$south-SNdif50 
    WEdiff50= (table1$east-table1$west)/2
    table1$west50=table1$west-WEdiff50 
    EWdiff50= (table1$east-table1$west)/2 
    table1$east50=table1$east+EWdiff50 
  table2= data.frame(link=table1$link,west=table1$west, east=table1$east,south=table1$south, north=table1$north,  north50=  table1$north50, south50= table1$south50,west50= table1$west50, east50= table1$east50,date=table1$date)
  #########################################

 # cl <- makePSOCKcluster(detectCores (logical=F)) 
 # clusterEvalQ(cl, {
 #   library(sp)
 #   library(spatialEco)
 #   library(raster)	
 #   library(magick)
 #   library(EBImage)	
 # })
 # registerDoParallel(cl)

  # foreach(y = 1:length(tblImage$NewImgPth)) %dopar% {
   for (y in 1:length(listImg)) {
	 
    img=  listImg[y] 
    selectRow=table2[table2$link==img,]
	
	 xlim <<-unique(c(selectRow$west50,selectRow$east50))
     ylim <<-unique(c(selectRow$south50,selectRow$north50))
	
	
    Poligon <- as(raster::extent(selectRow$west50,selectRow$east50,selectRow$south50,selectRow$north50), "SpatialPolygons")
    proj4string(Poligon) <- crs
	pts = lns[!is.na(over(lns,as(Poligon,"SpatialPolygons"))),]
	
	 fig <- image_graph(width = 1084, height = 1084, res =720)
     par(mai=c(0,0,0,0),bg=NA,fig=c(0,1,0,1),bty ="n")
       plot(pts,xlim=xlim,ylim=ylim,
       lwd=3,pch=16,cex=4,bg=16,axes=F, ann=F, xaxt='n', yaxt='n')
	   dev.off()
       fig=image_crop(fig,"1024x1024+30+30+30+30")
	   
	   listMsk=basename(tblImage$NewImgPth[tblImage$name==img])
	   
	   for (u in 1:length(listMsk)) {
	    pathImgSave=paste0(MskDir,"\\",listMsk[u])
	    pathImgSave=gsub("jpg","png",pathImgSave)
        image_write(fig, path = pathImgSave, format = "png")   
       
	}
    }
  ###########################################
  listImg=list.files(ImageDir, full.names=T)
  listMsk=list.files(MskDir, full.names=T)
  

	 for (i in 1:length(listImg)) {
    img=readImage(listImg[i])
	msk=readImage(listMsk[i])
    msk[msk==0]=1
	msk[msk< 1]=0
    msk[img[,,1]==0]=0
	writeImage(msk,listMsk[i])

}	 
	  
	  

	  