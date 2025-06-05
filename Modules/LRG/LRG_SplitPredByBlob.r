    library(EBImage)
    library(dplyr)
	library(magick)
	library(tools)
	library(sp)
    library(raster)
	library(geosphere)
	

	Type="Predict"   # "Train"
    BlobFemaleLimit=600
    labelInput  #= "D:\\PL_DB\\2021_3101_OPP\\20210725_084310"
    Species     #="LRG"
	
	
     date1 =   substr(basename(labelInput),1,15)
     predsDir =paste0(labelInput,"\\Predict\\Preds")
     pth_resultBlob <- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv") 
	 MsrD =paste0(labelInput,"\\Predict\\",Species,"_Measurements");dir.create(MsrD,showWarnings = F)
	 dirSave =paste0(MsrD,"\\Image");dir.create(dirSave,showWarnings = F)
	 pthtblB =paste0(MsrD,"\\", Species,"_BlobMTable.csv")
	 ImgDir =paste0(labelInput,"\\Predict\\Haulout")
	 pthCoordTable =paste0(labelInput,"\\", date1,"_table.csv")
	 MesurDir=paste0(labelInput,"\\Polygons\\Animal_measurements")
	 MesurPTH =list.files(MesurDir, full.names=T, pattern="shp")
	 tblB=NULL
	 PRJ =CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
	 
     BlobTable =read.csv(pth_resultBlob)
	 CoordTable =read.csv(pthCoordTable); CoordTable$link=paste0(CoordTable$date,"_",CoordTable$link)
	 lns=shapefile(MesurPTH); proj4string(lns) <- PRJ
	 ############################################# sort only central part and sort biger then  BlobFemaleLimit
	   dimModel=BlobTable$DimModel[1]
        indexImgSize=   1024/dimModel
       IndexBorderLeft=0.25*dimModel
       IndexBorderRight=0.75*dimModel
       BlobTable=BlobTable[BlobTable$m.cx >= IndexBorderLeft & BlobTable$m.cx <= IndexBorderRight,] # filter only central part
       BlobTable=BlobTable[BlobTable$m.cy >= IndexBorderLeft & BlobTable$m.cy <= IndexBorderRight,]
	   LstImgWthAnmls=BlobTable$img[BlobTable$s.area>BlobFemaleLimit]
	 ##############################################
     listPreds=list.files(predsDir,full.names=T,pattern=Species) 
 
      
       if (length(listPreds)==0){stop(paste0("No prediction found for ", Species))} 
      
      
 

for (f in 1:length(listPreds)) {
  
   # cl <- makePSOCKcluster(detectCores (logical=F)) 
  #  clusterEvalQ(cl, {library(EBImage)})
  #  registerDoParallel(cl)
	
    PredsRDS=readRDS(listPreds[f])
    listImageBl=PredsRDS$listImageBl
	SpeciesRem=paste0(Species,"_")
	CheckPresence=basename(listImageBl) %in% gsub(SpeciesRem,"", LstImgWthAnmls)
	listImageBl$CheckPresence=CheckPresence
	
	preds=PredsRDS$preds
    dimModel=PredsRDS$dimModel
    dimPreds=PredsRDS$DimPreds
    dim(preds)=c(dimPreds)
	
	
 #########################################
 
  #  foreach(i = 1:length(listImageBl)) %dopar% {
      for (i in 1: length (listImageBl$CheckPresence)) {	
	if(listImageBl$CheckPresence[i]==T) {
	
	 name=basename(as.character(listImageBl[i]))
	 RefSort = CoordTable[CoordTable$link==name,]
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
      mask0=preds[i, , , ]
	  
	   mask0=preds[i, , , ]
      img0 <- t(mask0)
	  img0 <- t(img0)
      dim(img0) <- c(dimModel[1], dimModel[2], 1)
      img = getFrame(img0, 1)
      nmask = thresh(img, 18, 18, 0.009)  
      nmask1 <- fillHull(nmask)
      nmask2 = opening(nmask1, makeBrush(7,shape='disc')) 
	  nmask2 = resize(nmask2,1024,1024)
	  nmask3 = fillHull(nmask2)	
      nmask4 = bwlabel(nmask3)
	  
	    
#------------------------------------------------------------	 
	   vectorObjects=c(1:max(nmask4))
	  
    for (h in 1:length(vectorObjects)) {
      object <- vectorObjects[!vectorObjects %in% h]
      letter = rmObjects(nmask4, object, reenumerate=FALSE)# get only one object

	  ################################# here we shood paste script to finde a length of the seal
	 if( Type=="Train"){
	  
	  Oc =data.frame(ocontour(letter));names(Oc)=c("x","y")# here may be invert needed
	  Oc$lon=((1024- Oc$y)*NSdifStep)+RefSort$south50 
      Oc$lat=(Oc$x*WEdifStep)+RefSort$west50
	  coordinates(Oc) <- ~ lat+lon 
	  
	  srPolygons=list()
	  srPolygons[[1]] = Polygons(list(Polygon(Oc)),1)
	  Poligon =SpatialPolygons(srPolygons);proj4string(Poligon) <-PRJ
	  
	  pts = lns[!is.na(over(lns,as(Poligon,"SpatialPolygons"))),]
  if (length(pts) !=0) {
	  lngth= round(geosphere::lengthLine(pts),digits = 2)*100 # IN SANTIMETERS
	}
	}
	  ########################################################### MAKE THE BLOB BIGER
	  letter1=dilate(letter, makeBrush(41, shape='diamond'))
	  
	  fts = data.frame(computeFeatures.moment(letter1))
	  x=mean(fts$m.cx)
	  y=mean(fts$m.cy)
if (x>1024*0.25 & x<1024*0.75 & y>1024*0.25 & y<1024*0.75) { 

    imgpth=paste0(ImgDir,"\\",name)
if( Type=="Predict"){lngth=NA
                    NewImgPth=paste0(dirSave,"\\",file_path_sans_ext(name),"_",h,".",file_ext(name))}
if( Type=="Train"){NewImgPth=paste0(dirSave,"\\",lngth,"_", file_path_sans_ext(name),"_",h,".",file_ext(name))}	
	
	
    letter1[letter1>0.5]=1
	letter1[letter1<=0.5]=0
   
    img1<<-readImage(imgpth)
	
	img1[letter1==0]=0
	
    writeImage(img1,NewImgPth)
  #  display(img1)
  tblB1=data.frame(basename(NewImgPth),name,lngth)
  tblB=rbind(tblB,tblB1)
    }
	}
	}
	}
    }

	write.csv(tblB,pthtblB,row.names=F)
#########################################################	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	