    library(EBImage)
    library(dplyr)
	library(magick)
	library(tools)
	

    BlobFemaleLimit=600
    labelInput
    Species
	
	
     date1=   substr(basename(labelInput),1,15)
     predsDir=paste0(labelInput,"\\Predict\\Preds")
     pth_resultBlob <- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv") 
	 MsrD=paste0(labelInput,"\\Predict\\",Species,"_Measurements");dir.create(MsrD,showWarnings = F)
	 dirSave=paste0(MsrD,"\\Image");dir.create(dirSave,showWarnings = F)
	 pthtblB=paste0(MsrD,"\\", Species,"_BlobMTable.csv")
	 ImgDir=paste0(labelInput,"\\Predict\\Haulout")
	 
     BlobTable=read.csv(pth_resultBlob)
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
	  mask0=preds[i, , , ]
      img0 <- t(mask0)
	  img0 <- t(img0)
      dim(img0) <- c(dimModel[1], dimModel[2], 1)
      img = getFrame(img0, 1)
      nmask = thresh(img, 18, 18, 0.009)  
      nmask1 <- fillHull(nmask)
      nmask2 = opening(nmask1, makeBrush(7,shape='disc') ) 
	  nmask3 = fillHull(nmask2)	
      nmask4 = bwlabel(nmask3)
	 
	
	   vectorObjects=c(1:max(nmask4))
	   
    for (h in 1:length(vectorObjects)) {
      object <- vectorObjects[!vectorObjects %in% h]
      letter = rmObjects(nmask4, object, reenumerate=FALSE)
	  letter1=dilate(letter, makeBrush(45, shape='diamond'))
	  letter2=resize(letter1,1024,1024)
	  fts = data.frame(computeFeatures.moment(letter2))
	  x=mean(fts$m.cx)
	  y=mean(fts$m.cy)
  if (x>1024*0.25 & x<1024*0.75 & y>1024*0.25 & y<1024*0.75) { 

    imgpth=paste0(ImgDir,"\\",name)
	NewImgPth=paste0(dirSave,"\\",file_path_sans_ext(name),"#",h,".",file_ext(name))
	
    letter2[letter2>0.5]=1
	letter2[letter2<=0.5]=0
   
    img1<<-readImage(imgpth)
	
	img1[letter2==0]=0
	
    writeImage(img1,NewImgPth)
  #  display(img1)
  tblB1=data.frame(NewImgPth,name)
  tblB=rbind(tblB,tblB1)
    }
	}
	}
	}
	#stopCluster(cl)
	}
	
	write.csv(tblB,pthtblB,row.names=F)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	