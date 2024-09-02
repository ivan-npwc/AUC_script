
     library(EBImage)
     Species="NFSAdult"
	 labelInput #"C:\\Users\\usato\\NFS_DB\\2024_138_OPP\\20240613_124327"
	 
     predsDir=paste0(labelInput,"\\Predict\\Preds")
     
	 listPredsNFS=list.files(predsDir,full.names=T,pattern="NFSAdult")  
	 listPredsSSL=list.files(predsDir,full.names=T,pattern="SSLAdult")
	 listPredsPUP=list.files(predsDir,full.names=T,pattern="SSLPup")
	 
     date1=substr(basename(labelInput),1,15)
     resultBlob=NULL 
	 
  for (f in 1:length(listPredsNFS)) {
  
   cl <- makePSOCKcluster(detectCores (logical=F)-1) 
    clusterEvalQ(cl, {library(EBImage)})
    registerDoParallel(cl)
  
  
       Species=strsplit(basename(listPredsNFS[f]),split = "_")[[1]][2]
       
	   pth_resultBlob <<- paste0(labelInput,"\\Predict\\","NFSAdult","_BlobTable_", date1, ".csv") 
       pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\","NFSAdult", "_BlobTable_", date1, "_tmp.csv")
	   
       PredsRDSNFS=readRDS(listPredsNFS[f])
	   PredsRDSSSL=readRDS(listPredsSSL[f])
	   PredsRDSPUP=readRDS(listPredsPUP[f])
	   
       listImageBl_NFS=PredsRDSNFS$listImageBl
	   listImageBl_SSL=PredsRDSSSL$listImageBl
	   listImageBl_PUP=PredsRDSPUP$listImageBl
	   
       predsNFS=PredsRDSNFS$preds
	   predsSSL=PredsRDSSSL$preds
	   predsPUP=PredsRDSPUP$preds
	   
       dimModel<<-PredsRDSNFS$dimModel
       dimPreds=PredsRDSNFS$DimPreds
	   
       dim(predsNFS)=c(dimPreds)
	   dim(predsSSL)=c(dimPreds)
	   dim(predsPUP)=c(dimPreds)
#########################################
resultBlob_tmp=NULL
resultBlob_tmp <- foreach(i = 1:length(listImageBl_NFS),.combine=rbind) %dopar% {
 #  for (i in 1: length (listImageBl)) {
     
	# i=200
     name=basename(listImageBl_NFS[i])
     img_pth=listImageBl_NFS[i]
     
	 mask0NFS=predsNFS[i, , , ]
	 mask0SSL=predsSSL[i, , , ]
	 mask0PUP=predsPUP[i, , , ]
	 
     img0NFS <- t(mask0NFS)
	 img0SSL <- t(mask0SSL)
	 img0PUP <- t(mask0PUP)
	 
     dim(img0NFS) <- c(dimModel[1], dimModel[2], 1)
	 dim(img0SSL) <- c(dimModel[1], dimModel[2], 1)
	 dim(img0PUP) <- c(dimModel[1], dimModel[2], 1)
	
	 
     imgNFS = getFrame(img0NFS, 1)
	 imgSSL = getFrame(img0SSL, 1)
	 imgPUP = getFrame(img0PUP, 1)
	 
	imgSSL= erode(imgSSL, makeBrush(17,shape='disc'))
	 imgPUP= dilate(imgSSL, makeBrush(63,shape='disc'))
	 
	 img=imgNFS-imgPUP -imgSSL
	 img[img[,]<0]=0
	 
	 
	# display(img)
	 
	 
       nmask = thresh(img, 18, 18, 0.009) 
      # display(nmask)
	   
       nmask1 <- fillHull(nmask)
       nmask2 = opening(nmask1, makeBrush(7,shape='disc')) # shape='Gaussian', sigma=50
	   
	 
	   
       nmask3 = fillHull(nmask2)
       nmask4 = bwlabel(nmask3)
         if (max(nmask4)!=0) {   
            fts = computeFeatures.moment(nmask4)  # coordinat
            shapeFeatures <- computeFeatures.shape(nmask4) # get radiuus, perimetr, area for a future IT IS MATERIAL FOR XGBOOST
            BlobTable=data.frame(fts,shapeFeatures,img=name,img_pth=img_pth) 
       #     resultBlob_tmp=rbind(resultBlob_tmp,BlobTable)			



}
} 
  
if (is.null(resultBlob_tmp)==F){resultBlob=rbind(resultBlob,resultBlob_tmp)}
             
     stopCluster(cl)         
 
}
            resultBlob$DimModel=dimModel[1]
		   write.csv(resultBlob,pth_resultBlob,row.names = F)
		  
    
