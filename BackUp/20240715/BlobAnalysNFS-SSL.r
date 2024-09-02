
     library(EBImage)
     Species="NFSAdult"
	 labelInput #="C:\\Users\\usato\\NFS_DB\\2024_138_OPP\\20240630_124112"
	 
     predsDir=paste0(labelInput,"\\Predict\\Preds")
     
	 listPredsNFS=list.files(predsDir,full.names=T,pattern="NFSAdult")  
	 listPredsSSL=list.files(predsDir,full.names=T,pattern="SSLAdult")
	 listPredsPUP=list.files(predsDir,full.names=T,pattern="SSLPup")
	 listPredsNFSPup=list.files(predsDir,full.names=T,pattern="NFSPup")
	# listPredsNFS_AN_TF_UNET=list.files(predsDir,full.names=T,pattern="NFS_AN_TF_UNET")
	 
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
	   PredsRDSNFSPup=readRDS(listPredsNFSPup[f])
	#   PredsRDSNFS_AN_TF_UNET=readRDS(listPredsNFS_AN_TF_UNET[f])
		
       listImageBl_NFS=PredsRDSNFS$listImageBl
	   listImageBl_SSL=PredsRDSSSL$listImageBl
	   listImageBl_PUP=PredsRDSPUP$listImageBl
	   listImageBl_NFSPup=PredsRDSNFSPup$listImageBl
	#   listImageBl_NFS_AN_TF_UNET=PredsRDSNFS_AN_TF_UNET$listImageBl
	   
	   
       predsNFS=PredsRDSNFS$preds
	   predsSSL=PredsRDSSSL$preds
	   predsPUP=PredsRDSPUP$preds
	   predsNFSPup=PredsRDSNFSPup$preds
	#   predsNFS_AN_TF_UNET=PredsRDSNFS_AN_TF_UNET$preds
	   
       dimModel<<-PredsRDSNFS$dimModel
       dimPreds=PredsRDSNFS$DimPreds
	   
       dim(predsNFS)=c(dimPreds)
	   dim(predsSSL)=c(dimPreds)
	   dim(predsPUP)=c(dimPreds)
	   dim(predsNFSPup)=c(dimPreds)
	#   dim(predsNFS_AN_TF_UNET)=c(dimPreds)
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
	 mask0NFSPup=predsNFSPup[i, , , ]
	# mask0NFS_AN_TF_UNET=predsNFS_AN_TF_UNET[i, , , ]
	 
     img0NFS <- t(mask0NFS)
	 img0SSL <- t(mask0SSL)
	 img0PUP <- t(mask0PUP)
	 img0NFSPup <- t(mask0NFSPup)
	# img0NFS_AN_TF_UNET <- t(mask0NFS_AN_TF_UNET)
	 
     dim(img0NFS) <- c(dimModel[1], dimModel[2], 1)
	 dim(img0SSL) <- c(dimModel[1], dimModel[2], 1)
	 dim(img0PUP) <- c(dimModel[1], dimModel[2], 1)
	 dim(img0NFSPup) <- c(dimModel[1], dimModel[2], 1)
	# dim(img0NFS_AN_TF_UNET) <- c(dimModel[1], dimModel[2], 1)
	
	  
	 
     imgNFS = getFrame(img0NFS, 1)
	 imgSSL = getFrame(img0SSL, 1)
	 imgPUP = getFrame(img0PUP, 1)
	 imgNFSPup = getFrame(img0NFSPup, 1)
	# imgNFS_AN_TF_UNET = getFrame(img0NFS_AN_TF_UNET, 1)
	 
	# imgPUP[imgPUP[,]  != 0] = 1
	 
	 imgSSL= dilate(imgSSL, makeBrush(37,shape='disc')) # shape='Gaussian', sigma=50
	 imgPUP= dilate(imgPUP, makeBrush(43,shape='disc'))
	 imgNFSPup= dilate(imgNFSPup, makeBrush(7,shape='disc'))
	 
	# imgNFS_AN_TF_UNET= erode(imgNFS_AN_TF_UNET, makeBrush(3,shape='disc'))
	 
	# display(img)
	 
     #  nmaskNFS = thresh(imgNFS, 18, 18, 0.009) 
     #  nmask1NFS <- fillHull(nmaskNFS)
     #  nmask2NFS = opening(nmask1NFS, makeBrush(7,shape='disc')) # shape='Gaussian', sigma=50
     #  nmask3NFS = fillHull(nmask2NFS)
	   
	   nmaskSSL = thresh(imgSSL, 18, 18, 0.009) 
       nmask1SSL <- fillHull(nmaskSSL)
       nmask2SSL = opening(nmask1SSL, makeBrush(7,shape='disc')) # shape='Gaussian', sigma=50
       nmask3SSL = fillHull(nmask2SSL)
	 
	 
	#   nmaskPUP = thresh(imgPUP, 18, 18, 0.009) 
     #  nmask1PUP <- fillHull(nmaskPUP)
    #   nmask2PUP = opening(nmask1PUP, makeBrush(7,shape='disc')) # shape='Gaussian', sigma=50
     #  nmask3PUP = fillHull(nmask2PUP)
	 
	   nmaskNFSPup = thresh(imgNFSPup, 18, 18, 0.009) 
       nmask1NFSPup <- fillHull(nmaskNFSPup)
       nmask2NFSPup = opening(nmask1NFSPup, makeBrush(7,shape='disc')) # shape='Gaussian', sigma=50
       nmask3NFSPup = fillHull(nmask2NFSPup)
	 
	   
	   
	  nmask3NFS = imgNFS  -  nmask3SSL-  imgPUP - nmask3NFSPup #nmask3PUP -    nmask3NFS=imgNFS - imgPUP - imgSSL
	  nmask3NFS[nmask3NFS[,]<0]=0
	   
	   # nmask3NFS= dilate(nmask3NFS, makeBrush(7,shape='disc'))
	   nmask= thresh(nmask3NFS, 18, 18, 0.009) 
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
		  
    
