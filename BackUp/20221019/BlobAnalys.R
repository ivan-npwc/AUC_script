
     Species
	 labelInput
	 
	 if (Species=="NFSPup1") {source("Modules/BlobAnalysNFSPup.r")} else {
	 
     predsDir=paste0(labelInput,"\\Predict\\Preds")
     listPreds=list.files(predsDir,full.names=T,pattern=Species)  
     date1=substr(basename(labelInput),1,15)
     resultBlob=NULL
	 
	 
	 
 
	 
	 
  for (f in 1:length(listPreds)) {
  
   cl <- makePSOCKcluster(detectCores (logical=F)-1) 
    clusterEvalQ(cl, {library(EBImage)})
    registerDoParallel(cl)
  
  
       Species=strsplit(basename(listPreds[f]),split = "_")[[1]][2]
       pth_resultBlob <<- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv") 
       pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\",Species, "_BlobTable_", date1, "_tmp.csv")
       PredsRDS=readRDS(listPreds[f])
       listImageBl=PredsRDS$listImageBl
       preds=PredsRDS$preds
       dimModel<<-PredsRDS$dimModel
      dimPreds=PredsRDS$DimPreds
      dim(preds)=c(dimPreds)
#########################################
resultBlob_tmp=NULL
resultBlob_tmp <- foreach(i = 1:length(listImageBl),.combine=rbind) %dopar% {
 #  for (i in 1: length (listImageBl)) {

     name=basename(listImageBl[i])
     img_pth=listImageBl[i]
     mask0=preds[i, , , ]
     img0 <- t(mask0)
     dim(img0) <- c(dimModel[1], dimModel[2], 1)
     img = getFrame(img0, 1)
       nmask = thresh(img, 18, 18, 0.009)  
       nmask1 <- fillHull(nmask)
       nmask2 = opening(nmask1, makeBrush(7,shape='disc') ) # shape='Gaussian', sigma=50
	   
	  if (Species != "LRG") { nmask2 = erode(nmask2, makeBrush(3, shape='diamond'))}
	   
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
		  
   }   
