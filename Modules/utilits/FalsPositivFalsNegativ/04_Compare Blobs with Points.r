AccuracyAnalis=function (labelInput) {
     library(EBImage)
	 library(parallel)
     library(doParallel)
     library(foreach)
	 library(magick)
	 
    ImgsPointDir=paste0(labelInput,"\\Predict\\PointsObserverImg")
     predsDir=paste0(labelInput,"\\Predict\\Preds")
	 pthSave= paste0(labelInput,"\\Predict\\Check") ; if (dir.exists(pthSave)==F) {dir.create(pthSave)}
     listPreds=list.files(predsDir,full.names=T)  
     date1=basename(labelInput) 
     resultBlob=NULL
	 
  for (f in 1:length(listPreds)) {
  
 cl <- makePSOCKcluster(4) 
    clusterEvalQ(cl, {library(EBImage);library(magick)})
    registerDoParallel(cl)
  
       Species=strsplit(basename(listPreds[f]),split = "_")[[1]][2]
       PredsRDS=readRDS(listPreds[f])
       listImageBl=PredsRDS$listImageBl
       preds=PredsRDS$preds
       dimModel=PredsRDS$dimModel
      dimPreds=PredsRDS$DimPreds
      dim(preds)=c(dimPreds)
#########################################
resultBlob_tmp <- foreach(i = 1:length(listImageBl),.combine=rbind) %dopar% {
     name=basename(listImageBl[i])
     img_points_pth=  paste0(ImgsPointDir ,"\\",name)
	   if (file.exists(img_points_pth) ==T) {
	 
           mask0=preds[i, , , ]
           img0 <- t(mask0)
           dim(img0) <- c(dimModel[1], dimModel[2], 1)

                  img_points=image_read(img_points_pth)
                  image_blobs=image_read(img0)
    			  image_blobs=image_scale(image_blobs,"1024x1024!")
				  
                  Check=image_composite(image_blobs,img_points,operator = "blend", compose_args="70")
                 save1=paste0(pthSave,"\\",name)
                 image_write(Check,save1)
    		  }}} 
stopCluster(cl)			  
}
AccuracyAnalis(labelInput=labelInput)