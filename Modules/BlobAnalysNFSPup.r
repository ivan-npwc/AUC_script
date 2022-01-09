if (!require("parallel")) {install.packages("parallel"); library("parallel")}
if (!require("doParallel")) {install.packages("doParallel"); library("doParallel")}
if (!require("foreach")) {install.packages("foreach"); library("foreach")}
library(EBImage)
library(tools)

     labelInput ="E:\\2021_19_OPP\\20210728_105310_DKV1996"
     Species = "NFSPup"
   
  
      predsDir=paste0(labelInput,"\\Predict\\Preds")
      listPreds=list.files(predsDir,full.names=T,pattern=Species)  
      date1=substr(basename(labelInput),1,15)
   
    pth_resultBlob <<- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv") 
    pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\",Species, "_BlobTable_", date1, "_tmp.csv")
    resultBlob_tmp=NULL
    resultBlob=NULL
   
   
   
   
   
     cl <- makePSOCKcluster(detectCores (logical=F)) 
    clusterEvalQ(cl, {library(EBImage)})
    registerDoParallel(cl)
   
   
   
   for (f in 1:length(listPreds)) {
     PredsRDS=readRDS(listPreds[f])
     dimPreds=PredsRDS$DimPreds
	 preds=  PredsRDS$preds
	# dim(preds)=c(dimPreds)
  ###########
     imgs= data.frame(tiles=PredsRDS$listImageBl)
	 imgs$imNoext=file_path_sans_ext(imgs$tiles)
	 imgs$img= paste0(substr(imgs$imNoext,1,29),".jpg")
	 imgs$Npreds=c(1:length(imgs$tiles))
#############################
listImgs=unique(imgs$img) 
#########################################
resultBlob_tmp <- foreach(i = 1:length(listImgs),.combine=rbind) %dopar% {
#  for (i in 1: length (listImgs)) {
 #######################################
  name=listImgs[i]
  Npreds=imgs$Npreds[imgs$img==name]
  ##############################crop1 
  mask1=preds[Npreds[1], , ,]
  Crop1=mask1[0:192,1:256]
  ################################################crop2
  mask2=preds[Npreds[2], , ,] 
  Crop2=mask2[64:256,0:256]
  ########
Imgrow1=abind(Crop1,Crop2,along=1)
#############################################cpop 3
   mask3=preds[Npreds[3], , ,] 
  Crop3=mask3[0:192,0:256]
###########################################crop 4
  mask4=preds[Npreds[4], , ,] 
  Crop4=mask4[64:256,0:256]
Imgrow2=abind(Crop3,Crop4,along=1)
#####################
ImgBig=abind(Imgrow1[,1:193],Imgrow2[,64:256],along=2)
ImgBig1=resize(ImgBig,193,193)
imgOrig=matrix(data=0, nrow = 256, ncol = 256)
imgOrig[32:224,32:224]=ImgBig1
########################################################################################################
############################################################################################################ blob analis
############################################################################################################
     mask0=imgOrig
     img0 <- t(mask0)
     dim(img0) <- c(256, 256, 1)
     img = getFrame(img0, 1)
       nmask = thresh(img, 18, 18, 0.009)  
       nmask1 <- fillHull(nmask)
       nmask2 = opening(nmask1, makeBrush(7,shape='disc') ) # shape='Gaussian', sigma=50
       nmask3 = fillHull(nmask2)
       nmask4 = bwlabel(nmask3)
	   
         if (max(nmask4)!=0) {   
            fts = computeFeatures.moment(nmask4)  # coordinat
            shapeFeatures <- computeFeatures.shape(nmask4) # get radiuus, perimetr, area for a future IT IS MATERIAL FOR XGBOOST
            BlobTable=data.frame(fts,shapeFeatures,img=name,img_pth=name) 
           # resultBlob_tmp=rbind(resultBlob_tmp,BlobTable)			
}
##
}
print(f)

if (is.null(resultBlob_tmp)==F){resultBlob=rbind(resultBlob,resultBlob_tmp)
 write.csv(resultBlob,pth_resultBlob_tmp,row.names = F) 
 }
 
}
            
write.csv(resultBlob,pth_resultBlob,row.names = F)
		  
      
  stopCluster(cl)



#display(imgOrig)


#PredsNew=list(preds=imgOrig,listImageBl=img)
#if (i==1){PredsNew1=PredsNew} else {

#PredsNew1$preds=append(list(PredsNew$preds),list(PredsNew1$preds))
#PredsNew1$listImageBl=c(PredsNew$listImageBl,PredsNew1$listImageBl)
#}
#}	 
#}
#PredsNew1$dimModel=256








