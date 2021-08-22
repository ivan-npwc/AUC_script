  NormAgeFun = function (train_Gen_dir="C:\\SSL_DB\\SSL_AUC\\20191218\\data\\TRAIN\\Age_SSL",
                         Split=T) { 
  library(magick)
  library(parallel)
  library(doParallel)
     Small_train_dir= paste0(train_Gen_dir,"\\Small\\Train")
     Small_val_dir= paste0(train_Gen_dir,"\\Small\\Validate")
     Big_train_dir = paste0(train_Gen_dir,"\\Big\\Train")
     Big_val_dir= paste0(train_Gen_dir,"\\Big\\Validate")
       ListFolderSmallTrain=list.files(Small_train_dir,full.names=T)
       ListFolderSmallVal=list.files(Small_val_dir, full.names=T)
        countImgsVal=NULL
        countImgsTrain=NULL
    for (y in 1:length(ListFolderSmallTrain)) {
       folder=ListFolderSmallTrain[y]
       countImgs=data.frame(length(list.files(folder)))
       countImgsTrain=rbind(countImgs,countImgsTrain)
    } ; TrainMaxForAge=max(countImgsTrain[,1])
if (Split== T) 	{
    for (y in 1:length(ListFolderSmallVal)) {
       folder=ListFolderSmallVal[y]
       countImgs=data.frame(length(list.files(folder)))
       countImgsVal=rbind(countImgs,countImgsVal)
    };ValMaxForAge=max(countImgsVal[,1])
	}
    for (i in 1:length(ListFolderSmallTrain)) {	
       Age= basename(ListFolderSmallTrain[i])
       Big_train_dir_age=paste0(Big_train_dir,"\\",Age) 
       Big_val_dir_age=paste0(Big_val_dir,"\\",Age) 
       SmallValDirAge=paste0(Small_val_dir,"\\",Age) 
       SmallTrainDirAge=paste0(Small_train_dir,"\\",Age) 
         TrainListImgs=list.files(Big_train_dir_age)
         ValListImgs=list.files(Big_val_dir_age)
           TrainCountImg=length(TrainListImgs)
           ValCountImg=length(ValListImgs)
	cl <- makePSOCKcluster(detectCores ()) 
    clusterEvalQ(cl, {
    library(magick)
    })
    registerDoParallel(cl)	   	   
   if (TrainCountImg < TrainMaxForAge) {                  #TRAIN
      TrainCountImg_NEED= round(TrainMaxForAge - TrainCountImg)
	  
        foreach(f = 1:TrainCountImg_NEED) %dopar% {
        # for (f in 1: TrainCountImg_NEED) {                             
            index=sample(1:length(TrainListImgs))[1]
            randomImg=paste0(TrainListImgs[index])
            RandomImgSmall_pth=paste0(SmallTrainDirAge,"\\",randomImg)
		    RandomImgBig_pth=paste0(Big_train_dir_age,"\\",randomImg)
		      img_Small=image_read(RandomImgSmall_pth)
		      img_Big=image_read(RandomImgBig_pth)
                newPthSmall=paste0(SmallTrainDirAge,"\\",f,"_",randomImg)
		        newPthBig=paste0(Big_train_dir_age,"\\",f,"_",randomImg)
        image_write(img_Small,newPthSmall)
		image_write(img_Big,newPthBig)
 } 
 }   
      if (Split== T) {
      if (ValCountImg < ValMaxForAge) {                           # VALIDATE 
         ValCountImg_NEED= round(ValMaxForAge - ValCountImg)
		 foreach(f = 1:ValCountImg_NEED) %dopar% {	
         # for (f in 1: ValCountImg_NEED) {                             
             index=sample(1:length(ValListImgs))[1]
             randomImg=paste0(ValListImgs[index])
             RandomImgSmall_pth=paste0(SmallValDirAge,"\\",randomImg)
		     RandomImgBig_pth=paste0(Big_val_dir_age,"\\",randomImg)
		img_Small=image_read(RandomImgSmall_pth)
		img_Big=image_read(RandomImgBig_pth)
            newPthSmall=paste0(SmallValDirAge,"\\",f,"_",randomImg)
		    newPthBig=paste0(Big_val_dir_age,"\\",f,"_",randomImg)
        image_write(img_Small,newPthSmall)
		image_write(img_Big,newPthBig)
 } 
 }
 }
 stopCluster(cl)
 }
###############################################
ImgsRenameTolabel=function (train_Gen_dir) {
    Small_train_dir= paste0(train_Gen_dir,"\\Small\\Train")
    Small_val_dir= paste0(train_Gen_dir,"\\Small\\Validate")
    Big_train_dir = paste0(train_Gen_dir,"\\Big\\Train")
    Big_val_dir= paste0(train_Gen_dir,"\\Big\\Validate")
  	listFolder=c(Small_train_dir,Small_val_dir,Big_train_dir,Big_val_dir)
      for (i in 1:length(listFolder)) {
           GenFolder=  listFolder[i]
		 if (dir.exists( GenFolder)==T)  { 
           listAgeFold=list.files(GenFolder)
               for (u in 1:length(listAgeFold)) {
                 age= listAgeFold[u]
                 AgeDir= paste0(GenFolder,"\\",age)
				   if (dir.exists(AgeDir)==T) {
                 listImgsFrom=list.files(AgeDir,full.names=T)
                 ListImgsTo=paste0(age,"_",basename(listImgsFrom))
                 ListImgsTo1=paste0(GenFolder,"\\",age,"\\",ListImgsTo)
   file.rename(listImgsFrom,ListImgsTo1)
}
}
}
}
}
ImgsRenameTolabel(train_Gen_dir)
 }
NormAgeFun()