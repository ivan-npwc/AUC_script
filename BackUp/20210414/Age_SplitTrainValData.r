	
SplitTrainValData =function (train_Gen_dir="C:\\SSL_DB\\SSL_AUC\\20191218\\data\\TRAIN\\Age_SSL",
                             ValIndex=0.3,
						     Split=T) {
  library(parallel)
  library(doParallel)
  cl <- makePSOCKcluster(detectCores ()) 
  clusterEvalQ(cl, { })
  registerDoParallel(cl)
  
   Small_FromDir =paste0(train_Gen_dir,"\\Small\\All")	
   Big_FromDir=paste0(train_Gen_dir,"\\Big\\All")
   Small_train_dir = paste0(train_Gen_dir,"\\Small\\Train")
   Small_val_dir = paste0(train_Gen_dir,"\\Small\\Validate")
   Big_train_dir = paste0(train_Gen_dir,"\\Big\\Train")
   Big_val_dir = paste0(train_Gen_dir,"\\Big\\Validate")		
     unlink(Small_train_dir, recursive=T); dir.create(Small_train_dir)
     unlink(Small_val_dir, recursive=T);dir.create(Small_val_dir)
     unlink(Big_train_dir, recursive=T);dir.create(Big_train_dir)
     unlink(Big_val_dir, recursive=T);dir.create(Big_val_dir)
   listFolder=list.files(Big_FromDir)
    foreach(i = 1:length(listFolder)) %dopar% {
   #  for (i in 1:length(listFolder)) {
        Age=paste0(listFolder[i])
        Big_age_folder=paste0(Big_FromDir,"\\",Age)
	    Smal_age_folder=paste0(Small_FromDir,"\\",Age)
        ImgList=list.files(Big_age_folder)
        index=sample(1:length(ImgList))[1:round(ValIndex * length(ImgList))]
        ImgListVal=ImgList[index]
        ImgListTrain=ImgList[-index]
   Big_train_dir_age=paste0(Big_train_dir,"\\",Age) ; if (dir.exists(Big_train_dir_age) ==F) {dir.create(Big_train_dir_age)}
   SmallTrainDirAge=paste0(Small_train_dir,"\\",Age) ; if (dir.exists(SmallTrainDirAge) ==F) {dir.create(SmallTrainDirAge)}
   if (Split==T) {
    Big_val_dir_age=paste0(Big_val_dir,"\\",Age) ; if (dir.exists(Big_val_dir_age) ==F) {dir.create(Big_val_dir_age)}
    SmallValDirAge=paste0(Small_val_dir,"\\",Age) ; if (dir.exists(SmallValDirAge) ==F) {dir.create(SmallValDirAge)}
   }
   
   SmallImgListVal=paste0(Small_FromDir,"\\",Age,"\\",ImgListVal)
   SmalImgListTrain=paste0(Small_FromDir,"\\",Age,"\\",ImgListTrain)
   BigImgListVal=paste0(Big_FromDir,"\\",Age,"\\",ImgListVal)
   BigImgListTrain=paste0(Big_FromDir,"\\",Age,"\\",ImgListTrain)
  if (Split==T) {
      file.copy(SmallImgListVal,SmallValDirAge)
      file.copy(SmalImgListTrain,SmallTrainDirAge)
	  file.copy(BigImgListVal,Big_val_dir_age)
	  file.copy(BigImgListTrain,Big_train_dir_age)
} else {
     file.copy(paste0(Smal_age_folder,"\\",ImgList),SmallTrainDirAge) 
     file.copy(paste0(Big_age_folder,"\\",ImgList), Big_train_dir_age) 
  }
  
   }
    stopCluster (cl)
   }
   SplitTrainValData()