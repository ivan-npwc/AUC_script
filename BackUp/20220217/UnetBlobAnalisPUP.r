
                     library(tools)
					 library(EBImage)
             		 labelInput 
                     Species
                     batch_size_global=  600
                     batch_size =60
                     date1=substr(basename(paste0(labelInput)),1,15)
					 predict_dir=paste0(labelInput,"\\Predict\\PUP")
					 pth_resultBlob <<- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv") 
                     pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\",Species, "_BlobTable_", date1, "_tmp.csv")
			         predsDir=paste0(labelInput,"\\Predict\\Preds")
					# TMPpredsSaveDir=paste0(labelInput,"\\Predict\\PUP_Preds")
                     resultBlob_tmp=NULL
                     resultBlob=NULL
					 
##############################################################################################################################
#dir.create(TMPpredsSaveDir)

if (Species== "NFSPup") { PTHweight=paste0(System_data,"\\weights\\",NFS_Pup_weight_pth)}
if (Species== "SSLPup") { PTHweight=paste0(System_data,"\\weights\\",SSL_Pup_weight_pth)}


    
if(dir.exists(predsDir)==T){listPredsDelete=list.files(predsDir,full.names=T,pattern= Species);unlink(listPredsDelete) }else{dir.create(predsDir)}


##############################################################################  
  
  if(exists("unet1")==F) {source("Modules/unetVGG16Create.r")}
  
   weight<<-readRDS(PTHweight)
   set_weights(unet1,weight)
   
   attr(dice_coef_loss, "py_function_name") <- "dice_coef_loss"
   attr(dice_coef, "py_function_name") <- "dice_coef"
   
   unet1 <- unet1 %>%
        compile(
        optimizer = optimizer_adam(lr= 0.0001 , decay = 1e-6 ),     #optimizer_nadam              
        loss =   dice_coef_loss,    
        metrics = c(dice_coef) 
    )
	
	
  layer_output <- get_layer(unet1, index=1)$output
  shape=layer_output$shape								 
  dimModel<<-c(paste0(shape[2]),paste0(shape[2]))								 
  
  
  listImage_glob <<-list.files(predict_dir, full.names = T,  recursive = T, include.dirs = F,pattern="png|JPG|jpg|jpeg")
  if (length(listImage_glob)==0){stop("No Images For Predict Found")}
  global_steps <<- round(length(listImage_glob)/batch_size_global)
  if(length(listImage_glob) > (global_steps*batch_size_global)) {global_steps=global_steps+1}
 ###################################################### 
   cl <- makePSOCKcluster(detectCores(logical=FALSE)) 
    clusterEvalQ(cl, {
      library(magick)     
      library(abind)     
      library(reticulate)
      library(EBImage)
     library(keras)
	  library(tools)
 
      imageRead <- function(image_file,
	                        dimModel1
                            ) {	
		dimModel2<<-dimModel1					
        img <- image_read(image_file)
        img <- image_scale(img, paste0(dimModel2[1], "x", dimModel2[2], "!"))
      }
      img2arr <- function(image, 
	                      dimModel1) {			  
		dimModel2<<-dimModel1				  
        result <- aperm(as.numeric(image[[1]])[, , 1:3], c(2, 1, 3)) # transpose
        dim(result) <- c(1, dimModel2[1], dimModel2[2], 3)
        return(result)
      }						  
    })
    registerDoParallel(cl)
  #################################################################################
  for (e in 1:global_steps) {
  # foreach(e = 1:global_steps) %dopar% { 
  #  e=1
    batch_ind_global <- c(1:length(listImage_glob))[1:batch_size_global]
    listImage <- listImage_glob[batch_ind_global]
    listImage=listImage[is.na(listImage)==F]
    if (length(listImage_glob) > length(listImage)) {
      listImage_glob <<- listImage_glob[-batch_ind_global]
    }
    listImage=listImage
    listImageBl=listImage
    listImgName<- basename(listImage) 
    batch_size=batch_size
    steps= round(length(listImage)/batch_size)
    if(length(listImage) > (steps*batch_size)) {steps=steps+1} 
 
    #######################################################################################
    test_generator <- function(listImage,batch_size,dimModel) {			  
      function()   {     
        batch_ind <- c(1:length(listImage))[1:batch_size]
        batch_images_list <- listImage[batch_ind]
        batch_images_list=batch_images_list[is.na(batch_images_list)==F]
        if (length(listImage) > length(batch_images_list)) {
          listImage <<- listImage[-batch_ind]
        }
        dimModel0=dimModel
        x_batch <- foreach(y = 1:length(batch_images_list)) %dopar% {
          img <- imageRead(image_file = batch_images_list[y],dimModel1=dimModel0)
          arr <- img2arr(img,dimModel1=dimModel0)
        }
        x_batch <- do.call(abind, c(x_batch, list(along = 1)))
        result <- list(keras_array(x_batch))
      }
    }
    
    test_iterator <- py_iterator(test_generator(listImage,batch_size,dimModel=dimModel))	
#	batch=iter_next(test_iterator)
 if (tensorflow::tf_version() <= "2.0") {
           preds <- predict_generator(unet1, test_iterator, steps = steps)
 } else {
           preds=keras:::predict.keras.engine.training.Model(object=unet1,
                                                   x=test_iterator,
                                                   batch_size = batch_size,
                                                   verbose = 0,
                                                   steps = steps
                                                   )
 }
    ############
	pthSavePreds=paste0(predsDir,"\\Preds_",Species,"_",e)
	Preds1=list(preds=preds,DimPreds=dim(preds),dimModel=dimModel,listImageBl=listImageBl)
	
	 PredsRDS=Preds1
     dimPreds=PredsRDS$DimPreds
	 preds=  PredsRDS$preds
	# dim(preds)=c(dimPreds)
  ###########
     imgs= data.frame(tiles=basename(PredsRDS$listImageBl))
	 imgs$imNoext=file_path_sans_ext(imgs$tiles)
	 
	 for (i in 1:length(imgs$imNoext)) {
	 imgs$img[i]=paste0(strsplit(imgs$imNoext[i],"#")[[1]][1],".jpg")
	 }
	
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
       nmask2 = opening(nmask1, makeBrush(5,shape='disc') ) # shape='Gaussian', sigma=50
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
print(paste0(e," in  ",global_steps))

if (is.null(resultBlob_tmp)==F){resultBlob=rbind(resultBlob,resultBlob_tmp)
write.csv(resultBlob,pth_resultBlob_tmp,row.names = F) 
 }
 
}
            
write.csv(resultBlob,pth_resultBlob,row.names = F)
	
	
	
	
	#saveRDS(Preds1,pthSavePreds)
	
stopCluster(cl)


#unlink(TMPpredsSaveDir, recursive=T)
