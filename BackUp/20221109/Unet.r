if (Species== "NFSPup1") { source("Modules/UnetBlobAnalisNFSPup.r") } else {
                     labelInput 
                     Species
                     batch_size_global=  300
                     batch_size =30
                     date1=substr(basename(paste0(labelInput)),1,15)
					 predict_dir=paste0(labelInput,"\\Predict\\Haulout")
					 listImgdonePredPTH=paste0(labelInput,"\\Predict\\listImgdonePred.csv")
##############################################################################################################################
if (Species== "NFSAdult") {PTHweight=NFS_Adult_weight_pth }		
if (Species== "SSLAdult") {PTHweight=SSL_Adult_weight_pth}	
if (Species== "NFSPup1") { PTHweight=NFS_Pup_weight_pth256; predict_dir=paste0(labelInput,"\\Predict\\PUP")}
if (Species== "NFSPup") { PTHweight=NFS_Pup_weight_pth}
if (Species== "SSLPup") { PTHweight=SSL_Pup_weight_pth}
if (Species== "LRG") {PTHweight=LRG_pth}
if (Species== "WLRS" & Terrain =="Sand")   {PTHweight=WLRS_Sand_weight_pth}
if (Species== "WLRS" & Terrain =="Rocky")   {PTHweight=WLRS_Rocky_weight_pth}


    PTHweight<-paste0(System_data,"/weights/",PTHweight)
if (ModelCheckAlg==T) { PTHweight <- PTHweightCHECK}
    predsDir=paste0(labelInput,"\\Predict\\Preds")
	
	#if(dir.exists(predsDir)==T){listPredsDelete=list.files(predsDir,full.names=T,pattern= Species);unlink(listPredsDelete) }else{
	dir.create(predsDir)
############################################################################## 
if (file.exists(listImgdonePredPTH)){listImgdonePred=read.csv(listImgdonePredPTH)[,1]} else {listImgdonePred=NULL} 
##############################################################################  
  
  if(exists("unet1")==F & Species != "NFSPup") {source("Modules/unetVGG16Create.r")}
  if(exists("unet1")==F & Species == "NFSPup") {source("Modules/Unet512Create.r")}
  
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
  
  pth_resultBlob <<- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv") 
  pth_resultBlob_tmp <<-paste0(labelInput,"\\Predict\\",Species, "_BlobTable_", date1, "_tmp.csv")
  listImage_glob <<-list.files(predict_dir, full.names = T,  recursive = T, include.dirs = F,pattern="png|JPG|jpg|jpeg")
  
  if (is.null(listImgdonePred)==F){listImage_glob<<-listImage_glob[!listImage_glob %in% listImgdonePred] }
  
  global_steps <<- round(length(listImage_glob)/batch_size_global)
  if(length(listImage_glob) > (global_steps*batch_size_global)) {global_steps=global_steps+1}
 ###################################################### 
   cl <- makePSOCKcluster(detectCores(logical=FALSE)-2) 
    clusterEvalQ(cl, {
      library(magick)     
      library(abind)     
      library(reticulate)
      library(EBImage)
     library(keras)
 
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
	
	
	
	
	
	saveRDS(Preds1,pthSavePreds)
	print(paste0(e," in  ",global_steps))
	
	if (file.exists(listImgdonePredPTH)){listImgdonePred=read.csv(listImgdonePredPTH)[,1]}
	listImageWrite=c(listImage,listImgdonePred)
	write.csv(listImageWrite,listImgdonePredPTH,row.names=F)
	
}
stopCluster(cl)


}