
library(EBImage)
#MaskAutoCreator=function (labelInput1=labelInput) {

labelInput1=  labelInput #"E:\\SSL_DB\\2020_138_OPP\\20200615"                        
  
  predsDir=paste0(labelInput1,"\\Predict\\Preds")
  listPreds=list.files(predsDir,full.names=T)  
  date1=basename(labelInput1) 
  ImgDir=paste0(labelInput1,"\\Error\\Image")
  MskDir=paste0(labelInput1,"\\Error\\Mask")
  ListImgs=list.files(ImgDir)
  
  for (f in 1:length(listPreds)) {
    
    cl <- makePSOCKcluster(detectCores ()-1) 
    clusterEvalQ(cl, {library(EBImage)})
    registerDoParallel(cl)
       Species=strsplit(basename(listPreds[f]),split = "_")[[1]][2]
    
    PredsRDS=readRDS(listPreds[f])
    listImageBl=PredsRDS$listImageBl
	SpeciesRem=paste0(Species,"_")
	CheckPresence=basename(listImageBl) %in% gsub(SpeciesRem,"", ListImgs)
#	if(length(CheckPresence[CheckPresence==T]==0)<1)  { # work inly with prediction which includes image error
	
    preds=PredsRDS$preds
    dimModel=PredsRDS$dimModel
    dimPreds=PredsRDS$DimPreds
    dim(preds)=c(dimPreds)
    #########################################
 
    foreach(i = 1:length(listImageBl)) %dopar% {
      #  for (i in 1: length (listImageBl)) {
    
      name=basename(listImageBl[i])
	if (name %in%  gsub(SpeciesRem,"", ListImgs)==T){
       mask0=preds[i, , , ]
      img0 <- t(mask0)
	   img0 <- t(img0)
      dim(img0) <- c(dimModel[1], dimModel[2], 1)
      img = getFrame(img0, 1)
      nmask = thresh(img, 18, 18, 0.009)  
      nmask1 <- fillHull(nmask)
      nmask2 = opening(nmask1, makeBrush(7,shape='disc') ) # shape='Gaussian', sigma=50
      nmask3 = fillHull(nmask2)
      nmask4 = bwlabel(nmask3)
	  nmask5=resize(nmask4,1024,1024)
	  MaskPth=paste0(MskDir,"\\",SpeciesRem, gsub("jpg","png",name))
      writeImage(nmask5,MaskPth)
	  
	 # print(paste0(i,"    ",max(nmask4),"    ",name))
	 }
	 }
	# }  
 stopCluster(cl)
     }
#MaskAutoCreator()