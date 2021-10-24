library(EBImage)

Species
labelInput
UseAllHauloutImages
TimeOn1Anmls=7 #, 3 seconds for painting and 3 seconds for check
limitAutoMask=25 #no auto mask for more then 25 Animals


  predsDir=paste0(labelInput,"\\Predict\\Preds")
  listPreds=list.files(predsDir,full.names=T,pattern=Species) 
       if (length(listPreds)==0){stop(paste0("No prediction found for ", Species))} 
  date1=   substr(basename(labelInput),1,15)

  if (UseAllHauloutImages==F){
  ImgDir=paste0(labelInput,"\\Error_",Species,"\\Image")
  MskDir=paste0(labelInput,"\\Error_",Species,"\\Mask")
 
  } 
  if (UseAllHauloutImages==T){ 
  HauloutDir=paste0(labelInput,"\\Predict\\Haulout")
  listImgsHaul=list.files(HauloutDir,full.names=T)
  dir.create(paste0(labelInput,"\\Error_",Species))
  ImgDir=paste0(labelInput,"\\Error_",Species,"\\Image"); dir.create(ImgDir)
  MskDir=paste0(labelInput,"\\Error_",Species,"\\Mask"); dir.create(MskDir)
 file.copy(listImgsHaul,ImgDir)
 } 
 
  ListImgs=list.files(ImgDir)
  for (f in 1:length(listPreds)) {
    
    cl <- makePSOCKcluster(detectCores (logical=F)) 
    clusterEvalQ(cl, {library(EBImage)})
    registerDoParallel(cl)
    #   Species=strsplit(basename(listPreds[f]),split = "_")[[1]][2]
    
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
      
	  eierode = erode(nmask2, makeBrush(3, shape='diamond'))
	#  dilate=dilate(eierode, makeBrush(2, shape='diamond'))
	  
      nmask3 = fillHull(eierode)	
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
	 
	library(dplyr)


DirImgs=paste0(labelInput,"\\Error_",Species,"\\Image")
pth_resultBlob <- paste0(labelInput,"\\Predict\\",Species,"_BlobTable_", date1, ".csv")

listImgs=list.files(DirImgs)
 listImgs1=gsub(paste0(Species,"_"),"",listImgs)
resultBlob =read.csv(pth_resultBlob)
SORTresultBlob =  resultBlob[basename(as.character(resultBlob$img_pth)) %in% listImgs1,]

SORTresultBlob1=SORTresultBlob %>%
                select(img_pth) %>%
			   group_by(img_pth) %>%
			   summarise(n=n())

SORTresultBlob1=SORTresultBlob1[order(SORTresultBlob1$n,decreasing = T),]	
SORTresultBlob1$time1=	SORTresultBlob1$n*TimeOn1Anmls
GaremMsk=basename(as.character(SORTresultBlob1$img_pth [SORTresultBlob1$n > limitAutoMask]))
GaremMsk=gsub("jpg","png",GaremMsk)
GaremMsk1=paste0(labelInput,"\\Error_",Species,"\\Mask\\", Species,"_",GaremMsk)
#unlink(GaremMsk1)
TimeGeneral=round(sum(SORTresultBlob1$time1)/60/60)
print(paste0("Time for masking ", TimeGeneral," hours"))

   
			    
	 
	 
#MaskAutoCreator()