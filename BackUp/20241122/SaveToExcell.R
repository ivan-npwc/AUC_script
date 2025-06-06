
  library(dplyr)
  library(rgdal)
  library(tools)
  library(tidyr)

   labelInput
   Species
   ################
   
   OPPdirName=basename(gsub(basename(labelInput),"",labelInput))
   site=strsplit(OPPdirName,"_")[[1]][2]
   ##############
    if (Species=="SSLPup")   {stop("You can not Import SSL pups without Adult animals, please select  SSLAdult")}
 
  ObserverPoints1=NULL
  PredictPoints1=NULL
 
   if (loopCheck == 0){DirSaveExcel=choose.dir(caption = "Select folder to save Excell")}
  
   date_1= substr(basename(labelInput),1,15)
   year=substr(date_1,1,4)
   
   PthSaveExcell<<-paste0(DirSaveExcel,"\\",Species,"_",site,"_",year,".csv")
   ObserverCountDir=paste0(labelInput,"\\Observer_count")
   ObserverCountP=list.files(ObserverCountDir,full.names=T,pattern = "kml|shp")[1]
   PredictPointsP=paste0(labelInput,"\\Predict\\", Species,"_",date_1,".kml")

  AdditnlPointsPnfsP = paste0(labelInput,"\\Predict\\NFSPupAdd_",date_1,".kml")
  WLRSadd = paste0(labelInput,"\\Predict\\WLRSadd_",date_1,".kml")
  
  if (length(ObserverCountP)>0 & is.na(ObserverCountP)==F) {
    ObserverPoints= data.frame(readOGR(ObserverCountP))
	if (length(ObserverPoints$LAYER) ==0) {ObserverPoints$LAYER= ObserverPoints$Description}
	
	ObserverPoints1= ObserverPoints %>%
	                 group_by(Age=LAYER) %>%
					 summarize(Count=n(),Type="Observer Model Count")
	
					
}
	
 ################################
  if (file.exists(PredictPointsP)) {
 PredictPoints=  data.frame(readOGR(PredictPointsP))
 PredictPoints1=PredictPoints %>%
	                group_by  (Age= Description) %>%
					summarize(Count=n(),Type="Predict")
					}
 #########
	res=rbind(ObserverPoints1,PredictPoints1)
	res$Age[res$Age=="An"]="AN"
	res$Age[res$Age=="Sa"]="SA"
	res$Age[res$Age=="OM"]="AN"
	res$Age[res$Age=="UNK"]="U"
   res$Site=site
   res$Data=date_1
	res2=res
	 if (loopCheck ==0){write.csv(res2,PthSaveExcell,row.names=F)} else {
	resP=read.csv(PthSaveExcell);res2=rbind(res2,resP);write.csv(res2,PthSaveExcell,row.names=F)}
	
	loopCheck<<-1
	
##########################################################
       a=read.csv(PthSaveExcell)
	   
	   res1=	a %>% spread (Age,Count)
	 #  res3=data.frame(Site=site,Data=Data,res1)
	   PthSaveExcell1<<-paste0(DirSaveExcel,"\\Spread_",Species,"_",site,"_",year,".csv")
	   write.csv(res1,PthSaveExcell1, row.names=F)
	