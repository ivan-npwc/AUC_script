  library(RSQLite)
  library(dplyr)
  library(rgdal)
  
  source("Modules/GetAgisoftShape.r")
  source("Modules/SQLiteWrite.r")
  source("Modules/PredsPrepareToSQL.r")
   
   labelInput
   Species
   site<<-"138"
   Effort<<-read.csv(KK_Effort,sep=",")

###############################################################################
    CheckSubFold1=dir.exists(paste0(labelInput,"\\Predict"));CheckSubFold2=dir.exists(paste0(labelInput,"\\Observer count"))
    if (CheckSubFold1==F & CheckSubFold2==F) {SubFold=T} else {SubFold=F} 
      if(SubFold==T) {listSubfold=list.dirs(labelInput,full.names=T,recursive=F)}
      if(SubFold==F) {listSubfold=labelInput} 
  #################################################### FULL COUNT collect data in sub fold
   EffortDay<<-Effort[Effort$date %in%  basename(listSubfold)  & Effort$type_count !="opp_manual_model_count",]
   
   if (length(EffortDay$date) != 0){
   
   if (length(EffortDay$observer[1])==0) {stop(paste0("No Effort found for ",DtimSur2))}
     for (i in 1: length(EffortDay$type_count)) {  
	   EffortSort<<-EffortDay[i,] 
   for (y in 1:length (listSubfold)) {
      labelinput1<<-listSubfold[y]
        Subdata=PredsPrepareSQL(labelinput1)
        if (y==1){FinData=Subdata} else {
         FinData$animal_type= c(FinData$animal_type,Subdata$animal_type)  
         FinData$iLeft= c(FinData$iLeft,Subdata$iLeft)
         FinData$iTop= c(FinData$iTop,Subdata$iTop)
	     FinData$local_site=c(FinData$local_site,Subdata$local_site)
		 FinData<<-FinData
     } 
	} 
	####
	   SQLiteWriteFunction(
    GPSAltitude=FinData$GPSAltitude,
    site=FinData$site,
    r_year = FinData$r_year ,          
    r_date= FinData$r_date,
    time_start= FinData$time_start , 
    observer =FinData$observer,
    animal_type=   FinData$animal_type,
    iLeft=FinData$iLeft,
    iTop=FinData$iTop,
    file_nameOPP= basename(FinData$file_nameOPP),
    ########################################   photo_count_list
    visibility= FinData$visibility,
    CommentsPCL= FinData$CommentsPCL,
    rain= FinData$rain,
    distance= FinData$distance,
	type_count=FinData$type_count,
    type= FinData$type,
    quality= FinData$quality,
    splash= FinData$splash,
    species= FinData$Species,
    #######################################   photo_count_files
	 latitude=FinData$lat,
     longitude=FinData$lon,
     altitude=FinData$altitude,
    CommentsPCF=FinData$CommentsPCF,      
    file_name= FinData$file_name,
	local_site=FinData$local_site
	####################################   polygons_model_sites
  )
  }
  }
#########################################################	MANUAL COUNT each sub fold in unique 
##############################################################
##########################################################

 
 EffortDay<<-Effort[Effort$date %in%  basename(listSubfold)  & Effort$type_count =="opp_manual_model_count",] ## sort all sub folder and count type for general folder

if (length(EffortDay$date) != 0){ 
   
	for (y in 1:length (EffortDay$date)) {
	EffortSort<<-EffortDay[y,]
    labelinput1<<-paste0(listSubfold[basename(listSubfold)==EffortDay$date[y]])
     FinData<<-PredsPrepareSQL(labelinput1) 
	 
 SQLiteWriteFunction(
    GPSAltitude=FinData$GPSAltitude,
    site=FinData$site,
    r_year = FinData$r_year ,          
    r_date= FinData$r_date,
    time_start= FinData$time_start , 
    observer =FinData$observer,
    animal_type=   FinData$animal_type,
    iLeft=FinData$iLeft,
    iTop=FinData$iTop,
    file_nameOPP= basename(FinData$file_nameOPP),
    ########################################   photo_count_list
    visibility= FinData$visibility,
    CommentsPCL= FinData$CommentsPCL,
    rain= FinData$rain,
    distance= FinData$distance,
	type_count=FinData$type_count,
    type= FinData$type,
    quality= FinData$quality,
    splash= FinData$splash,
    species= FinData$Species,
    #######################################   photo_count_files
	 latitude=FinData$lat,
     longitude=FinData$lon,
     altitude=FinData$altitude,
    CommentsPCF=FinData$CommentsPCF,      
    file_name= FinData$file_name,
	local_site=FinData$local_site
  )	 
}
}	 
################################################################	
 
 	
    
    

