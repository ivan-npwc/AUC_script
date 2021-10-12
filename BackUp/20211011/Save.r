  library(RSQLite)
  library(dplyr)
  library(rgdal)
  library(tools)
  
  source("Modules/GetAgisoftShape.r")
  source("Modules/SQLiteWrite.r")
  source("Modules/PredsPrepareToSQL.r")
   
   labelInput
   Species
   Count_type
   site
   SQLite_path
   System_data
###############################################################################
    CheckSubFold1=dir.exists(paste0(labelInput,"\\Predict"));CheckSubFold2=dir.exists(paste0(labelInput,"\\Observer_count"))
    if (CheckSubFold1==F & CheckSubFold2==F) {SubFold=T} else {SubFold=F} 
      if(SubFold==T) {listSubfold=list.dirs(labelInput,full.names=T,recursive=F)}
      if(SubFold==F) {listSubfold=labelInput} 
  #################################################### FULL COUNT collect data in sub fold   
   for (y in 1:length (listSubfold)) {
      labelinput1<<-listSubfold[y]
        Subdata=PredsPrepareSQL(labelinput1)
        if (y==1){FinData=Subdata} else {  ######################################### if NO subfold FinData=Subdata
         FinData$animal_type= c(FinData$animal_type,Subdata$animal_type)  
         FinData$iLeft= c(FinData$iLeft,Subdata$iLeft)
         FinData$iTop= c(FinData$iTop,Subdata$iTop)
	     FinData$local_site=c(FinData$local_site,Subdata$local_site)
		 FinData<<-FinData
     } 
	} 
##############################################################
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
	type_count=Count_type,
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

    

