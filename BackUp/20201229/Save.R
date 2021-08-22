

PredsWriteToSQL=function (labelinput1,
                           Species1=Species,
						   KK_Effort,
						   SQLite_path ) {  # It's only prefunction, the function is down  

 Species1<<-Species
 					  
    library(RSQLite)
	library(dplyr)
	library(rgdal)
   
     source("Modules/GetAgisoftShape.r")
   	 source("Modules/SQLiteWrite.r")
	 
	 ObserverCountDir=paste0(labelinput1,"\\Observer count")
	 ObserverCountP=list.files(ObserverCountDir,full.names=T,pattern=".shp")
	#ObserverCountP=paste0(labelinput1,"\\Observer count\\",basename(labelinput1),".shp")
	
    OPP_InfoTablePth=paste0(labelinput1,"\\Agisoft_IMG_INFO_",basename(labelinput1),".csv")
	
	date_1=basename(labelinput1)
   if (Species1=="NFSAdult") {SpeciesSQL="CU";PredictPointsP=paste0(labelinput1,"\\Predict\\NFSAdult_",date_1,".csv")}
   if (Species1=="SSLAdult") {SpeciesSQL="EJ";PredictPointsP=paste0(labelinput1,"\\Predict\\",date_1,"_SSLAdult_AgeLatLon.csv")}
   if (Species1=="NFSPup")   {SpeciesSQL="CU";PredictPointsP=paste0(labelinput1,"\\Predict\\NFSPup_",date_1,".csv")}
   if (Species1=="SSLPup")   {stop("You can not Import SSL pups without Adult animals, please select  SSLAdult")}
   if (Species1=="WLRS")    {SpeciesSQL="WLRS";PredictPointsP=paste0(labelinput1,"\\Predict\\WLRS_",date_1,".csv")}
   
   PredictPointsPsslp=paste0(labelinput1,"\\Predict\\SSLPup_",date_1,".csv")
   AdditnlPointsPnfsP = paste0(labelinput1,"\\Predict\\NFSPupAdd_",date_1,".kml")
   WLRSadd = paste0(labelinput1,"\\Predict\\WLRSadd_",date_1,".kml")
   
   dirModelCount=paste0(labelinput1,"\\Polygons\\Model")
   MdlPolPth=list.files(dirModelCount,full.names=T,pattern=".shp")
   #MdlPolPth=paste0(labelinput1,"\\Polygons\\Model\\Model.shp")
   
   if (file.exists(paste0(labelinput1,"\\",basename(labelinput1),".files"))==F) {GPSAltitude="No data";file_name="No data"} else {
  if (file.exists(OPP_InfoTablePth)==T) {OPP_InfoTable=read.csv(OPP_InfoTablePth)} else {OPP_InfoTable=GetIMGGEOINFOAgisoft(labelinput1)}
   Info=OPP_InfoTable[OPP_InfoTable$date==basename(labelinput1),]
   a=Info$Value[Info$Property==  "DJI/RelativeAltitude"]
   a=gsub("+","",a);a=gsub("-","",a)
   GPSAltitude=mean(as.numeric(a))
   file_name=unique(Info$Img)
}
#####################################################################################
 # for (i in 1: length(Effort$type_count)) {
 #  Count=NULL
 #  EffortSort=Effort[i,] 
 if (EffortSort$type_count=="Auto_count") {  # data for auto count
                  Count=read.csv(PredictPointsP);Count$X=NULL;if (Species1=="NFSPup"){Count$age="P"}
                 if(file.exists(PredictPointsPsslp)==T) {countP=read.csv(PredictPointsPsslp);countP$age="P"}
				 
				 if(file.exists(AdditnlPointsPnfsP)== T) {countP=data.frame(rgdal::readOGR(AdditnlPointsPnfsP,"Point Features"))
				                                         countP=data.frame(lat=countP$coords.x2,lon=countP$coords.x1,age="P")}
														 
				 if(file.exists(WLRSadd)==T) {countP=data.frame(rgdal::readOGR(WLRSadd,"Point Features"))
				                              countP=data.frame(lat=  countP$coords.x2,lon=countP$coords.x1,age="U")}
				 
				 if (exists("countP")){Count=rbind(Count,countP)}
				 Count$local_site="No_info"
				 Count<<-Count}		 
  
  observer=EffortSort$observer
  if (EffortSort$type_count %in% c("Manual_full_count","Manual_model_count")) { # data for manual count
 
  if (length(ObserverCountP)>1) {ObserverCountP=paste0(labelinput1,"\\Observer count\\",basename(labelinput1),"_",observer,".shp")}
   
               if (file.exists(ObserverCountP)==F) {stop(paste0("No observer count data, but Effort exists ",labelinput1,"   ",observer))}
                Count1= data.frame(shapefile(ObserverCountP))
                Count=data.frame(lon=Count1$coords.x1, lat= Count1$coords.x2,age= Count1$LAYER)
				Count$local_site="No_info"
              }
  if (EffortSort$type_count==	"Manual_model_count") {               # upload model poligons in count data
         ModelPol=shapefile(MdlPolPth)
         NSubPol=length(ModelPol@polygons)
          DF=NULL
         for (i in 1:NSubPol) {
           SubPol1=ModelPol@polygons[i]
           Coords=SubPol1[[1]]@Polygons[[1]]@coords
           SubDF=data.frame(age="MP",
                  local_site=paste0(i,"_",c(1:length(Coords[,1]))),
				  lat=Coords[,1],
				  lon=Coords[,2])
           DF<<-rbind(SubDF,DF)
}
    Count<-rbind(Count,DF)                                                      #ADD model pol to model count
  }		  
				
	    
   Count$age=gsub("AN","An",Count$age)
   Count$age=gsub("Unk","U",Count$age)
   Count$age=gsub("SA","Sa",Count$age)
   Count$age=gsub("DF","U",Count$age)
  
  Count1 =Count %>% 
         group_by(lon,lat,age,local_site)%>% 
		 summarize(n=n()) 
		#filter(n>1)
		
	Error <<- Count1 %>% group_by(lon,lat)%>% summarize(n=n()) %>% filter(n>1)
	Count2=	Count1[!(Count1$lat %in% Error$lat),]
	Count3=Count2
	
    # Count3= data.frame(lon=Count2$lon, lat= Count2$lat,age= Count2$age)	  
###################

  r_date1=paste0(substr(EffortSort$date,0,4),"-",substr(EffortSort$date,5,6),"-",substr(EffortSort$date,7,8))
#############################
 Subdata <-list(
    GPSAltitude=GPSAltitude,
    site=strsplit(basename(pthOPP),"_")[[1]][2],
    r_year =  format(as.POSIXct(strptime(EffortSort$date, "%Y%m%d")),"%Y"),          
    r_date= r_date1,
    time_start= EffortSort$time_start , 
    observer =EffortSort$observer,
    animal_type=   Count3$age,
    iLeft=Count3$lon,
    iTop=Count3$lat,
    file_nameOPP=labelinput1,
    ########################################   photo_count_list
    visibility= EffortSort$visibility,
    CommentsPCL= EffortSort$Comments,
    rain= EffortSort$rain,
    distance= EffortSort$distance,
	type_count=EffortSort$type_count,
    type= EffortSort$type,
    quality= EffortSort$quality,
    splash= EffortSort$splash,
    Species1= SpeciesSQL,
    #######################################   photo_count_files
    CommentsPCF=labelinput1,      
    file_name= file_name, #unique(Info$Img),
	local_site=Count3$local_site
  )
  return(Subdata)
}
##################################################################################################
#####################################################################data collect in each sub fold
    CheckSubFold1=dir.exists(paste0(labelInput,"\\Predict"));CheckSubFold2=dir.exists(paste0(labelInput,"\\Observer count"))
    if (CheckSubFold1==F & CheckSubFold2==F) {SubFold=T} else {SubFold=F} 
      if(SubFold==T) {listSubfold=list.dirs(labelInput,full.names=T,recursive=F)}
      if(SubFold==F) {listSubfold=labelInput}
	  
      EffortH<<-read.csv(KK_Effort,sep=",")
	  if (nchar(basename(labelInput))>8) {
      DtimSur=basename(labelInput)
      DtimSur1=strsplit(DtimSur,split="_")                       # it is for sub fold, extract prefix 01 or 02
      DtimSur2=paste0(DtimSur1[[1]][1],"_",DtimSur1[[1]][2])
	  } else {DtimSur2=basename(labelInput)}                    # swich if labelinput jast 8 digit or with prefix time and else
      Effort= EffortH[EffortH$date==basename(DtimSur2),]

if (length(Effort$observer)==0) {stop(paste0("No Effort found for ",DtimSur2))}
	  
	  
	  
 for (i in 1: length(Effort$type_count)) { #each type count write for diggerent photo_count_list 
       
	   EffortSort<<-Effort[i,] 
 
   for (y in 1:length (listSubfold)) {
      labelinput1<<-listSubfold[y]
        Subdata=PredsWriteToSQL(labelinput1)
        if (y==1){FinData=Subdata} else {
  
         FinData$animal_type= c(FinData$animal_type,Subdata$animal_type)  
         FinData$iLeft= c(FinData$iLeft,Subdata$iLeft)
         FinData$iTop= c(FinData$iTop,Subdata$iTop)
	     FinData$local_site=c(FinData$local_site,Subdata$local_site)
     }
	 FinData<<-FinData
#	 CheckError=paste0(FinData$iLeft,"_",FinData$iTop)
#	 Error1 =Error %>% group_by(iLeft,iTop)%>% summarize(n=n()) %>% filter(n>1)
#	Count2=	Count1[!(Count1$lat %in% Error$lat),]
	 
	 }
 ####################################################   data write sum result  
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
    file_nameOPP=FinData$file_nameOPP,
    ########################################   photo_count_list
    visibility= FinData$visibility,
    CommentsPCL= FinData$CommentsPCL,
    rain= FinData$rain,
    distance= FinData$distance,
	type_count=FinData$type_count,
    type= FinData$type,
    quality= FinData$quality,
    splash= FinData$splash,
    species= FinData$Species1,
    #######################################   photo_count_files
    CommentsPCF=FinData$CommentsPCF,      
    file_name= FinData$file_name,
	local_site=FinData$local_site
  )
}
 	
    
    

