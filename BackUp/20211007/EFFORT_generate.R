library(tools)

labelInput
System_data
Species

EffortNames=paste0(System_data,"\\EFFORT.csv")
EffortPth=paste0(System_data,"\\2020_138_SSL_Effort.csv")
################################################################################ EFFORT COPY TO DATA FOLDER
remCharter=basename(labelInput)
Gendir=gsub(remCharter,"",labelInput)
Datadir=gsub("OPP","Data",GenSSLdir); dir.create(Datadir,showWarnings=F)
EffortName1=paste0(basename(Datadir));EffortName2=gsub("Data","EFFORT",EffortName1);EffortName=paste0(EffortName2,"_",Species,".csv")
NewPthEffort=paste0(Datadir,"\\",EffortName)
file.copy(EffortPth,NewPthEffort)
################################################################################### GET LIST OPP
listOPPSub=NULL

listOPP=list.files(Gendir, full.names=T)
 for (i in 1:length(listOPP)) {
 OPPdir=listOPP[i]
   checkSubOPP=list.files(OPPdir, pattern=".psx")
   if(length(checkSubOPP) ==0) {listSubOPP=data.frame(pth=list.dirs(OPPdir,recursive=F))} else{listSubOPP=data.frame(pth=OPPdir)}
    listOPPSub=rbind(listOPPSub,listSubOPP)
 }
####################################################################
effort=NULL
ObsCount=NULL
for (i in 1:length(listOPPSub[,1])) {
 opp=as.character(listOPPSub[i,])
 datetime=substr(basename(opp),1,15)
 time1=strsplit(datetime,"_")[[1]][2]
 time2=paste0(substr(time1,1,2),":",substr(time1,3,4))
  if(file.exists(paste0(opp,"\\Predict\\",Species,"_",datetime,".kml"))){  # checkAutoCount
        effortAuto=data.frame(observer="Auto_count",date=datetime,time_start=time2,type_count="auto_count_full")
        effort=rbind(effort,effortAuto)
      }
	  ##############                                                    # checkModelCount   # ONLY ONE OBSERVER COUNT MUST EXISTS 
  CheckModelCount=list.files(paste0(opp,"\\Polygons\\Model"))                       
   if (length(CheckModelCount) !=0){
   if (dir.exists(paste0(opp,"\\Observer count"))){ObsCount=list.files(paste0(opp,"\\Observer count"))[1]}
   if (dir.exists(paste0(opp,"\\Observer_count"))){ObsCount=list.files(paste0(opp,"\\Observer_count"))[1]}  
     Observer1=  file_path_sans_ext(ObsCount)
     Observer2=strsplit(Observer1,"_")[[1]][3]
	 effortModel=data.frame(observer=Observer2,date=datetime,time_start=time2,type_count="manual_count_model")
	 effort=rbind(effort,effortModel)
}

############# 
   ObsCount=NULL                                         # CHECK OBSERVER FUUL COUNT
   if (dir.exists(paste0(opp,"\\Observer count"))){ObsCount=list.files(paste0(opp,"\\Observer count"))[1]}
   if (dir.exists(paste0(opp,"\\Observer_count"))){ObsCount=list.files(paste0(opp,"\\Observer_count"))[1]}

   if (is.null(ObsCount)!=T) {     #if observer count folder exists
   if (is.na(ObsCount) !=T) {      # if observer count folder is empty
     Observer1=  file_path_sans_ext(ObsCount)
     Observer2=strsplit(Observer1,"_")[[1]][3]
	 effortFULL=data.frame(observer=Observer2,date=datetime,time_start=time2,type_count="manual_count_full")
	 effort=rbind(effort,effortFULL)
   }
   }
}

effort$rain="UNKR"
effort$quality="UNKQ"
effort$splash="UNKS"
effort$visibility="L100"
effort$distance="L100"
effort$species=Species
effort$Comments="Auto effort generated"

write.csv(effort,NewPthEffort, row.names=F)























