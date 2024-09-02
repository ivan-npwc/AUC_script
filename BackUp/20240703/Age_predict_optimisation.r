



 labelInput
 date1=substr(basename(labelInput),1,15)
 month<<-substr(basename(labelInput),5,6)
 Species
 
 

 
  
  LIMIT = 0.92
  crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

  kmlPathSave=paste0(labelInput,"\\Predict\\","NFSAdult", "_", date1, ".kml")
  AgePredOptimisation_pth = paste0(labelInput,"\\Predict\\","NFSAdult_AgePredOptimisation", "_", date1,".csv")   
  save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species,"_", date1, ".csv")
  ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
  ObserverPointDIR=paste0(labelInput,"\\Observer_count")
  
  ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern="shp|kml")[1] 
  ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern="shp|kml")
  
   difference=read.csv(save_pth_check_diff)
   APO=read.csv(AgePredOptimisation_pth)
   ModelPoligon1=readOGR(ModelPoligonPTH)  
  
  #while (TFdiff ==0){
for (i in 1:10){
 
   
  
   TFdiff=difference[difference$age=="TF",]$diffInd
	
	if (TFdiff<0){
     LIMIT=LIMIT-0.1
     APO$name[APO$AN < LIMIT]="Bch"; APO$name[APO$AN > = LIMIT]="AN"
     APO$name[APO$Rookery=="Rookery" & APO$name== "AN"]="TF"
	 APO$name[APO$Rookery=="Rookery" & APO$name== "Bch"]="F"
  
   

  
  }
  source("Modules/Error_calculate.r")
  }
   
  
  
  
  
  
  
  
  
  