#labelInput1= "E:\\2019_138_OPP\\20190610"
#Species1= "SSLAdult"
labelInput1=labelInput
                        Species1=Species
						ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
                        HouloutPoligonDIR=paste0(labelInput,"\\Polygons\\Houlout")
                        RookeryPolygonDIR=paste0(labelInput,"\\Polygons\\Rookery")
                        ObserverPointDIR=paste0(labelInput,"\\Observer count")
                        PredictPointPTH = paste0(labelInput,"\\Predict\\", Species,"_", basename(labelInput), ".csv")
                        PredictPointPTH_SSL =  paste0(labelInput,"\\Predict\\",  basename(labelInput),"_",Species, "_AgeLatLon.csv")

if(dir.exists(HouloutPoligonDIR)==F){HouloutPoligonDIR=gsub("Houlout","Haulout",HouloutPoligonDIR)}
if(dir.exists(ObserverPointDIR)==F){ObserverPointDIR=paste0(labelInput,"\\Observer_count")}



   if (dir.exists(ObserverPointDIR) & dir.exists(ModelPoligonDIR)) {
ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern=".shp")
HouloutPoligonPTH=list.files(HouloutPoligonDIR,full.names=T,pattern=".shp")
RookeryPolygonPTH=list.files(RookeryPolygonDIR,full.names=T,pattern=".shp")
ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern=".shp") 
    
if (length(ModelPoligonPTH)>1 | length(HouloutPoligonPTH)>1 | length(RookeryPolygonPTH)>1 | length(ObserverPointPTH)>1) {
 stop("Only one shape file must by in one folder") }


library(spatialEco)
library(dplyr)
library(raster)


 
save_pth_check_diff=paste0(labelInput1,"\\Predict\\Check_difference",Species1,"_", basename(labelInput1), ".csv")
 
##################################################################################################################
  if (Species1=="SSLPup" & file.exists(ModelPoligonPTH)==F & file.exists(ObserverPointPTH)==T  & file.exists(PredictPointPTH)) {
         ObserverPoint= data.frame(shapefile(ObserverPointPTH))
          PredictPoint=read.csv(PredictPointPTH)	
ObserverPoint1=data.frame(lon=ObserverPoint$coords.x1, lat= ObserverPoint$coords.x2,   age= ObserverPoint$LAYER)  # type
ObserverPoint2=ObserverPoint1[ObserverPoint1$age == "P",]

ObserverPoint3=ObserverPoint2 %>% group_by(age) %>% summarise(ObserverCount=n())	
PredictPoint1=PredictPoint %>% group_by(age) %>% summarise(PredictCount=n())	
      Compare1 = merge(ObserverPoint3, PredictPoint1, by="age",all=T)
	Compare1$CountDiff= Compare1$PredictCount- Compare1$ObserverCount
	Compare1$PercentDiff= abs(Compare1$CountDiff)/Compare1$ObserverCount *100
	TottalErrorCount=sum(Compare1$ObserverCount,na.rm = T) - sum(Compare1$PredictCount,na.rm = T)
	TottalErrorPercent=TottalErrorCount/ sum(Compare1$ObserverCount,na.rm = T)  *100
		Report=list(date=basename(labelInput1),
		            Species1=Species1,
                     Compare1,
					 TottalErrorCount=TottalErrorCount,
					TottalErrorPercent= TottalErrorPercent)            
					}
##################################################################################################################
  if (Species1=="SSLAdult" & file.exists(ModelPoligonPTH)==F & file.exists(ObserverPointPTH)==T  & file.exists(PredictPointPTH_SSL)) {
         ObserverPoint= data.frame(shapefile(ObserverPointPTH))
          PredictPoint=read.csv(PredictPointPTH_SSL)	
ObserverPoint1=data.frame(lon=ObserverPoint$coords.x1, lat= ObserverPoint$coords.x2,   age= ObserverPoint$LAYER)  # type
ObserverPoint2=ObserverPoint1[ObserverPoint1$age != "P" & ObserverPoint1$age != "DP",]
ObserverPoint2$age=gsub("TN","TF",ObserverPoint2$age)
ObserverPoint3=ObserverPoint2 %>% group_by(age) %>% summarise(ObserverCount=n())	
PredictPoint1=PredictPoint %>% group_by(age) %>% summarise(PredictCount=n())	
      Compare1 = merge(ObserverPoint3, PredictPoint1, by="age",all=T)
	Compare1$CountDiff= Compare1$PredictCount- Compare1$ObserverCount
	Compare1$PercentDiff= abs(Compare1$CountDiff)/Compare1$ObserverCount *100
	TottalErrorCount=sum(Compare1$ObserverCount,na.rm = T) - sum(Compare1$PredictCount,na.rm = T)
	TottalErrorPercent=TottalErrorCount/ sum(Compare1$ObserverCount,na.rm = T)  *100
		Report<<-list(date=basename(labelInput1),
		            Species1=Species1,
                     Compare1,
					 TottalErrorCount=TottalErrorCount,
					TottalErrorPercent= TottalErrorPercent)            
					}
##########################################################################################################
 if (Species1=="SSLAdult" & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)  & file.exists(PredictPointPTH_SSL)) {
         ObserverPoint= data.frame(shapefile(ObserverPointPTH))
		 ObserverPoint$age= ObserverPoint$LAYER
ObserverPoint=ObserverPoint[ObserverPoint$age != "P" & ObserverPoint$age != "DP",]		 
          PredictPoint=read.csv(PredictPointPTH)
           
		   ObserverPoint$age=gsub("Sa","Bch",ObserverPoint$age)
           ObserverPoint$age=gsub("J","Bch",ObserverPoint$age)
           ObserverPoint$age=gsub("TN","An",ObserverPoint$age)
           ObserverPoint2=ObserverPoint %>% group_by(age) %>% summarise(ObserverCount=n())
		   
		   
		     PredictPoint6=PredictPoint %>% group_by(age) %>% summarise(AutoCount=n())
	         Compare1=merge(ObserverPoint2,PredictPoint6,by="age",all=T)
			 
			 TottalErrorIndivid=abs(sum(Compare1$ObserverCount,na.rm = T)- sum(Compare1$AutoCount,na.rm = T))
			 TottalErrorPercent=TottalErrorIndivid/sum(Compare1$ObserverCount,na.rm = T)*100
			 
		     Report<<-list(date1=basename(labelInput1),
              Species1=Species1,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent		
			)
			
			print(Report)	
					}			
##############################################################################################################		
 if (Species1=="NFSAdult" & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {		
		  ObserverPoint= shapefile(ObserverPointPTH)
          PredictPoint1=read.csv(PredictPointPTH)	
		  ModelPoligon1=shapefile(ModelPoligonPTH)
		 
		  proj4string(ModelPoligon1) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
		  proj4string(ObserverPoint) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
		  ObserverPoint0=   point.in.poly(ObserverPoint,ModelPoligon1)
		 ObserverPoint0=as.data.frame(ObserverPoint0)
         ObserverPoint0=ObserverPoint0[is.na(ObserverPoint0$LAYER.y) == F,]
		
        coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
        data   <- data.frame(age= PredictPoint1$age)   # data
        crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
       PredictPoints <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
										
   ObserverPoint1=data.frame(lon=ObserverPoint0$coords.x1, lat= ObserverPoint0$coords.x2,   age= as.factor(ObserverPoint0$LAYER.x))
   ObserverPoint1$age=gsub("Sa","Bch",ObserverPoint1$age)
   ObserverPoint1$age=gsub("J","Bch",ObserverPoint1$age)
   ObserverPoint1$age=gsub("TN","An",ObserverPoint1$age)
   ObserverPoint2=ObserverPoint1 %>% group_by(age) %>% summarise(ObserverCount=n())

    PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
    PredictPoint3=as.data.frame(PredictPoint2)
    PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]
    PredictPoint5=data.frame(age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1)  
    PredictPoint6=PredictPoint5 %>% group_by(age) %>% summarise(AutoCount=n())
	Compare1=merge(ObserverPoint2,PredictPoint6,by="age",all=T)
	
	 
			 TottalErrorIndivid=abs(sum(Compare1$ObserverCount,na.rm = T)- sum(Compare1$AutoCount,na.rm = T))
			 TottalErrorPercent=TottalErrorIndivid/sum(Compare1$ObserverCount,na.rm = T)*100
	
	

Report=list(date1=basename(labelInput1),
             Species1=Species1,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent
			 )
           
}   
###########################################################################################
if (Species1 %in% c("WLRS","NFSPup")  & file.exists(ModelPoligonPTH) & file.exists(ObserverPointPTH)) {		



 ObserverPoint= shapefile(ObserverPointPTH)
          PredictPoint1=read.csv(PredictPointPTH)	
		  ModelPoligon1=shapefile(ModelPoligonPTH)
		 
		  proj4string(ModelPoligon1) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
		  proj4string(ObserverPoint) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
		  ObserverPoint0=   point.in.poly(ObserverPoint,ModelPoligon1)
		 ObserverPoint0=as.data.frame(ObserverPoint0)
         ObserverPoint0=ObserverPoint0[is.na(ObserverPoint0$LAYER.y) == F,]
		
        coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
        data   <- data.frame(age= PredictPoint1$age)   # data
        crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
       PredictPoints <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
										
   ObserverPoint1=data.frame(lon=ObserverPoint0$coords.x1, lat= ObserverPoint0$coords.x2,   age= as.factor(ObserverPoint0$LAYER.x))
   
   
   ObserverPoint2=ObserverPoint1 %>% group_by(age) %>% summarise(ObserverCount=n())

    PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
    PredictPoint3=as.data.frame(PredictPoint2)
    PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]
    PredictPoint5=data.frame(age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1)  
    PredictPoint6=PredictPoint5 %>% group_by(age) %>% summarise(AutoCount=n())
	Compare1=merge(ObserverPoint2,PredictPoint6,by="age",all=T)
	
	 
			 TottalErrorIndivid=abs(sum(Compare1$ObserverCount,na.rm = T)- sum(Compare1$AutoCount,na.rm = T))
			 TottalErrorPercent=TottalErrorIndivid/sum(Compare1$ObserverCount,na.rm = T)*100
	
	

Report<<-list(date1=basename(labelInput1),
             Species1=Species1,
              Compare1=Compare1,
			  TottalErrorIndivid=TottalErrorIndivid,
			  TottalErrorPercent=TottalErrorPercent
			 )
			 
 write.csv(Compare1,save_pth_check_diff); print(Report)          
}					
########################################################################################################
###########################################################################################
} else {print("Data insufficient to Error calculate")}

