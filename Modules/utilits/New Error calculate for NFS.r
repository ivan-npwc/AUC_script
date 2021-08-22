library(spatialEco)
library(dplyr)
library(raster)

 

 OPPdir="F:\\NFS_2020\\2020_138_OPP"
  Species1="NFSAdult"
  Species="NFSAdult"
listOPP=list.files( OPPdir,full.names=T)

for (y in 1:length(listOPP)) {

ModelPoligonPTH=NULL
HouloutPoligonPTH=NULL
RookeryPolygonPTH=NULL
ObserverPointPTH=NULL



  labelInput=listOPP[y]
  labelInput1=labelInput
  save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species1,"_", basename(labelInput), ".csv")

						ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
                        HouloutPoligonDIR=paste0(labelInput,"\\Polygons\\Houlout")
                        RookeryPolygonDIR=paste0(labelInput,"\\Polygons\\Rookery")
                        ObserverPointDIR=paste0(labelInput,"\\Observer count")
                        PredictPointPTH = paste0(labelInput,"\\Predict\\", Species,"_", basename(labelInput), ".csv")
                      


   if (dir.exists(ObserverPointDIR) & dir.exists(ModelPoligonDIR)) {   #это не верно !!!! если не находит, использует прошлый !!!!!!
ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern=".shp")
HouloutPoligonPTH=list.files(HouloutPoligonDIR,full.names=T,pattern=".shp")
RookeryPolygonPTH=list.files(RookeryPolygonDIR,full.names=T,pattern=".shp")
ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern=".shp") 
    
if (length(ModelPoligonPTH)>1 | length(HouloutPoligonPTH)>1 | length(RookeryPolygonPTH)>1 | length(ObserverPointPTH)>1) {
 stop("Only one shape file must by in one folder") }


		
		  ObserverPoint= shapefile(ObserverPointPTH)
          PredictPoint1=read.csv(PredictPointPTH)	
		  ModelPoligon1=shapefile(ModelPoligonPTH)
		 
		  proj4string(ModelPoligon1) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
		  proj4string(ObserverPoint) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
		  ObserverPoint0=   point.in.poly(ObserverPoint,ModelPoligon1)
		 ObserverPoint0_1=as.data.frame(ObserverPoint0)
    if (length(ObserverPoint0_1$HEXID) !=0) { ObserverPoint0_2=ObserverPoint0_1[is.na(ObserverPoint0_1$HEXID) == F,]
       ObserverPoint1=data.frame(lon=ObserverPoint0_2$coords.x1, lat= ObserverPoint0_2$coords.x2,   age= as.factor(ObserverPoint0_2$LAYER))
	}
	if (length(ObserverPoint0_1$HEXID)==0) { ObserverPoint0_2=ObserverPoint0_1[is.na(ObserverPoint0_1$LAYER.y) == F,]
       ObserverPoint1=data.frame(lon=ObserverPoint0_2$coords.x1, lat= ObserverPoint0_2$coords.x2,   age= as.factor(ObserverPoint0_2$LAYER.x))	}
	
		  
   ObserverPoint1$age=gsub("Sa","Bch",ObserverPoint1$age)
   ObserverPoint1$age=gsub("J","Bch",ObserverPoint1$age)
   ObserverPoint1$age=gsub("TN","An",ObserverPoint1$age)
   ObserverPoint2=ObserverPoint1 %>% group_by(age) %>% summarise(ObserverCount=n())
	ObserverPoint2=ObserverPoint2[ObserverPoint2$age != "Tr",]	 

		
        coords <- data.frame(lat= PredictPoint1$lon, lon=PredictPoint1$lat)   
        data   <- data.frame(age= PredictPoint1$age)   # data
        crs    <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") 
       PredictPoints <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
										
 

    PredictPoint2 <- point.in.poly(PredictPoints,ModelPoligon1)
    PredictPoint3=as.data.frame(PredictPoint2)
if (length(PredictPoint3$HEXID) !=0){	
    PredictPoint4=PredictPoint3[is.na(PredictPoint3$HEXID) == F,]
    PredictPoint5=data.frame(age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1) 
	                             }

if (length(PredictPoint3$HEXID)==0){	
    PredictPoint4=PredictPoint3[is.na(PredictPoint3$LAYER) == F,]
    PredictPoint5=data.frame(age=PredictPoint4$age,lat=PredictPoint4$coords.x2,lon=PredictPoint4$coords.x1) 
	                             }

								 
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
	WriteReport=data.frame(Report$Compare1,date1=basename(labelInput1),Species=Species1)	 
	write.csv(WriteReport, save_pth_check_diff,row.names=F)
print(Report)	
           
}  else {print("Not enough data to error calculate")}


}