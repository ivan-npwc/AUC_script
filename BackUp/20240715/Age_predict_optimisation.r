library(rgdal)



 labelInput  #= "C:\\Users\\usato\\NFS_DB\\2024_138_OPP\\20240704_085125"
 date1=substr(basename(labelInput),1,15)
 mnth<<-substr(basename(labelInput),5,6)
 Species= "NFSAdult"
 PredModelCount_pth=paste0(labelInput,"\\Predict\\PredModelCoun",Species,"_", date1, ".kml")
 
#source("Modules/KMLwrite_function.r")
 
  
  LIMIT = 0.92
  crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

  kmlPathSave=paste0(labelInput,"\\Predict\\","NFSAdult", "_", date1, ".kml")
  AgePredOptimisation_pth = paste0(labelInput,"\\Predict\\","NFSAdult_AgePredOptimisation", "_", date1,".csv")   
 # save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species,"_", date1, ".csv")
  ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
  ObserverPointDIR=paste0(labelInput,"\\Observer_count")
  Exlude_polygonDir=paste0(labelInput, "\\Polygons\\Exclude")
  
   if(dir.exists(Exlude_polygonDir)==T){Exlude_polygon_pth<-list.files(Exlude_polygonDir,full.names=T,pattern="shp|kml")}
  ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern="shp|kml")[1] 
  ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern="shp|kml")
  
 #  difference=read.csv(save_pth_check_diff)
   APO=read.csv(AgePredOptimisation_pth)
   APO$name=""
   
   APO$name[APO$AN >= LIMIT]="AN"
   APO$name[APO$AN < LIMIT]="Bch"
   
    APO$name[APO$Rookery=="Rookery" & APO$name== "AN"]="TF"
	APO$name[APO$Rookery=="Rookery" & APO$name== "Bch"]="F" 
   
   
   
   Observer=readOGR(ObserverPointPTH)
   
   if (file.exists(Exlude_polygon_pth)){ polygon_exlude=readOGR(Exlude_polygon_pth)} else {stop("No Exlude_polygon  found")}
   proj4string(polygon_exlude) <- crs	
	
    coords <- data.frame(lat= APO$lat, lon=APO$lon)   
    data   <- data.frame(AN=APO$AN, Bch=APO$Bch, Rookery=APO$Rookery,name = APO$name)   # data

      APO1 <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
		APO1=	spTransform(APO1,crs)
        proj4string(APO1) <- crs
	
	    APO2 = APO1[is.na(over(APO1,as(polygon_exlude,"SpatialPolygons"))),] 
	
		ModelPoligon1=readOGR(ModelPoligonPTH)
		
		proj4string(ModelPoligon1) <- crs	
        ModelPoligon1=	spTransform(ModelPoligon1,crs)		
       
	    proj4string(Observer) <- crs
        Observer=	spTransform(Observer,crs)
		
   
  APO3= APO2[!is.na(over(APO2,as(ModelPoligon1,"SpatialPolygons"))),]
  OBS = Observer[!is.na(over(Observer,as(ModelPoligon1,"SpatialPolygons"))),]
   

   
    auto=length( APO3$name[APO3$name=="TF"])
    obs=length(OBS$LAYER[OBS$LAYER=="TF"])
  
TFdiff=auto-obs
  #while (TFdiff ==0){
  
for (i in 1:10000){
 
	if (TFdiff>0){
	 LIMIT=LIMIT+0.0001
	APO3$name[APO3$AN < LIMIT]="Bch"; APO3$name[APO3$AN >= LIMIT]="AN"
     APO3$name[APO3$Rookery=="Rookery" & APO3$name== "AN"]="TF"
	 APO3$name[APO3$Rookery=="Rookery" & APO3$name== "Bch"]="F"
	}
	
	if (TFdiff<0){
     LIMIT=LIMIT-0.0001
     APO3$name[APO3$AN < LIMIT]="Bch"; APO3$name[APO3$AN  >= LIMIT]="AN"
     APO3$name[APO3$Rookery=="Rookery" & APO3$name== "AN"]="TF"
	 APO3$name[APO3$Rookery=="Rookery" & APO3$name== "Bch"]="F" 
  }
   
   auto=length( APO3$name[APO3$name=="TF"])
   obs=length(OBS$LAYER[OBS$LAYER=="TF"])
  
TFdiff=auto-obs
TFdiff<<-TFdiff
  }
   print(paste("TF diff Observer-Auto is  ", TFdiff))
   APO2
   LIMIT
  
     APO2$name[APO2$AN < LIMIT]="Bch"; APO2$name[APO2$AN  >= LIMIT]="AN"
     APO2$name[APO2$Rookery=="Rookery" & APO2$name== "AN"]="TF"
	 APO2$name[APO2$Rookery=="Rookery" & APO2$name== "Bch"]="F"
     APO2$age=APO2$name
     APO2$name=NULL
	 APO2$AN=NULL         
	 APO2$Bch=NULL  
	 APO2$Rookery=NULL 
	 APO2$Description= APO2$age
	 
	
	
	cnt=length(APO2$Description)
	exludedAnimals=length(APO1$Rookery)-cnt
	
	print(paste0(exludedAnimals, "  Fur seal were exluded from prediction by exlude polygons"))
	print(paste0(cnt,"   NFS predicted"))
	 

    unlink(kmlPathSave)
    writeOGR(APO2,kmlPathSave,driver="KML", layer="Description")

  
  
  
  
  
  
  