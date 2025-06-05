library(rgdal)



 labelInput# = "D:\\NFS_DB\\2024_138_OPP\\20240613_124327"
 date1=substr(basename(labelInput),1,15)
 mnth<<-substr(basename(labelInput),5,6)
 Species= "NFSAdult"
 USE_FUUL_GAREM_MALE_COUNT = TRUE
  LIMIT = 0.92
  limtAN = 0.92
  crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

  kmlPathSave=paste0(labelInput,"\\Predict\\","NFSAdult", "_", date1, ".kml")
  AgePredOptimisation_pth = paste0(labelInput,"\\Predict\\","NFSAdult_AgePredOptimisation", "_", date1,".csv")   
  if (file.exists(AgePredOptimisation_pth)==F){source ("C:\\Users\\usato\\SSL_DB\\AUC\\Modules\\Age_PREDICT_NFS.r")}
 # save_pth_check_diff=paste0(labelInput,"\\Predict\\Check_difference",Species,"_", date1, ".csv")
  ModelPoligonDIR=paste0(labelInput,"\\Polygons\\Model")
  ObserverPointDIR=paste0(labelInput,"\\Observer_count")
  Exlude_polygonDir=paste0(labelInput, "\\Polygons\\Exclude")
  
   if(dir.exists(Exlude_polygonDir)==T){Exlude_polygon_pth<-list.files(Exlude_polygonDir,full.names=T,pattern="shp|kml")}
  ObserverPointPTH=list.files(ObserverPointDIR,full.names=T,pattern="shp|kml")[1] 
  ModelPoligonPTH=list.files(ModelPoligonDIR,full.names=T,pattern="shp|kml")
  
  #######################################################################################################################################
 #  difference=read.csv(save_pth_check_diff)
   APO_r=read.csv(AgePredOptimisation_pth)
   APO_r$name=""
  
   APO_r=APO_r[APO_r$Rookery=="Rookery",]
  
   APO_r$name[APO_r$AN >= LIMIT]="AN"
   APO_r$name[APO_r$AN < LIMIT]="Bch"
   
    APO_r$name[ APO_r$name== "AN"]="TF"
	APO_r$name[APO_r$name== "Bch"]="F" 

   Observer=readOGR(ObserverPointPTH)
   
   if (file.exists(Exlude_polygon_pth)){ polygon_exlude=readOGR(Exlude_polygon_pth)} else {stop("No Exlude_polygon  found")}
   proj4string(polygon_exlude) <- crs	
	
    coords <- data.frame(lat= APO_r$lat, lon=APO_r$lon)   
    data   <- data.frame(AN=APO_r$AN, Bch=APO_r$Bch, Rookery=APO_r$Rookery,name = APO_r$name)   # data

      APO_r1 <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
		proj4string(APO_r1) <- crs
		APO_r1=	spTransform(APO_r1,crs)

	    rk = APO_r1[is.na(over(APO_r1,as(polygon_exlude,"SpatialPolygons"))),] 

		ModelPoligon1=readOGR(ModelPoligonPTH)
		proj4string(ModelPoligon1) <- crs
		ModelPoligon1=	spTransform(ModelPoligon1,crs)

	    proj4string(Observer) <- crs
        Observer=	spTransform(Observer,crs)
#####	
  if (USE_FUUL_GAREM_MALE_COUNT == FALSE){ 
      rk3= rk[!is.na(over(rk,as(ModelPoligon1,"SpatialPolygons"))),]                    # if use model NFS Garem Male Count 
      OBS = Observer[!is.na(over(Observer,as(ModelPoligon1,"SpatialPolygons"))),]
  } 
  if (USE_FUUL_GAREM_MALE_COUNT == TRUE){ 
      rk3=rk
      OBS = Observer
   }
   
    auto=length(rk3$name[rk3$name=="TF"])
    obs=length(OBS$LAYER[OBS$LAYER=="TF"])
  
TFdiff=auto-obs
  #while (TFdiff ==0){
######## 
for (i in 1:10000){
 
	if (TFdiff>0){
	 LIMIT=LIMIT+0.0001
	 rk3$name[rk3$AN < LIMIT]="F"; 
	 rk3$name[rk3$AN >= LIMIT]="TF"
    
	}
	
	if (TFdiff<0){
     LIMIT=LIMIT-0.0001
     rk3$name[rk3$AN < LIMIT]="F"
	 rk3$name[rk3$AN  >= LIMIT]="TF"
    
  }
   
   auto=length(rk3$name[rk3$name=="TF"])
   obs=length(OBS$LAYER[OBS$LAYER=="TF"])
  
TFdiff=auto-obs
TFdiff<-TFdiff
  }
  
   print(paste("TF diff Observer-Auto is  ", TFdiff))
   print(paste("TF pred limit is  ", LIMIT))
  # APO2
   LIMIT
     
     rk$name[rk$AN < LIMIT]="F"
	 rk$name[rk$AN  >= LIMIT]="TF"
 
    
############################################################################################ AN predict optimisation using Model AN count

   AP_h=read.csv(AgePredOptimisation_pth)
   AP_h=AP_h[AP_h$Rookery=="Haulout",]
   AP_h$name=""
   
   AP_h$name[AP_h$AN >= LIMIT]="AN"
   AP_h$name[AP_h$AN < LIMIT]="Bch"
   
   Observer=readOGR(ObserverPointPTH)
   
   if (file.exists(Exlude_polygon_pth)){ polygon_exlude=readOGR(Exlude_polygon_pth)} else {stop("No Exlude_polygon  found")}
   proj4string(polygon_exlude) <- crs	
	
    coords <- data.frame(lat= AP_h$lat, lon=AP_h$lon)   
    data   <- data.frame(AN=AP_h$AN, Bch=AP_h$Bch, Rookery=AP_h$Rookery,name = AP_h$name)   # data

      AP_h1 <- SpatialPointsDataFrame(coords = coords,
                                        data = data, 
                                        proj4string = crs)
		proj4string(AP_h1) <- crs
		AP_h1=	spTransform(AP_h1,crs)
        
	
	    Hl = AP_h1[is.na(over(AP_h1,as(polygon_exlude,"SpatialPolygons"))),] 
	
		ModelPoligon1=readOGR(ModelPoligonPTH)
		proj4string(ModelPoligon1) <- crs
		ModelPoligon1=	spTransform(ModelPoligon1,crs)
	    proj4string(Observer) <- crs
        Observer=	spTransform(Observer,crs)
#####		
      AP_h3= Hl[!is.na(over(Hl,as(ModelPoligon1,"SpatialPolygons"))),]                    # if use model NFS Garem Male Count 
      OBS = Observer[!is.na(over(Observer,as(ModelPoligon1,"SpatialPolygons"))),]
      OBS$LAYER[OBS$LAYER=="An"]="AN"
	  
    autoAN=length(AP_h3$name[AP_h3$name=="AN"])
    obsAN=length(OBS$LAYER[OBS$LAYER=="AN"])
  
  ANdiff=autoAN-obsAN
  #while (TFdiff ==0){
######
for (i in 1:10000){
	if (ANdiff>0){
	 limtAN=limtAN+0.0001
	 AP_h3$name[AP_h3$AN < limtAN]="Bch"
	 AP_h3$name[AP_h3$AN >= limtAN]="AN"
 
	}
	
	if (ANdiff<0){
     limtAN=limtAN-0.0001
     AP_h3$name[AP_h3$AN < limtAN]="Bch"
	 AP_h3$name[AP_h3$AN  >= limtAN]="AN"

  } 
   autoAN=length(AP_h3$name[AP_h3$name=="AN"])
   obsAN=length(OBS$LAYER[OBS$LAYER=="AN"])
  
ANdiff=autoAN-obsAN
ANdiff<-ANdiff
  }
 ################ 

   print(paste("AN diff Observer-Auto is  ", ANdiff))
   print(paste("AN pred limit is  ", limtAN))
   
   
   
   Hl$name[Hl$AN < limtAN]="Bch" 
   Hl$name[Hl$AN  >= limtAN]="AN"
  
  # Hl$name[Hl$Rookery=="Haulout" & Hl$name== "AN"]="AN"
   #Hl$name[Hl$Rookery=="Haulout" & Hl$name== "Bch"]="Bch"


   AP_fin=rbind(Hl,rk)



     AP_fin$age=AP_fin$name
	 AP_fin$Description= AP_fin$age
     AP_fin$name=NULL
	 AP_fin$AN=NULL         
	 AP_fin$Bch=NULL  
	 AP_fin$Rookery=NULL 

	
	
	cnt=length(AP_fin$Description)
	exludedAnimals=length(AP_fin$Rookery)-cnt
	
	print(paste0(exludedAnimals, "  Fur seal were exluded from prediction by exlude polygons"))
	print(paste0(cnt,"   NFS predicted"))
	 

    unlink(kmlPathSave)
    writeOGR(AP_fin,kmlPathSave,driver="KML", layer="Description")

  
  
  
  
  
  
  