
PredsPrepareSQL=function (
                         labelinput1
                         # Species=Species,
                         # SQLite_path 
						  ) { 
 
     r_date1=paste0(substr(EffortSort$date,0,4),"-",substr(EffortSort$date,5,6),"-",substr(EffortSort$date,7,8))
     observer=EffortSort$observer 
     time_start= EffortSort$time_start
     crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
     Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Haulout");HPF<-list.files(Haulout_polygonDir,full.names=T,pattern=".shp")
     Haulout_polygon=shapefile(HPF)  
     proj4string(Haulout_polygon) <- CRS(crs)
  ObserverCountDir=paste0(labelinput1,"\\Observer count")
  ObserverCountP=list.files(ObserverCountDir,full.names=T,pattern=".shp")
  #ObserverCountP=paste0(labelinput1,"\\Observer count\\",basename(labelinput1),".shp")
  
  OPP_InfoTablePth=paste0(labelinput1,"\\Agisoft_IMG_INFO_",basename(labelinput1),".csv")
  
  date_1=basename(labelinput1)
  if (Species=="NFSAdult") {SpeciesSQL="CU";PredictPointsP=paste0(labelinput1,"\\Predict\\NFSAdult_",date_1,".csv")}
  if (Species=="SSLAdult") {SpeciesSQL="SSL";PredictPointsP=paste0(labelinput1,"\\Predict\\",date_1,"_SSLAdult_AgeLatLon.csv")}
  if (Species=="NFSPup")   {SpeciesSQL="CU";PredictPointsP=paste0(labelinput1,"\\Predict\\NFSPup_",date_1,".csv")}
  if (Species=="SSLPup")   {stop("You can not Import SSL pups without Adult animals, please select  SSLAdult")}
  if (Species=="WLRS")    {SpeciesSQL="WLRS";PredictPointsP=paste0(labelinput1,"\\Predict\\WLRS_",date_1,".csv")}
  
  PredictPointsPsslp=paste0(labelinput1,"\\Predict\\SSLPup_",date_1,".csv")
  AdditnlPointsPnfsP = paste0(labelinput1,"\\Predict\\NFSPupAdd_",date_1,".kml")
  WLRSadd = paste0(labelinput1,"\\Predict\\WLRSadd_",date_1,".kml")
  
  dirModelCount=paste0(labelinput1,"\\Polygons\\Model")
  MdlPolPth=list.files(dirModelCount,full.names=T,pattern=".shp")
  #MdlPolPth=paste0(labelinput1,"\\Polygons\\Model\\Model.shp")
  InfoDayTable=NULL
  if (file.exists(paste0(labelinput1,"\\",basename(labelinput1),".files"))==F) {
  
   #   InfoDayTable$latitude="No data"
   #   InfoDayTable$="No data"
   #   InfoDayTable$="No data"
   #   GPSAltitude="No data"
   #   file_name="No data"
	  
	  
	  
	  } else {
    if (file.exists(OPP_InfoTablePth)==T) {OPP_InfoTable=read.csv(OPP_InfoTablePth)} else {OPP_InfoTable=GetIMGGEOINFOAgisoft(labelinput1)}
    Info=OPP_InfoTable#[OPP_InfoTable$date==basename(labelinput1),]
    
    a=Info$Value[Info$Property==  "DJI/RelativeAltitude"]
    a=gsub("+","",a);a=gsub("-","",a)
    GPSAltitude=mean(as.numeric(a))
    
    file_name= basename(as.character(unique(Info$Img)))
    
    
    LatLon =Info[Info$Property %in% c("Exif/GPSLongitude","Exif/GPSLatitude",'DJI/RelativeAltitude'),]
    
    InfoDayTable=NULL
    for (y in 1:length(file_name)){
      imgN=file_name[y]
      tbl=LatLon[LatLon$IMGbasename==imgN,]
      lat=tbl$Value[tbl$Property =='Exif/GPSLatitude'][1]
      lon=tbl$Value[tbl$Property =='Exif/GPSLongitude'][1]
      alt=tbl$Value[tbl$Property =='DJI/RelativeAltitude'][1]
      preTb=data.frame(file_name=imgN,latitude=lat,longitude=lon,altitude=alt)
      InfoDayTable=rbind(preTb,InfoDayTable)
    }
  } 
  #####################################################################################
 # for (i in 1: length(EffortDay$type_count)) {
 #   Count=NULL
 #   EffortSort=EffortDay[i,]

    
    if (EffortSort$type_count=="opp_auto_count") {  ################################################################## data for auto count
      Count=read.csv(PredictPointsP);Count$X=NULL;if (Species=="NFSPup"){Count$age="P"}
      if(file.exists(PredictPointsPsslp)==T) {countP=read.csv(PredictPointsPsslp);countP$X=NULL;age="P"}
      
      if(file.exists(AdditnlPointsPnfsP)== T) {countP=data.frame(rgdal::readOGR(AdditnlPointsPnfsP,"Point Features"))
      countP=data.frame(lat=countP$coords.x2,lon=countP$coords.x1,age="P")}
      
      if(file.exists(WLRSadd)==T) {countP=data.frame(rgdal::readOGR(WLRSadd,"Point Features"))
      countP=data.frame(lat=  countP$coords.x2,lon=countP$coords.x1,age="U")}
      
      if (exists("countP")){Count=rbind(Count,countP)}
      Count$local_site="U"
    }		 
   ################################################## 
    
    if (EffortSort$type_count %in% c("opp_manual_full_count","opp_manual_model_count","manual_count_full")) { ################################### data for manual count
      
     if (length(ObserverCountP)>1) {ObserverCountP=paste0(labelinput1,"\\Observer count\\",basename(labelinput1),"_",observer,".shp")}
      
      if (file.exists(ObserverCountP,showWarnings=F)) {
        Points= shapefile(ObserverCountP)
		proj4string(Points) <- CRS(crs)
		index= Points %over% Haulout_polygon  # FILTER POINTS ONLY INSIDE HAUL POL
        Points$index=index[,1]
        Points1=Points[is.na(Points$index)==F,]
        latlon=data.frame(coordinates(Points1))
		latlon$age=data.frame(Points1)$LAYER
        Count=data.frame(lon=latlon$coords.x1, lat= latlon$coords.x2,age= latlon$age)
        Count$local_site="U"
      } else{print(paste0("No observer count data, but Effort exists ",labelinput1,"   ",observer))}
      
    }
    
    #########################################################################ADD MODEL POLYGONS
    if (EffortSort$type_count==	"opp_manual_model_count" & file.exists(MdlPolPth,showWarnings=F)  & file.exists(ObserverCountP,showWarnings=F)) { 
      ModelPol=shapefile(MdlPolPth)
      projection= proj4string(ModelPol)
      NSubPol=length(ModelPol@polygons)
      polygons_model_sites=NULL
      for (i in 1:NSubPol) {
        SubPol1=ModelPol@polygons[i]
        Coords=SubPol1[[1]]@Polygons[[1]]@coords
        SubDF=data.frame(id=i,order_point=c(1:length(Coords[,1])), latitude=Coords[,1],longitude=Coords[,2])
        polygons_model_sites<-rbind(SubDF,polygons_model_sites)		   
      }  
      polygons_model_sites$site=site
      polygons_model_sites$species=SpeciesSQL 
      polygons_model_sites$r_date=r_date1
      polygons_model_sites$time_start=paste0(time_start)
      polygons_model_sites$projection=projection
      
      sqlite    <- dbDriver("SQLite")
      SSL <- dbConnect(sqlite,   SQLite_path)	
      pmsE= dbReadTable(SSL,"polygons_model_sites")
      pms= polygons_model_sites 
      New= pms[!(pms$id %in% pmsE$id &  pms$order_point %in% pmsE$order_point & 
                   pms$latitude %in% pmsE$latitude  &  pms$longitude %in% pmsE$longitude  & 
                   pms$species %in% pmsE$species  &  pms$r_date %in% pmsE$r_date  &  pms$time_start %in% pmsE$time_start),]
      if (length(pms$id) !=0) {dbWriteTable(SSL, "polygons_model_sites", New, append=T)}					  
    }
    ############################################################			    
    Count$age=gsub("An","AN",Count$age)
    Count$age=gsub("Unk","U",Count$age)
    Count$age=gsub("Sa","SA",Count$age)
    Count$age=gsub("Bch","SA",Count$age)
    Count$age=gsub("DF","U",Count$age)
    
    Count1 =Count %>% 
      group_by(lon,lat,age,local_site)%>% 
      summarize(n=n()) 
    #filter(n>1)
    
    Error <- Count1 %>% group_by(lon,lat)%>% summarize(n=n()) %>% filter(n>1)   # NEED REMOVE ONLY ONE DOUBLE POINT !
    Count2=	Count1[!(Count1$lat %in% Error$lat  & Count1$lon %in% Error$lon),]
    Count3=Count2	  
    ###################
    #############################
    data1 <-list(
      GPSAltitude=GPSAltitude,
      site= site, # strsplit(basename(pthOPP),"_")[[1]][2],
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
      Species= SpeciesSQL,
      #######################################   photo_count_files
      latitude=InfoDayTable$lat,
      longitude=InfoDayTable$lon,
      altitude=InfoDayTable$altitude,
      CommentsPCF=labelinput1,      
      file_name= file_name, #unique(Info$Img),
      local_site=Count3$local_site
    )
    return(data1)
  }
