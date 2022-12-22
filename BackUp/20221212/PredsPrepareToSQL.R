
#PredsPrepareSQL=function (labelinput1) { #labelinput1 - it is subfolder in general labelInput folder
					   
					     labelinput1 
						 labelInput
			             site
						 System_data
                         Species
                         SQLite_path
                         Count_type	
						 KK_Effort
						 

    source("Modules/GetAgisoftShape.r")	
	
     Count3=NULL
	 photo_count1=NULL
     photo_count_list1=NULL
     photo_count_files1=NULL
     datetime =substr(basename(labelInput),1,15)
	 date_1=substr(basename(labelinput1),1,15) # for subFold
	 creator=gsub(paste0(datetime,"_"),"",basename(labelInput))
     r_date1=paste0(substr(datetime,0,4),"-",substr(datetime,5,6),"-",substr(datetime,7,8))
     time_start= paste0(substr(datetime,10,11),":",substr(datetime,12,13),":",substr(datetime,14,15))
	 r_year =  format(as.POSIXct(strptime(r_date1, "%Y-%m-%d")),"%Y")
	 if(exists("datecreated")==F){ datecreated <<- paste0(format(Sys.time(), " %Y-%m-%d %H:%M:%S"),"  R")}
	 
     crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
     Haulout_polygonDir=paste0(labelinput1, "\\Polygons\\Haulout");HPF<-list.files(Haulout_polygonDir,full.names=T,pattern=".shp")
    if (length(HPF)==0){stop("No Haulout Polygon found")}
 	 Haulout_polygon=shapefile(HPF)  
     proj4string(Haulout_polygon) <- CRS(crs)
  ObserverCountDir=paste0(labelinput1,"\\Observer_count")
  ObserverCountP=list.files(ObserverCountDir,full.names=T,pattern = "kml|shp")
  if (Species=="NFSPup" & Count_type =="manual_count_model") {
  ModelCountDir=paste0(ObserverCountDir,"\\Model_count")
  ObserverCountP=list.files(ModelCountDir,full.names=T,pattern = "kml|shp")
  }
  ########################################################################################## NEED EXCLUDE LATER 
  DirLocSites=paste0(System_data,"\\polygons_local_sites\\",site);pthLcSt=list.files(DirLocSites,full.names=T,pattern = "kml|shp")
  localSite=readOGR(pthLcSt)
  proj4string(localSite) <- CRS(crs)
  
  OPP_InfoTablePth=paste0(labelinput1,"\\Agisoft_IMG_INFO_",basename(labelinput1),".csv")
  
  
  if (Species=="NFSAdult") {SpeciesSQL="CU";PredictPointsP=paste0(labelinput1,"\\Predict\\NFSAdult_",date_1,".kml")}
  if (Species=="SSLAdult") {SpeciesSQL="SSL";PredictPointsP=paste0(labelinput1,"\\Predict\\",date_1,"_SSLAdult_AgeLatLon.kml")}
  if (Species=="NFSPup")   {SpeciesSQL="CU";PredictPointsP=paste0(labelinput1,"\\Predict\\NFSPup_",date_1,".kml")}
  if (Species=="SSLPup")   {stop("You can not Import SSL pups without Adult animals, please select  SSLAdult")}
  if (Species=="WLRS")    {SpeciesSQL="WLRS";PredictPointsP=paste0(labelinput1,"\\Predict\\WLRS_",date_1,".kml")}
  
  PredictPointsPsslp=paste0(labelinput1,"\\Predict\\SSLPup_",date_1,".kml")
  AdditnlPointsPnfsP = paste0(labelinput1,"\\Predict\\NFSPupAdd_",date_1,".kml")
  WLRSadd = paste0(labelinput1,"\\Predict\\WLRSadd_",date_1,".kml")
  
  dirModelCount=paste0(labelinput1,"\\Polygons\\Model")
  MdlPolPth=list.files(dirModelCount,full.names=T,pattern=".shp")
  InfoDayTable=NULL
  Count=NULL
  ####################################################################################### GET INFO FOR foto_count_files
  ########################################################################################
  if (file.exists(paste0(labelinput1,"\\",date_1,".files"))==F) {
               #   InfoDayTable$latitude="No data"
               #   InfoDayTable$="No data"
               #   InfoDayTable$="No data"
               #   GPSAltitude="No data"
               #   file_name="No data  
	  } else {
    if (file.exists(OPP_InfoTablePth)==T) {OPP_InfoTable=read.csv(OPP_InfoTablePth)} else {OPP_InfoTable=GetIMGGEOINFOAgisoft(labelinput1)}
    Info=OPP_InfoTable
    a=Info$Value[Info$Property==  "DJI/RelativeAltitude"]
    a=gsub("+","",a);a=gsub("-","",a)
    GPSAltitude=mean(as.numeric(a))
    file_name= basename(as.character(unique(Info$Img)))
    LatLon =Info[Info$Property %in% c("Exif/GPSLongitude","Exif/GPSLatitude",'DJI/RelativeAltitude'),]
    
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
  ##################################################################################### COLLECT data for auto count
    if (Count_type=="auto_count_full") {
      observer=	"Auto_Count"
      Count1=readOGR(PredictPointsP)
      if(file.exists(PredictPointsPsslp)==T) {countP=readOGR(PredictPointsPsslp)}
      if(file.exists(AdditnlPointsPnfsP)== T) {countP=readOGR(AdditnlPointsPnfsP)}
      if(file.exists(WLRSadd)==T) {countP=readOGR(WLRSadd)}
      if (exists("countP")){Count1=rbind(Count1,countP)}
	  proj4string(Count1) <- CRS(crs)
	     pts1=data.frame(point.in.poly(Count1,localSite))
	     Count=data.frame(lon=pts1$coords.x1, lat= pts1$coords.x2,age= pts1$Description,local_site=pts1$LAYER, creator=observer)
    }		 
   ##################################################################################  COLLECT DATA MANUAL COUNT
   ##################################################################################
  options(warn=-1)
   if (Count_type %in% c("manual_count_model","manual_count_full")) { ################################### data for manual count
    if (length(ObserverCountP[1])!=0 & file.exists(ObserverCountP,showWarnings=F)) {
	#if (file.exists(MdlPolPth) & Count_type== "manual_count_full") {stop("You select manual_count_full BUT Model Polygons exists")}
	Count=NULL
	  for(i in 1:length(ObserverCountP)) {
        Points= readOGR(ObserverCountP[i])
		proj4string(Points) <- CRS(crs)
		observer=strsplit(file_path_sans_ext(basename(ObserverCountP[i])),"_")[[1]][3]
		
		pts = Points[!is.na(over(Points,as(Haulout_polygon,"SpatialPolygons"))),]
		options(warn=0)
		if (length(pts)==0){ print(labelinput1);stop("None of the points inside the Haulout polygon")}
		
		pts1=data.frame(point.in.poly(pts,localSite))
        ObsCount=data.frame(lon=pts1$coords.x1, lat= pts1$coords.x2,age= pts1$LAYER.x,local_site=pts1$LAYER.y, creator=observer)
		Count<<-rbind(Count,ObsCount)
        }
		} else {print(paste0("No observer count for,",labelinput1))
		}
        }		
    ############################################################################### ADD MODEL POLYGONS
    ###############################################################################
    if (Count_type ==	"manual_count_model" & file.exists(MdlPolPth[1],showWarnings=F)  & file.exists(ObserverCountP[1],showWarnings=F)) { 
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
  #################################################################### CORRECTION OF ERROR 	
#################################################################	
if (is.null(Count) == F) { # only if  data in Count
    Count$age=gsub("An","AN",Count$age)
    Count$age=gsub("Unk","U",Count$age)
    Count$age=gsub("Sa","SA",Count$age)
    Count$age=gsub("Bch","SA",Count$age)
    Count$age=gsub("DF","U",Count$age)
    
    C0 <<- Count %>% 
      group_by(across())%>% 
      summarize(n=n()) 
	  
	  C1 <<- C0 %>% group_by(lon, lat) %>%
	     summarize(age=first(age),
		        local_site=first(local_site),    
				creator=first(creator))
		 
		 
 ######################################################################
 visibility="UNKN" 
 rain  ="UNKN"
 distance ="UNKN"
 quality ="UNKN"
 splash  ="UNKN"
 if (file.exists(KK_Effort)) {
 Effort=read.csv(KK_Effort)
 Row = Effort %>% filter(observer==observer & date== datetime & type_count== Count_type)
  if (length(Row$observer)>0){
 visibility= Row$visibility
 rain  = Row$rain
 distance = Row$distance
 quality = Row$quality
 splash  = Row$splash
  }
}
##############################################################
 #########################################################################
 photo_count1<<-data.frame(r_year,
                          site,
                          r_date=r_date1,
						  time_start,
						  observer=observer,
						  local_site=C1$local_site,
						  animal_type=C1$age,
						  iLeft=C1$lon,
						  iTop=C1$lat,
                          datecreated,
						  file_name=basename(labelinput1),
						  species=SpeciesSQL,
						  GPSLongitude=C1$lon,
						  GPSLatitude=C1$lat,
						  GPSAltitude=GPSAltitude,
						  creator=creator,
						  type=Count_type)	
						  
photo_count_list1 <<- data.frame(r_year,
                                  site, 
								  r_date=r_date1,
								  time_start,
								  observer=observer,
								  comments="data from R",
								  visibility,  
                                  rain,  
                                  distance,
                                  type=Count_type,
								  quality, 
                                  splash,  
								  datecreated,
								  species=SpeciesSQL,
								  type_count=Count_type)
												  
							 						 
photo_count_files1<<-data.frame(r_year,
                               site,
							   r_date=r_date1,
							   time_start,
							   observer=observer,
							   comment=basename(labelinput1),
							   datecreated,
							   file_name=InfoDayTable$file_name,
							   latitude=InfoDayTable$latitude,
							   longitude=InfoDayTable$longitude,
							   altitude=InfoDayTable$altitude)

  }


  
