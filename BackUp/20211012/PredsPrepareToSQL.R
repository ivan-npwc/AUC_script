
PredsPrepareSQL=function (labelinput1) { #labelinput1 - it is subfolder in general labelInput folder
						 
						 labelInput
			             site
						 System_data
                         Species
                         SQLite_path
                         Count_type						 
    Count3=NULL
     datetime =basename(labelInput)
     r_date1=paste0(substr(datetime,0,4),"-",substr(datetime,5,6),"-",substr(datetime,7,8))
     time_start= paste0(substr(datetime,10,11),":",substr(datetime,12,13),":",substr(datetime,14,15))
	 
     crs    <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
     Haulout_polygonDir=paste0(labelinput1, "\\Polygons\\Haulout");HPF<-list.files(Haulout_polygonDir,full.names=T,pattern=".shp")
    if (length(HPF)==0){stop("No Haulout Polygon found")}
 	 Haulout_polygon=shapefile(HPF)  
     proj4string(Haulout_polygon) <- CRS(crs)
  ObserverCountDir=paste0(labelinput1,"\\Observer_count")
  ObserverCountP=list.files(ObserverCountDir,full.names=T,pattern = "kml|shp")
  DirLocSites=paste0(System_data,"\\polygons_local_sites\\",site);pthLcSt=list.files(DirLocSites,full.names=T,pattern = "kml|shp")
  locSite=readOGR(pthLcSt)
  proj4string(locSite) <- CRS(crs)
  
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
  InfoDayTable=NULL
  Count=NULL
  ####################################################################################### GET INFO FOR foto_count_files
  ########################################################################################
  if (file.exists(paste0(labelinput1,"\\",basename(labelinput1),".files"))==F) {
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
      Count=read.csv(PredictPointsP);Count$X=NULL;if (Species=="NFSPup"){Count$age="P"}
      if(file.exists(PredictPointsPsslp)==T) {countP=read.csv(PredictPointsPsslp);countP$X=NULL;age="P"}
      
      if(file.exists(AdditnlPointsPnfsP)== T) {countP=data.frame(rgdal::readOGR(AdditnlPointsPnfsP,"Point Features"))
      countP=data.frame(lat=countP$coords.x2,lon=countP$coords.x1,age="P")}
      
      if(file.exists(WLRSadd)==T) {countP=data.frame(rgdal::readOGR(WLRSadd,"Point Features"))
      countP=data.frame(lat=  countP$coords.x2,lon=countP$coords.x1,age="U")}
      
      if (exists("countP")){Count=rbind(Count,countP)}
      Count$local_site="U"
    }		 
   ################################################## ################################  COLLECT DATA MANUAL COUNT
   ##################################################################################
   options(warn=-1)
   if (Count_type %in% c("opp_manual_model_count","manual_count_full")) { ################################### data for manual count
    if (length(ObserverCountP)!=0 & file.exists(ObserverCountP,showWarnings=F)) {
	Count=NULL
	  for(i in 1:length(ObserverCountP)) {
        Points= readOGR(ObserverCountP[i])
		observer=strsplit(file_path_sans_ext(basename(ObserverCountP[i])),"_")[[1]][3]
		proj4string(Points) <- CRS(crs)
		pts = Points[!is.na(over(Points,as(Haulout_polygon,"SpatialPolygons"))),]
		options(warn=0)
		if (length(pts)==0){ print(labelinput1);stop("None of the points inside the Haulout polygon")}
		pts1=data.frame(point.in.poly(pts,locSite))
        ObsCount=data.frame(lon=pts1$coords.x1, lat= pts1$coords.x2,age= pts1$Description,local_site=pts1$LAYER, creator=observer)
		Count<<-rbind(Count,ObsCount)
        }} else {print(paste0("No observer count for,",labelinput1))
		}
        }		
    ############################################################################### ADD MODEL POLYGONS
    ###############################################################################
    if (Count_type==	"opp_manual_model_count" & file.exists(MdlPolPth,showWarnings=F)  & file.exists(ObserverCountP,showWarnings=F)) { 
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
if (is.null(Count) !=T) { # only if  data in Count
    Count$age=gsub("An","AN",Count$age)
    Count$age=gsub("Unk","U",Count$age)
    Count$age=gsub("Sa","SA",Count$age)
    Count$age=gsub("Bch","SA",Count$age)
    Count$age=gsub("DF","U",Count$age)
    
    Count1 =Count %>% 
      group_by(across())%>% 
      summarize(n=n()) 
 
    Count3=Count1  
  ############################  USED LIST, BECOSE THE LENGTH OF CHARACTER IS DIFFERENT files name and count are different

    Subdata <<-list(
      GPSAltitude=GPSAltitude,
      site= site, 
      r_year =  format(as.POSIXct(strptime(r_date1, "%Y-%m-%d")),"%Y"),          
      r_date= r_date1,
      time_start= time_start , 
      creator = Count3$creator,
      animal_type=   Count3$age,
      iLeft=Count3$lon,
      iTop=Count3$lat,
      file_nameOPP=labelinput1,
      ########################################   photo_count_list
	  type_count=Count_type,
	  Species= SpeciesSQL,
      #######################################   photo_count_files
      latitude=InfoDayTable$lat,
      longitude=InfoDayTable$lon,
      altitude=InfoDayTable$altitude,
      CommentsPCF=labelinput1,      
      file_name= file_name, #unique(Info$Img),
      local_site=Count3$local_site
    )
    return(Subdata)
  }
  }
  
