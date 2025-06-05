	 library(dplyr)
	 library(tfdatasets)
	 library(keras)
	library(tfdatasets)
	library(tidyverse)
	library(rsample)
	library(reticulate)
	library(raster)
	 
	 labelInput
	 date1=substr(basename(labelInput),1,15)
	 month<<-substr(basename(labelInput),5,6)
	 Species
 
#source("C:\\Users\\usato\\SSL_DB\\AUC\\Modules\\KMLwrite_function.r")



 LIMIT = 0.92
 crs = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

kmlPathSave=paste0(labelInput,"\\Predict\\","NFSAdult", "_", date1, ".kml")
 AgePredOptimisation_pth = paste0(labelInput,"\\Predict\\","NFSAdult_AgePredOptimisation", "_", date1,".csv")        

Rookery_polygonDir=paste0(labelInput, "\\Polygons\\Rookery")
Haulout_polygonDir=paste0(labelInput, "\\Polygons\\Haulout")

 						 
   if(dir.exists(Haulout_polygonDir)==T){Haulout_polygon<-list.files(Haulout_polygonDir,full.names=T,pattern=".shp|kml")}
   if(dir.exists(Rookery_polygonDir)==T){Rookery_polygon_pth <-list.files(Rookery_polygonDir,full.names=T,pattern=".shp|kml")}
   

DirR=paste0(labelInput,"\\Predict\\",Species,"\\Rookery");R=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\",Species,"\\Haulout");H=list.files(DirH,full.names=T)
if (dir.exists(DirH)==F) {
DirR=paste0(labelInput,"\\Predict\\Age_predict\\Rookery");R=list.files(DirR,full.names=T)
DirH=paste0(labelInput,"\\Predict\\Age_predict\\Haulout");H=list.files(DirH,full.names=T)
}

RH=c(R,H)
##################################################################################
Pointsfilter=function (tble,pthPolygon,IN) {
coords <- data.frame(lat= tble$lon, lon=tble$lat)   
    data   <- data.frame(age= tble$age)   # data
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  crs)
	Points =spTransform(Points,crs)								 							 
    polygon_poly=shapefile(pthPolygon)  
    proj4string(polygon_poly) <- crs
   if (IN =="in"){pts = Points[!is.na(over(Points,as(polygon_poly,"SpatialPolygons"))),]}
   if (IN =="out"){pts = Points[is.na(over(Points,as(polygon_poly,"SpatialPolygons"))),]}
   return(pts)
   }
   #########################################################################
Age_Pred_Fun=function(
    Species="NFSAdult", 
	vision_dimensions=256,
	batch_size=64,
	type,
	Age_Name,
	listImgPred,
#	R_mdlBSAgePTH = paste0(System_data,"/weights/NFS_AGE/NFSageBASE/TF_F_An_Bch_20220819_loss_0.13_epoch_01.h5"),
	H_mdlBSAgePTH =paste0(System_data,"/weights/NFS_AGE/NFSageBASE/TF_F_An_Bch_20220819_loss_0.13_epoch_01.h5"),
	
	#RookerytWeightPTH= paste0(System_data,"/weights/NFS_AGE/NFS_age_TF-F-20220824_loss_013_epoch_03" ),
	HauloutWeightPTH=  paste0(System_data,"/weights/NFS_AGE/NFS_age__AN-Bch_20220826_loss_026_epoch_05" ))	{	   
	
	library(abind)
    library(reticulate)
    library(parallel)
    library(doParallel)
    library(foreach)
	library(EBImage)
    library(keras)
	library(magick) 
	
	date1=substr(basename(labelInput),1,15)
	PthTblAgeRef=paste0(labelInput,"\\Predict\\",date1,"_", Species,"AgeRef.csv")
	
	#if(exists("RmodelAge")==F){RmodelAge<<-load_model_hdf5(R_mdlBSAgePTH)}
	if(exists("HmodelAge")==F){HmodelAge<<-load_model_hdf5(H_mdlBSAgePTH)}
##################################################################################	
#if (type=="Rookery"){		  
#		  RookerytWeight=readRDS(RookerytWeightPTH)
#		  set_weights(RmodelAge,RookerytWeight)
#		  modelAge<-RmodelAge   		
#	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.csv")
#         kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.kml")       
#}
############################################################################
if (type=="Haulout"){  
		  HauloutWeight=readRDS(HauloutWeightPTH)
		  set_weights(HmodelAge,HauloutWeight)
		  modelAge<-HmodelAge	 
	      PTH_TableGeoAge =paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT.csv")
          kmlPathSave=paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT.kml")    
  }
############################################################################
  data <- tibble::tibble(img = listImgPred)
	create_dataset <- function(data, train, batch_size = batch_size, vision_dimensions) {
	 
	 dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$decode_jpeg(tf$io$read_file(.x$img))
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float64)
		)) %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions)),
		))
	   dataset3 <- dataset %>% dataset_batch(batch_size) %>%
                        	#   dataset_cache() %>%
							   dataset_map(unname) # Keras needs an unnamed output.
							   #%>% dataset_shuffle(buffer_size = #batch_size*vision_dimensions)
	}
	pred_data_set <- create_dataset(data, train = FALSE, batch_size=batch_size, vision_dimensions=vision_dimensions)
    pred=modelAge %>% predict(pred_data_set)
	
###############################################################################################
    preds3 <<- data.frame(pred)
	names(preds3) <- Age_Name
	preds3$link=basename(as.character(listImgPred))
for (i in 1:length(listImgPred)) {
   if(preds3[,1][i]> preds3[,2][i]){preds3$name[i]=Age_Name[1]}
   if(preds3[,1][i]< preds3[,2][i]){preds3$name[i]=Age_Name[2]}
   }
 
 ######################################
 if (type=="Haulout"){preds3$name[preds3$AN < LIMIT]="Bch"} # try 85
 #if (type=="Rookery"){preds3$name[preds3$TF < 0.80]="F"}
#################################################################################################
	  TblAgeRef=read.csv(PthTblAgeRef)
      TblAgeRef$link=basename(as.character(TblAgeRef$pth_save_img))

      TblGAg <- merge(x=preds3,y=TblAgeRef,by="link",all.x=T)
	  
	  AgePredOptimisation=data.frame(AN=TblGAg$AN,Bch=TblGAg$Bch,lat=TblGAg$lat,lon=TblGAg$lon,Rookery=TblGAg$Rookery)
      write.csv(AgePredOptimisation,AgePredOptimisation_pth, row.names=F) 
	  
      TableGeoAge1 <- data.frame(pth_save_img=TblGAg$pth_save_img, lon=TblGAg$lat,lat=TblGAg$lon,age= TblGAg$name)
	  TableGeoAge2=TableGeoAge1 %>% filter(age != "P")
      write.csv(TableGeoAge2,PTH_TableGeoAge)
	  
	

    coords <- data.frame(lat= TableGeoAge1$lon, lon=TableGeoAge1$lat)   
    data   <- data.frame(Description= TableGeoAge1$age)   # data
    PointsH <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  crs)
	PointsH =spTransform(PointsH,crs)
     unlink(kmlPathSave)
	  writeOGR(PointsH,kmlPathSave,driver="KML", layer="Description")
	 #KMLwrite(Img3=TableGeoAge2,kmlPathSave)
}
##################################
if (length(H) !=0){Age_Pred_Fun(type="Haulout",Age_Name= c("AN", "Bch"),listImgPred=RH); print("Age_pred")}
#if (length(R) !=0){Age_Pred_Fun(type="Rookery", Age_Name= c("F","TF"), listImgPred=R); print("Rookery")}
#########################################################################################
# rkPTH = paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.csv")
 #PthPup=paste0(labelInput,"\\Predict\\SSLPup_",date1,".csv")
hPth = paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT.kml")
#if (length(R) !=0){rk=read.csv(rkPTH)}
#if (length(H) !=0){

      rk=NULL
      Hl=NULL
      Houl=NULL
      ad=NULL
     Points=readOGR(hPth)
	 proj4string(Points) <- crs

   
   
    Rookery_polygon=readOGR(Rookery_polygon_pth)  
    proj4string(Rookery_polygon) <- crs
    rk = Points[!is.na(over(Points,as(Rookery_polygon,"SpatialPolygons"))),]
    if (!is.null(rk)) {rk$Description[rk$Description=="Bch"]="F";rk$Description[rk$Description=="AN"]="TF"}
    rk=data.frame(rk)
  

  
    Houl = Points[is.na(over(Points,as(Rookery_polygon,"SpatialPolygons"))),]
    Houl=data.frame(Houl)
  
    ad=rbind(rk,Houl)
    coords <- data.frame(lat= ad$coords.x1, lon=ad$coords.x2)   
    data   <- data.frame(Description= ad$Description)   # data
    Points <- SpatialPointsDataFrame(coords = coords,
                                     data = data, 
                                     proj4string =  crs)
    Points =spTransform(Points,crs)

    unlink(kmlPathSave)
    writeOGR(Points,kmlPathSave,driver="KML", layer="Description")



