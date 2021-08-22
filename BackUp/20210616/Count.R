
 
 AnmlsMear=list.files(paste0(labelInput, "\\Polygons\\Animal_measurments"),full.names=T,pattern=".shp")
 Haulout_polygon=list.files(paste0(labelInput, "\\Polygons\\Haulout"),full.names=T,pattern=".shp")
 
 if(file.exists(AnmlsMear)==F) {stop("Make Animal measurements please!")}
 if(file.exists(Haulout_polygon)==F) {stop("Make Haulout polygon please!")}
 

if (Species=="NFSAdult") {
 source("Modules/Unzip.r");print("Unzip")
  source("Modules/KMLprepare.r"); print("KMLprepare")
  source("Modules/Image_prepare.r");print("Image_prepare")
  source("Modules/Unet.r");print("Unet")
  source("Modules/BlobAnalys.r"); print("BlobAnalys")
  source("Modules/Geo_ref.r");print("Geo_ref")
  source("Modules/KML.r");print("KML")

}

if (Species=="SSLAdult") {
  source("Modules/Unzip.r");print("Unzip")
  source("Modules/KMLprepare.r"); print("KMLprepare")
  source("Modules/Image_prepare.r");print("Image_prepare")
  source("Modules/Unet.r");print("Unet")
  source("Modules/BlobAnalys.r"); print("BlobAnalys")
  source("Modules/Geo_ref.r");print("Geo_ref")
  source("Modules/KML.r");print("KML")
  
   source("Modules/Age_PREPROCESING.r");print("Age_PREPROCESING")
   source("Modules/Age_PREDICT.r");print("Age_PREDICT")
  # source("Modules/Age_POSTPROCESING.r");print("Age_POSTPROCESING")

}

if (Species=="SSLPup") {
 if (file.exists(paste0(labelInput,"\\Predict\\",basename(labelInput),"_SSLAdult_AgeLatLon.csv"))==T){
  source("Modules/Unet.r");print("Unet")
  source("Modules/BlobAnalys.r"); print("BlobAnalys")
  source("Modules/Geo_ref.r");print("Geo_ref")
  source("Modules/KML.r");print("KML")
  } else {print("Predict SSL Adult first!")}
}
