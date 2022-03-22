
 
 AnmlsMear=list.files(paste0(labelInput, "\\Polygons\\Animal_measurements"),full.names=T,pattern=".shp")
 AnmlsMear1=list.files(paste0(labelInput, "\\Polygons\\Animal_measurments"),full.names=T)
 file.copy(AnmlsMear1,paste0(labelInput, "\\Polygons\\Animal_measurements"))
 
 Haulout_polygon=list.files(paste0(labelInput, "\\Polygons\\Haulout"),full.names=T)
 ModPolDir=paste0(labelInput, "\\Polygons\\Model")
 
 
 if(file.exists(AnmlsMear)==F) {stop("Make Animal measurements please!")}
 if(file.exists(Haulout_polygon[1])==F) {stop("Make Haulout polygon please!")}
 

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
file.copy(Haulout_polygon,ModPolDir)
  source("Modules/Unzip.r");print("Unzip")
  source("Modules/KMLprepare.r"); print("KMLprepare")
  source("Modules/Image_prepare.r");print("Image_prepare")
  source("Modules/Unet.r");print("Unet")
  source("Modules/BlobAnalys.r"); print("BlobAnalys")
  source("Modules/Geo_ref.r");print("Geo_ref")
  source("Modules/KML.r");print("KML")
  
   source("Modules/Age_PREPROCESING.r");print("Age_PREPROCESING")
   source("Modules/Age_PREDICT.r");print("Age_PREDICT")
   source("Modules/Error_calculate.r")
}

if (Species=="SSLPup") {

  source("Modules/IMAGE_SPLIT.r");print("IMAGE_SPLIT")
  source("Modules/UnetBlobAnalisPUP.r"); print("UnetBlobAnalisPUP")
  source("Modules/Geo_ref.r");print("Geo_ref")
  source("Modules/KML.r");print("KML")
 
}
