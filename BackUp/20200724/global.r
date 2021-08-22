if (!require("DT")) {install.packages("DT"); library("DT")}
if (!require("keras")) {install.packages("keras"); library("keras")}
if (!require("XML")) {install.packages("XML"); library("XML")}
if (!require("magick")) {install.packages("magick"); library("magick")}
if (!require("filesstrings")) {install.packages("filesstrings"); library("filesstrings")}
if (!require("abind")) {install.packages("abind"); library("abind")}
if (!require("reticulate")) {install.packages("reticulate"); library("reticulate")}
if (!require("parallel")) {install.packages("parallel"); library("parallel")}
if (!require("doParallel")) {install.packages("doParallel"); library("doParallel")}
if (!require("foreach")) {install.packages("foreach"); library("foreach")}
if (!require("tensorflow")) {install.packages("tensorflow"); library("tensorflow")}
if (!require("sp")) {install.packages("sp"); library("sp")}
if (!require("rgdal")) {install.packages("rgdal"); library("rgdal")}
if (!require("geosphere")) {install.packages("geosphere"); library("geosphere")}
if (!require("dismo")) {install.packages("dismo"); library("dismo")}
if (!require("rgeos")) {install.packages("rgeos"); library("rgeos")}
if (!require("kohonen")) {install.packages("kohonen"); library("kohonen")}
if (!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if (!require("beepr")) {install.packages("beepr"); library("beepr")}
if (!require("tcltk")) {install.packages("tcltk"); library("tcltk")}
if (!require("sf")) {install.packages("sf"); library("sf")}
if (!require("spatialEco")) {install.packages("spatialEco")}
if (!require("encryptr")) {install.packages("encryptr");library("encryptr")}
if (!require("RSQLite")) {install.packages("RSQLite")}
#if (!require("sparklyr")) {install.packages("sparklyr");library(sparklyr);spark_install(version = "2.1.0")}
if (!require("writexl")) {install.packages("writexl")}
library(raster)

#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#  BiocManager::install("EBImage")

#########################################################
listValue <<- readRDS("System data/listUniq")
listTMP <<-readRDS("System data/listTMP")
######################
labelInput<<-listValue$labelInput

    nchaBName=nchar(basename(labelInput))+1
    pthOPP<<-substr(labelInput,0, nchar(labelInput)-nchaBName)
    listOPP<<-list.files(pthOPP)
#pthOPP<<- listValue$pthOPP
	
NFS_Adult_weight_pth<<-listValue$NFS_Adult_weight_pth
NFS_Pup_weight_pth<<-listValue$NFS_Pup_weight_pth
SSL_Adult_weight_pth<<-listValue$SSL_Adult_weight_pth
SSL_Pup_weight_pth<<-listValue$SSL_Pup_weight_pth
SSL_Age_pth <<-  listValue$SSL_Age_pth
WLRS_weight_pth<<-  listValue$WLRS_weight_pth
key <<-  listValue$key
SQLite_path<<-  listValue$SQLite_path

KK_Effort <<-listValue$KK_Effort

trainDir<<-listValue$trainDir
weight<<-listValue$Weight
Model_base<<-listValue$Model_base
BatchIntens<<-listValue$BatchIntens
Split<<-listValue$Split	       
Smooth<<-listValue$Smooth       
batch_size<<-listValue$batch_size
epochs<<-listValue$epochs
Deformation<<-listValue$Deformation








#SpeciesManager<<-listTMP$SpeciesManager
#ProsessManager<<-listTMP$ProsessManager
#listOPP1<<-listTMP$listOPP1


listR_year=c("2016","2017","2018", "2019")
listR_site=c("38", "138")





#OPPListPred1=listTMP$OPPListPred1
#Species=listTMP$Species
#BatchProcessVector=listTMP$OPPListPred1)
#source("Modules/ListTMPUpdate.r")

if(is.null(NFS_Adult_weight_pth)==T) {NFS_Adult_weight_pth="_"}
if(is.null(NFS_Pup_weight_pth)==T) {NFS_Pup_weight_pth="_"}
if(is.null(SSL_Adult_weight_pth)==T) {SSL_Adult_weight_pth="_"}
if(is.null(SSL_Pup_weight_pth)==T) {SSL_Pup_weight_pth="_"}
if(is.null(SQLite_path)==T) {SQLite_path="_"}
if(is.null(KK_Effort)==T) {KK_Effort="_"}
if (exists("PredictPoint")==F){PredictPoint="_"}
if (exists("ModelPoligon")==F){ModelPoligon="_"}
if (exists("ObserverPoint")==F){ObserverPoint="_"}



if (exists("trainDir")==F) {trainDir="_"}
if (exists("Model_base")==F) {Model_base="_"}
if (exists("Weight")==F) {Weight="_"}


######################
Pth_img_error <<- paste0(labelInput,"\\Error\\Points\\Error.shp")
if (file.exists(Pth_img_error)==F) {Pth_img_error="_"}
Rookery_polygon<<-paste0(labelInput,"\\Polygons\\Rookery\\Rookery.shp")
Haulout_polygon<<-paste0(labelInput,"\\Polygons\\Haulout\\Haulout.shp")
Exlude_polygon<<-paste0(labelInput,"\\Polygons\\Exclude\\Exclude.shp")
AnmlsMearPth<<-paste0(labelInput,"\\Polygons\\Animal_measurments\\Animal_measurments.shp")

if (file.exists(AnmlsMearPth)==F) {AnmlsMearPth="_"}
if (file.exists(Rookery_polygon)==F) {Rookery_polygon="_"}
if (file.exists(Haulout_polygon)==F) {Haulout_polygon="_"}
if (file.exists(Exlude_polygon)==F) {Exlude_polygon="_"}
Image_dir_Sin<<-"_"
Mask_dir_Sin<<-"_"
#################################################################
pth_log<<-paste0(labelInput,"\\",basename(labelInput), "-Log.csv")
if (file.exists(pth_log)==F) {
  log1=NULL } else { log1<<-read.csv(pth_log) 
    }
#####################################################
#KMLdir=paste0(labelInput,"\\",basename(labelInput))
#if (dir.exists(KMLdir)==F) {
#Unzip_progress = F } else {Unzip_progress=T}

pth_table<<-paste0(labelInput,"\\", basename(labelInput),"_table.csv")
if (file.exists(pth_table)==F) {
  KML_progress=F } else   {KML_progress=T}
 #############################################################
Tpth=paste0(labelInput,"\\", basename(labelInput), "_table.csv")
ImgSave=paste0(labelInput, "\\", "Predict", "\\","Input")
if (file.exists(Tpth)==T & dir.exists(ImgSave)==T) {
  table=read.csv(Tpth)
  Need=length(table$link)
  Presence=length(list.files(ImgSave))
  if (Need ==Presence) {Image_prepare_progress=T} else {Image_prepare_progress=F}
  } else {Image_prepare_progress=F}
########################################################################
lastBackUp=unique(listTMP$backUpDate)
if (lastBackUp != format(Sys.Date(),"%Y%m%d")) {
  from= "Modules"
  from1<<-list.files(from,full.names=T)
  to<<-paste0("BackUp\\",format(Sys.Date(),"%Y%m%d"))
 if (dir.exists(to)==F){dir.create(to)}
  file.copy(from1,to)
  file.copy("global.r",to)
  file.copy("server.r",to)
  file.copy("ui.r",to)
  
}
#####################################################
