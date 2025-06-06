if (!require("Rcpp")) {install.packages("Rcpp"); library("Rcpp")}
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
if (!require("tfdatasets")) {install.packages("tfdatasets"); library("tfdatasets")}
if (!require("purrr")) {install.packages("purrr"); library("purrr")}


if (!require("sp")) {install.packages("sp"); library("sp")}
if (!require("rgdal")) {install.packages("rgdal"); library("rgdal")}
if (!require("geosphere")) {install.packages("geosphere"); library("geosphere")}
if (!require("dismo")) {install.packages("dismo"); library("dismo")}
if (!require("rgeos")) {install.packages("rgeos"); library("rgeos")}
if (!require("kohonen")) {install.packages("kohonen"); library("kohonen")}
if (!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if (!require("beepr")) {install.packages("beepr"); library("beepr")}
#if (!require("tcltk")) {install.packages("tcltk"); library("tcltk")}
#if (!require("sf")) {install.packages("sf"); library("sf")}
if (!require("spatialEco")) {install.packages("spatialEco");library("spatialEco")}
#if (!require("encryptr")) {install.packages("encryptr");library("encryptr")}
if (!require("RSQLite")) {install.packages("RSQLite")}
#if (!require("sparklyr")) {install.packages("sparklyr");library(sparklyr);spark_install(version = "2.1.0")}
if (!require("writexl")) {install.packages("writexl")}
if (!require("shinythemes")) {install.packages("shinythemes"); library("shinythemes")}
#install.packages(c("pkg1", "pkg2"))
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#  BiocManager::install("EBImage")
########################################
#library(reticulate)
#use_condaenv("base",required = TRUE)
#py_config() 
#library(tensorflow)
#tf$config$list_physical_devices('GPU')
#########################################################
listValue <<- readRDS("listUniq")
listTMP <<-readRDS("listTMP")
######################
labelInput<<-listValue$labelInput

    nchaBName=nchar(basename(labelInput))+1
    pthOPP<<-substr(labelInput,0, nchar(labelInput)-nchaBName)
	site   <<-  strsplit(basename(pthOPP),"_")[[1]][2]
	
  #  listOPP<<-list.files(pthOPP)
  listOPP  <<-listValue$listOPP
#pthOPP<<- listValue$pthOPP
	
NFS_Adult_weight_pth<<-listValue$NFS_Adult_weight_pth
NFS_Pup_weight_pth<<-listValue$NFS_Pup_weight_pth
NFS_AN_TF_UNET<<-listValue$NFS_AN_TF_UNET
SSL_Adult_weight_pth<<-listValue$SSL_Adult_weight_pth
SSL_Pup_weight_pth<<-listValue$SSL_Pup_weight_pth
SSL_Age_pth <<-  listValue$SSL_Age_pth
SSL_Age_Weight_Rookery<<-  listValue$SSL_Age_Weight_Rookery
Weight<<- listValue$Weight
WLRS_Sand_weight_pth<<-  listValue$WLRS_Sand_weight_pth
WLRS_Rocky_weight_pth<<-  listValue$WLRS_Rocky_weight_pth
LRG_pth<<-listValue$LRG_pth
LRGH_MSRMNTS  <<-listValue$LRGH_MSRMNTS
key <<-  listValue$key
SQLite_path<<-  listValue$SQLite_path

NFS_Pup_weight_pth512 <<- listValue$NFS_Pup_weight_pth512
#site   <<-  listValue$site

KK_Effort <<-listValue$KK_Effort
DarkTheme<<-listValue$DarkTheme; if (DarkTheme==T){Theme<<-"slate"} else {Theme<<-"lumen"}
trainDir<<-listValue$trainDir

Model_base<<-listValue$Model_base
BatchIntens<<-listValue$BatchIntens
Split<<-listValue$Split	       
Smooth<<-listValue$Smooth       
batch_size<<-listValue$batch_size
epochs<<-listValue$epochs
Deformation<<-listValue$Deformation
System_data<<-listValue$System_data
DirModelsCheck<<-  listValue$DirModelsCheck
ModelCheckAlg <<- FALSE
TrainFromBaseModel<<-TRUE
type<<-listValue$type
System_data<<-listValue$System_data

#SpeciesManager<<-listTMP$SpeciesManager
#ProsessManager<<-listTMP$ProsessManager
#listOPP1<<-listTMP$listOPP1

listR_year=c("2016","2017","2018", "2019","2020","2021")
listR_site=                                                                             c("19",#severnoe
			                                                                              "20",#severo zapadnoe
																						  "30",#yugo vostochny
																						  "37",#kozlova
																						  "44",#kekurny
																						  "50",#sivuchy (SofOKH)
																						  "53",#vladimira
																						  "56",#anziferova
																						  "71",#lovushky
																						  "76",#raykoke
																						  "82",#srednego
																						  "95",#brat chirpoev
																						  "117",#moneron
																						  "118",#kuznezova
																						  "120",#opasnosty
																						  "138")# tuleny
listR_species=c("CU","SSL")

HaulAsModel=FALSE
UseRookeryOnly=FALSE
UseEffort=FALSE
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
#if (exists("Weight")==F) {Weight="_"}


######################
Image_dir_Sin<<-"_"
Mask_dir_Sin<<-"_"
UseAllHauloutImages<<-FALSE
#################################################################
pth_log<<-paste0(labelInput,"\\",basename(labelInput), "-Log.csv")
if (file.exists(pth_log)==F) {
  log1=NULL } else { log1<<-read.csv(pth_log) 
    }
#####################################################
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
