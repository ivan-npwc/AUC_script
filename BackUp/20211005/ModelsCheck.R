library(keras)
DirModelsCheck
ModelCheckAlg <<- TRUE
listModel=list.files(DirModelsCheck,full.names=T,pattern=".h5")
############################################################################################
if (exists("custom_metric")==T) {
dice_coef <<- 
	  custom_metric("dice_coef", function(y_true, y_pred) {
		smooth = 1
		y_true_f <- k_flatten(y_true)
		y_pred_f <- k_flatten(y_pred)
		intersection <- k_sum(y_true_f * y_pred_f)
		(2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
	  }) } else {
	  


 dice_coef <<- function(y_true, y_pred, smooth = 1) {
    y_true_f <- k_flatten(y_true)
    y_pred_f <- k_flatten(y_pred)
    intersection <- k_sum(y_true_f * y_pred_f)
    (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
  }


}
dice_coef_loss <<- function(y_true, y_pred) -dice_coef(y_true, y_pred)
###
if  (length(listModel)>0) {
  for (k in 1:length(listModel)) {
  PTH= listModel[k]
  unetModel =load_model_hdf5(PTH, custom_objects=c(dice_coef=dice_coef,dice_coef_loss=dice_coef_loss))
  a=get_weights(unetModel)
  Savepth=gsub(".h5","",PTH)
	saveRDS(a,Savepth)
	unetModel=NULL
	a=NULL
	unlink(PTH)
 }
 }
 ###################################################################################
listWeight=list.files(DirModelsCheck,full.names=T)
 for (k in 1:length(listWeight)) {

PTHweightCHECK<<-listWeight[k]

source("Modules/Unet.r")
source("Modules/BlobAnalys.r")
source("Modules/Geo_ref.r")

CheckModelkmlPathSave <<- paste0(labelInput,"\\Predict\\",basename(PTHweightCHECK), "_", basename(labelInput), ".kml")
CheckModelcsvPathSave <<- paste0(labelInput,"\\Predict\\",basename(PTHweightCHECK),"_", basename(labelInput), ".csv")
source("Modules/KML.r")


kmlPathSave1=NULL
}
########datacollect and error calculate
library(data.table)
listPredsRes=list.files(paste0(labelInput,"\\Predict"), pattern="RockyTerrain",full.names=T)
index=grep("csv",listPredsRes)
listPredsRes2=listPredsRes[index]

obsvrCntPth=  list.files(paste0(labelInput,"\\Observer count"),pattern="shp",full.names=T)[1]
Points= data.frame(shapefile(obsvrCntPth))

fin=NULL
for (i in 1:length(listPredsRes2)) {
pthPred= listPredsRes2[i]
preds=read.csv(pthPred)
PredsCount=length(preds[,1])
ObserverCount=length(Points[,1])
Model=basename(pthPred)
res=data.frame(ObserverCount=ObserverCount, ModelCount=PredsCount, diffr=abs(ObserverCount-PredsCount), Model=Model)
fin=rbind(res,fin)
}

fin1 <<- fin[order(fin$diffr),]
pthSaveFin=paste0(labelInput,"\\Predict\\ModelCheck.csv")
wtite.csv(fin1,pthSaveFin)























