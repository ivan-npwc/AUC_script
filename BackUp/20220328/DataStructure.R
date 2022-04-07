    source("Modules/KMLwrite_function.r")
	
	System_data
    labelInput
    Species
   date1=substr(basename(labelInput),1,15)
  library(raster)
  

  PolygonDir=paste0(labelInput,"\\Polygons")
  ObserverCountDir=paste0(labelInput,"\\Observer_count")

  AnimalMeasurDir=paste0(labelInput,"\\Polygons\\Animal_measurements")
  HauloutDir=paste0(labelInput,"\\Polygons\\Haulout")
  RookeryDir=paste0(labelInput,"\\Polygons\\Rookery")
   ExcludeDir=paste0(labelInput,"\\Polygons\\Exclude")
   ModelPolDir=paste0(labelInput,"\\Polygons\\Model")
  

  dir.create(PolygonDir,showWarnings = F)
  dir.create(ObserverCountDir,showWarnings = F)
   dir.create(AnimalMeasurDir,showWarnings = F)
   dir.create(HauloutDir,showWarnings = F)
   dir.create(RookeryDir,showWarnings = F)
  dir.create(ExcludeDir,showWarnings = F)
   dir.create(ModelPolDir,showWarnings = F)


##########################################################   
  if (Species=="SSLAdult") {
 rkPTH = paste0(labelInput,"\\Predict\\",date1,"_", Species,"_ROOKERY.csv")
 hPth = paste0(labelInput,"\\Predict\\",date1,"_", Species,"_HAULOUT.csv")
 PthPup=paste0(labelInput,"\\Predict\\SSLPup_",date1,".csv")

 
 kmlPathSave=paste0(labelInput,"\\Predict\\","SSLAdult", "_", date1, ".kml")

rk=NULL
Hl=NULL
Pup=NULL
adults1=NULL

if (file.exists(rkPTH)){rk=read.csv(rkPTH)}
if (file.exists(hPth)){Hl=read.csv(hPth)}
if (file.exists(PthPup)){P=read.csv(PthPup);Pup=data.frame(lat =as.numeric(P$lat),     lon=as.numeric(P$lon),  age=P$age)}

ad=rbind(rk,Hl)
if(is.null(ad)==F){adults1=data.frame(lat =as.numeric(ad$lat),     lon=as.numeric(ad$lon),  age=ad$age)}


PointsToWrite=rbind(adults1,Pup)


if (is.null(PointsToWrite)==F) {KMLwrite(Img3=PointsToWrite,kmlPathSave=kmlPathSave)}

						
							}
######################################################################
 if (Species=="NFSAdult"){  # file.copy(NFSageCategory,paste0(ObserverCountDir,"\\",NewNameAge))
                         #   ErrDirAdult=paste0(labelInput,"\\Error_NFSAdult");dir.create(ErrDirAdult)
                          #  ErrDirAdultPoints=paste0(ErrDirAdult,"\\Points");dir.create(ErrDirAdultPoints)
						  }
###########################################################################						  
if (Species=="WLRS"){  
                            ErrDirAdult=paste0(labelInput,"\\Error_WLRS");dir.create(ErrDirAdult)
                            ErrDirAdultPoints=paste0(ErrDirAdult,"\\Points");dir.create(ErrDirAdultPoints)
							PoligonErr=paste0(ErrDirAdult,"\\Polygons");dir.create(PoligonErr)
							}
###############################################################################	
if (Species=="SSLPup"){  
                         oldImgDir=paste0(labelInput,"\\Predict\\PUP")
						 newImgDir=paste0(labelInput,"\\Predict\\SSLPup\\ALL")
						 dir.create(paste0(labelInput,"\\Predict\\SSLPup")
						 dir.create(newImgDir)
						 imgs=list.files(oldImgDir, full.names=T)
						 file.copy(imgs,newImgDir)
						 unlink(oldImgDir, recursive=T)
							}				
###################################### DATA CORRECTION
ObserverCountDirOLD=paste0(labelInput,"\\Observer count")
 if (dir.exists(ObserverCountDirOLD)) {
listFiles=list.files(ObserverCountDirOLD, full.names=T)
file.copy(listFiles,ObserverCountDir)
}


MsrOLD=paste0(labelInput,"\\Polygons\\Animal_measurments")
 if (dir.exists(MsrOLD)) {
listMsr=list.files(MsrOLD, full.names=T)
file.copy(listMsr,AnimalMeasurDir)
}





unlink(ObserverCountDirOLD, recursive=T)
unlink(MsrOLD, recursive=T)
#if (Species=="NFSPup"){	ModelCountDir=paste0(ObserverCountDir,"\\Model_count"); dir.create(ModelCountDir)}	














