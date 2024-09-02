
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
   
   PredOptimisationDir=paste0(labelInput,"\\Polygons\\Prediction optimisation")
  

  dir.create(PolygonDir,showWarnings = F)
  dir.create(ObserverCountDir,showWarnings = F)
   dir.create(AnimalMeasurDir,showWarnings = F)
   dir.create(HauloutDir,showWarnings = F)
   dir.create(RookeryDir,showWarnings = F)
  dir.create(ExcludeDir,showWarnings = F)
   dir.create(ModelPolDir,showWarnings = F)
    dir.create(PredOptimisationDir,showWarnings = F)
 
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
						 dir.create(paste0(labelInput,"\\Predict\\SSLPup"))
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













