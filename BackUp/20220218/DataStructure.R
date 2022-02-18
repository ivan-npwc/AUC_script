    System_data
    labelInput
    Species
 
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


   
  if (Species=="SSLAdult") {#file.copy(ModelPOlSSL138,ModelPolDir)
                            #file.copy(SSLageCategory,paste0(ObserverCountDir,"\\",NewNameAge))
							
						#	ErrDirAdult=paste0(labelInput,"\\Error_SSLAdult");dir.create(ErrDirAdult)
                         #   ErrDirPup=paste0(labelInput,"\\Error_SSLPup");dir.create(ErrDirPup)
                         #   ErrDirAdultPoints=paste0(ErrDirAdult,"\\Points");dir.create(ErrDirAdultPoints)
                        #    ErrDirPupPoints=paste0(ErrDirPup,"\\Points");dir.create(ErrDirPupPoints)
							}
 if (Species=="NFSAdult"){  # file.copy(NFSageCategory,paste0(ObserverCountDir,"\\",NewNameAge))
                            ErrDirAdult=paste0(labelInput,"\\Error_NFSAdult");dir.create(ErrDirAdult)
                            ErrDirAdultPoints=paste0(ErrDirAdult,"\\Points");dir.create(ErrDirAdultPoints)}
if (Species=="WLRS"){  
                            ErrDirAdult=paste0(labelInput,"\\Error_WLRS");dir.create(ErrDirAdult)
                            ErrDirAdultPoints=paste0(ErrDirAdult,"\\Points");dir.create(ErrDirAdultPoints)
							PoligonErr=paste0(ErrDirAdult,"\\Polygons");dir.create(PoligonErr)
							}
					
###################################### DATA CORRECTION
ObserverCountDirOLD=paste0(labelInput,"\\Observer count")
 if (dir.exists(ObserverCountDirOLD)) {
listFiles=list.files(ObserverCountDirOLD, full.names=T)
#file.copy(listFiles,ObserverCountDir)
}


if (Species=="NFSPup"){	ModelCountDir=paste0(ObserverCountDir,"\\Model_count"); dir.create(ModelCountDir)}	


























