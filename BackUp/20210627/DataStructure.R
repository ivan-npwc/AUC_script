#DataStructure=function (labelInput=labelInput,
#                        species1=Species,
#                        StartSeason=T) {
  
 # labelInput<<-"D:\\NFS_2019\\2019_138_OPP_pups\\20190803\\20190803_01"
 # species1<<-Species
  
  library(raster)
  
  NFSageCategoryDir= "System data/AgeCategory/NFS"	#"G:\\My Drive\\AUC_script\\System data\\AgeCategory\\NFS" #
  SSLageCategoryDir= "System data/AgeCategory/SSL"	
  ModelPOlSSL138Dir= "System data/Model_polygons_138"
  
   ModelPOlSSL138<<-list.files(ModelPOlSSL138Dir,full.names=T)
  NFSageCategory<<-list.files(NFSageCategoryDir,full.names=T)
  SSLageCategory<<-list.files(SSLageCategoryDir,full.names=T)
  
  exten=extension(NFSageCategory)
  NewNameAge=paste0(basename(labelInput),exten)
  
  OPPname1=list.files(labelInput, pattern="psx", full.names=T)
  OPPname2=paste0(labelInput,"\\",gsub(".psx","",basename(OPPname1)),".files")
  Rename1=paste0(labelInput,"\\", basename(labelInput),".psx")
  Rename2=paste0(labelInput,"\\", basename(labelInput),".files")
  
  OPPkmzfrom=list.files(labelInput, pattern="kmz", full.names=T)
  OPPkmzto=paste0(labelInput,"\\", basename(labelInput),".kmz")
  
  PolygonDir=paste0(labelInput,"\\Polygons")
  ObserverCountDir=paste0(labelInput,"\\Observer count")
  AnimalMeasurDir=paste0(labelInput,"\\Polygons\\Animal_measurements")
  HauloutDir=paste0(labelInput,"\\Polygons\\Haulout")
  RookeryDir=paste0(labelInput,"\\Polygons\\Rookery")
   ExcludeDir=paste0(labelInput,"\\Polygons\\Exclude")
   ModelPolDir=paste0(labelInput,"\\Polygons\\Model")
  
#if(dir.exists(PolygonDir)==F){
  dir.create(PolygonDir,showWarnings = F)
  dir.create(ObserverCountDir,showWarnings = F)
   dir.create(AnimalMeasurDir,showWarnings = F)
   dir.create(HauloutDir,showWarnings = F)
   dir.create(RookeryDir,showWarnings = F)
  dir.create(ExcludeDir,showWarnings = F)
   dir.create(ModelPolDir,showWarnings = F)
#  }
 # ErrDir=paste0(labelInput,"\\Error");dir.create(ErrDir);PointsErrDir=paste0(ErrDir,"\\Points");dir.create(PointsErrDir)
   
  if (Species=="SSLAdult") {file.copy(ModelPOlSSL138,ModelPolDir)
                            file.copy(SSLageCategory,paste0(ObserverCountDir,"\\",NewNameAge))
							
							ErrDirAdult=paste0(labelInput,"\\Error_SSLAdult");dir.create(ErrDirAdult)
                            ErrDirPup=paste0(labelInput,"\\Error_SSLPup");dir.create(ErrDirPup)
                            ErrDirAdultPoints=paste0(ErrDirAdult,"\\Points");dir.create(ErrDirAdultPoints)
                            ErrDirPupPoints=paste0(ErrDirPup,"\\Points");dir.create(ErrDirPupPoints)
							}
 if (Species=="NFSAdult"){  file.copy(NFSageCategory,paste0(ObserverCountDir,"\\",NewNameAge))
                            ErrDirAdult=paste0(labelInput,"\\Error_NFSAdult");dir.create(ErrDirAdult)
                            ErrDirAdultPoints=paste0(ErrDirAdult,"\\Points");dir.create(ErrDirAdultPoints)}

############################

 # file.rename(OPPname1,Rename1)
 # file.rename(OPPname2,Rename2)	
 # file.rename(OPPkmzfrom,OPPkmzto)

  
  
#}