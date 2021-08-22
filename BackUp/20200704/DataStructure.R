#DataStructure=function (labelInput=labelInput,
#                        species1=Species,
#                        StartSeason=T) {
  
 # labelInput<<-"D:\\NFS_2019\\2019_138_OPP_pups\\20190803\\20190803_01"
 # species1<<-Species
  
  
  
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
  
  PolygonDir=paste0(labelInput,"\\Polygons"); dir.create(PolygonDir)
  ObserverCountDir=paste0(labelInput,"\\Observer count"); dir.create(ObserverCountDir)
  AnimalMeasurDir=paste0(labelInput,"\\Polygons\\Animal_measurments"); dir.create(AnimalMeasurDir)
  HauloutDir=paste0(labelInput,"\\Polygons\\Haulout"); dir.create(HauloutDir)
  RookeryDir=paste0(labelInput,"\\Polygons\\Rookery"); dir.create(RookeryDir)
  ExcludeDir=paste0(labelInput,"\\Polygons\\Exclude"); dir.create(ExcludeDir)
  ModelPolDir=paste0(labelInput,"\\Polygons\\Model"); dir.create(ModelPolDir)
  ErrDir=paste0(labelInput,"\\Error");dir.create(ErrDir);PointsErrDir=paste0(ErrDir,"\\Points");dir.create(PointsErrDir)
  
  if (Species=="SSLAdult") {file.copy(ModelPOlSSL138,ModelPolDir)
                            file.copy(SSLageCategory,paste0(ObserverCountDir,"\\",NewNameAge))
							}
 if (Species=="NFSAdult"){file.copy(NFSageCategory,paste0(ObserverCountDir,"\\",NewNameAge))}


############################

 # file.rename(OPPname1,Rename1)
 # file.rename(OPPname2,Rename2)	
 # file.rename(OPPkmzfrom,OPPkmzto)

  
  
#}