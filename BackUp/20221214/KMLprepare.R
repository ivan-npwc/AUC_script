date1=substr(basename(labelInput),1,15)
###########################################################
GetTableKml= function(doc.kml) {
  doc <- htmlParse(doc.kml)
  west=   as.numeric(sapply(getNodeSet(doc, "//west"), xmlValue))
  east=as.numeric(sapply(getNodeSet(doc, "//east"), xmlValue))
  south=as.numeric(sapply(getNodeSet(doc, "//south"), xmlValue))
  north=as.numeric(sapply(getNodeSet(doc, "//north"), xmlValue))
  image=sapply(getNodeSet(doc, "//href"), xmlValue)
  west=west[seq_along(west) %% 2 > 0]
  east=east[seq_along(east) %% 2 > 0]
  south=south[seq_along(south) %% 2 > 0]
  north=north[seq_along(north) %% 2 > 0]
  pathDir=sub("doc.kml","",doc.kml)
  table=data.frame(west,east,south,north,image)
  table
}
############################################################
listImg=function(KMLpath) { 
options(warn=-1)
  list.of.mask <- list.files(KMLpath) 
  pathFolders=list.dirs(KMLpath,full.names = TRUE,recursive = F)
  KMLimg=NULL
  for (q1 in 1: length(pathFolders)) {
    path=pathFolders[q1]
    imgList= list.files(path) 
    if (length(imgList)>2) {
      doc.kml=paste0(path, "\\","doc.kml")
      geo.infoALL<<-GetTableKml(doc.kml)
      geo.info=geo.infoALL[nchar(as.character(geo.infoALL$image))=="10",] # hier we sort image f livel only
if (length(geo.info[,1]) !=0) {  
      fLevelImg=geo.info$image
      fLevelImgPath=paste0(path,"\\",fLevelImg)
      KMLimg=rbind(KMLimg, data.frame(fLevelImgPath,geo.info,path))
    }}}
  KMLimg
  options(warn=0)
}
######################################################################
KMLprepare=function () {
  to1=  paste0(labelInput,"\\",date1)
  check=list.dirs(to1, recursive = F)
  check1=basename(check)
  check2=check1[1]
  check3<<-paste0(substr(check2,0,1))
 
 

 
    if (check3=="b") {
    TableNear<<-read.csv(paste0(System_data,"/TableNear_B.csv"))
  } 
    if (check3=="c") { 
    TableNear<<-read.csv(paste0(System_data,"/TableNear_C.csv"))
  }
   if (check3=="d") {
    TableNear<<-read.csv(paste0(System_data,"/TableNear_D.csv"))
  }
   if (check3=="e") {
    TableNear<<-read.csv(paste0(System_data,"/TableNear_E.csv"))
  } 
     if (check3=="f") {
    TableNear<<-read.csv(paste0(System_data,"/TableNear_F.csv"))
  } 
  
  
  
   if (!check3 %in% c("e","c","d","b","f")) { source("Modules/02_generate_image_anchors.r")}
  
  KMLpath<<-paste0(labelInput,"\\",date1)
  KMLimg<<-listImg(KMLpath)
  KMLimg$fold=basename(gsub("/","\\\\",KMLimg$path))
  KMLimg$link =  paste0(KMLimg$fold,"_",KMLimg$image,  sep="")
  KMLimg$format.img= substr(KMLimg$link,13,16) 
  
 
  
  ##########################################################################
  library(parallel)
  library(doParallel) 
  library(foreach)
  cl <- makePSOCKcluster(detectCores (logical=F)-1) 
  clusterEvalQ(cl, {
    library(magick)				  
  })
  registerDoParallel(cl)
 foreach(e = 1:length(KMLimg$link)) %dopar% {	
  #  for (e in 1:length(KMLimg$link)) {
    
    path=paste0(KMLimg$fLevelImgPath[e])
    if (strsplit(basename(path), split="\\.")[[1]][2] != "jpg") {
      
      new.path=gsub("png","jpg",path)
      if (file.exists(path)==T) {
        img=image_read(path)
        image_write(img, path =new.path , format = "jpg")
        unlink(path)
      }
    } 
  }
  stopCluster(cl)
  
  
  KMLimg$fLevelImgPath=gsub("png","jpg",KMLimg$fLevelImgPath)
  KMLimg$link=gsub("png", "jpg",KMLimg$link)
  TableNear$imgName=gsub("png", "jpg",TableNear$imgName)
   KMLimg$link=gsub("png", "jpg",KMLimg$link)
  
  ######################################################################################
  TableNear_forWrite<<-TableNear[TableNear$imgName %in% KMLimg$link,]
  

  TableNear_forWrite$leftName=gsub("png", "jpg",TableNear_forWrite$leftName)
   TableNear_forWrite$upName=gsub("png", "jpg",TableNear_forWrite$upName)
    TableNear_forWrite$rightName=gsub("png", "jpg",TableNear_forWrite$rightName)
	 TableNear_forWrite$downName=gsub("png", "jpg",TableNear_forWrite$downName)
  
  
  pth_table<<-paste0(labelInput,"\\", date1,"_table.csv")
  TableNear_forWrite$link=TableNear_forWrite$imgName
  TableNear_forWrite$link=as.character(TableNear_forWrite$link)
  KMLimg$link=as.character(KMLimg$link)
  table1=left_join(KMLimg,TableNear_forWrite)
  table1$date=date1
  write.csv(table1,pth_table, row.names = F)
  
  #########################################################################  
  source("Modules/NormalizePxSiz.r")
}

KMLprepare()
# stopCluster(cl)