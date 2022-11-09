

library(XML)
library(tidyr)
library(dplyr)

OPP_PointsTable=NULL
OPP_InfoTable=NULL
  
  labelInput1<<-labelInput
  labelinput1<<-labelInput
OPP_folder <<-labelInput
###############################################################################################
GetIMGGEOINFOAgisoft=function (OPP_folder) {
date_time=substr(basename(labelinput1),1,15)
OPP_InfoTablePth=paste0(labelinput1,"\\Agisoft_IMG_INFO_",date_time,".csv")
 date1=date_time
 
  pth.doc.zip=list.files(OPP_folder, recursive=T, pattern="frame.zip",full.names=T)
   
  if (file.exists(pth.doc.zip)==F){stop("Can't get Info from OPP")}
   if (length(pth.doc.zip)>1){stop("More then One chunk in the OPP or several OPP in one dir")} 
                          
  
    to=paste0(OPP_folder,"\\tmp")
    if(dir.exists(to)==F) {dir.create(to)}
    unzip(pth.doc.zip,exdir=to)
    doc.kml=paste0(OPP_folder,"\\tmp\\doc.xml")
    doc <- htmlParse(doc.kml)
    #########################
    IMG=   sapply(getNodeSet(doc, "//photo"), xmlAttrs)
    IMG=data.frame(IMG=IMG)
    ImgInfo=getNodeSet(doc, "//photo")
    TableImgInfo=NULL
    for (i in 1:length(IMG$IMG)) {
      row1=ImgInfo[[i]]
      imgName=paste0(xmlAttrs(row1))
      children=   xmlChildren(row1)
      for (y in 2:29) {
        childrenRow=children[y]$property
		if (is.null(childrenRow) != T) {
        childrenRow= xmlAttrs(childrenRow)
        childrenRowName= paste0(childrenRow[1])
        childrenRowValue= paste0(childrenRow[2])
        finRow=data.frame(Img=imgName,Property=childrenRowName,Value=childrenRowValue)
        TableImgInfo=rbind(finRow,TableImgInfo)
		}
      }}
    TableImgInfo$IMGbasename=basename(paste0(TableImgInfo$Img))
	TableImgInfo$date=basename(OPP_folder)
    write.csv(TableImgInfo,OPP_InfoTablePth)
    return(TableImgInfo)
  }
GetIMGGEOINFOAgisoft(OPP_folder=labelInput)

########################################################################################
#for (k in 1:length(listOPP2)) {
# PointsTable=NULL
# PointsTable=GetShapeAgisoft(listOPP2[k])
# PointsTable=PointsTable[is.na(PointsTable$label)==F,]
# OPP_PointsTable=rbind(OPP_PointsTable,PointsTable)
# OPP_PointsTable<<-OPP_PointsTable[OPP_PointsTable$type=="point",]
#}
#######################################################################################
#for (n in 1:length(listOPP2)) {
#InfoTable=GetIMGGEOINFOAgisoft(listOPP2[n])
#OPP_InfoTable <- rbind(OPP_InfoTable,InfoTable)
#}
#ncharBN= nchar(basename(paste0(OPP_InfoTable$Img)))+1
#OPP_InfoTable$foldName=    basename(substr(OPP_InfoTable$Img,0,nchar(paste0(OPP_InfoTable$Img))-ncharBN))
#OPP_InfoTable<<-OPP_InfoTable
#######################################################################################
#  OPP_PointsTableSum=OPP_PointsTable %>% 
#    group_by(date1,label) %>%
#    summarise(count=n()) 
#  OPP_PointsTableSum <<- data.frame(OPP_PointsTableSum) 
 ################################################################################## 
#  Dtime= paste0(OPP_InfoTable$Value[OPP_InfoTable$Property== "Exif/DateTimeOriginal"])
#  FoldName=paste0(OPP_InfoTable$foldName[OPP_InfoTable$Property== "Exif/DateTimeOriginal"])
#  Dtime1=data.frame(Dtime=Dtime,FoldName=FoldName)
#  Dtime1=Dtime1[is.na(Dtime1$Dtime)==F,]
#  Effort <<- Dtime1 %>%
#          group_by(FoldName) %>%
#		  summarise(Start= min(as.character(Dtime)), Stop=max(as.character(Dtime)))
#}


GetShapeAgisoft=function (OPP_folder) {
 if(nchar(basename(OPP_folder))>8) {date1=substr(basename(OPP_folder),1,8)} else {date1=basename(OPP_folder)}
  
 
  pth.doc.zip=paste0(OPP_folder,"\\",date1,".files\\0\\0\\shapes\\shapes.zip")
  if (file.exists(pth.doc.zip)==F) { pth.doc.zip=paste0(OPP_folder,"\\",date1,".files\\0\\0\\shapes.1\\shapes.zip")}
  if (file.exists(pth.doc.zip)==T){
    to=paste0(OPP_folder,"\\tmp")
    if(dir.exists(to)==F) {dir.create(to)}
	 if (length(pth.doc.zip)>1){stop("More then One chunk in the OPP or several OPP in one dir")}
                                warning(labelInput)	 
    unzip(pth.doc.zip,exdir=to)
    doc.kml=paste0(OPP_folder,"\\tmp\\doc.xml")
    doc <- htmlParse(doc.kml)
    #######################################################################
    vertices=   sapply(getNodeSet(doc, "//vertices"), xmlValue)
    vertices=data.frame(vertices=vertices)
    tableVertices1=NULL
    for (i in 1:length(vertices$vertices)) {
      row1=paste0(vertices$vertices[i])
      row2=unlist(strsplit(row1," "))
      for (y in 1:length(row2)) {
        if (y==length(row2)) {break}
        lat=row2[y]
        lon=row2[y+1]
        if (length(row2)>2){type="poligon"} else {type="point"}
        subrow=data.frame(lat=lat,lon=lon,type=type,shape.id=i)
        tableVertices1=rbind(subrow,tableVertices1)
      }
    }
    ##############
    group=   getNodeSet(doc, "//shape")
    groupTable=NULL
    for (e in 1:length(group)) {
      row1=xmlAttrs(group[[e]])
      row1=data.frame(id=row1[1],group_id=row1[2],  type=row1[3])
      groupTable=rbind(groupTable,row1)
    }
    groupTable=data.frame(groupTable)
    groupTable$shape.id=c(1:length(groupTable$type))
    groupTable=groupTable[,2:4]
    #####################
    groups1=getNodeSet(doc, "//groups")
    groups2=getNodeSet(doc, "//group")
    TypeShape=NULL
    for (r in 1:length(groups2)) {
      row1=xmlAttrs(groups2[[r]])
      TypeShape=rbind(TypeShape,row1)
    }
    ###############################   
    tableVertices2=left_join(tableVertices1,groupTable,by="shape.id")
    tableVertices3=data.frame(Lat=as.numeric(paste0(tableVertices2$lat)),Lon=as.numeric(paste0(tableVertices2$lon)),id1=as.factor(tableVertices2$group_id),type=tableVertices2$type.y,shape.id=tableVertices2$shape.id)
    TypeShape=data.frame(TypeShape)
    TypeShape$id1=as.factor(TypeShape$id)
    ResultTable=left_join(tableVertices3, TypeShape,by="id1")
    ResultTable=ResultTable[is.na(ResultTable$id)==F,]
    unlink(to,recursive =T)
  } else {ResultTable=data.frame(Lat= 0,  Lon= 0,  id1= 0,  type= "point",  shape.id= 0,  id= 0,   label= "No count")}
  ResultTable=data.frame(date1,ResultTable)
   ResultTable=ResultTable[is.na(ResultTable$label)==F,]
   ResultTable<<-ResultTable[ResultTable$type=="point",]
  
  return(ResultTable)
}
