CreateTableNear=function (labelinput=labelinput) {
source("Modules/01_create_tile_polygons.r")

generate_image_anchors=function(SpP_data_subset,SpP_data)
{
    require(sf)
	SpP_data_1 <- st_as_sf(SpP_data_subset)


#	grid_1 <- SpP_data_1 %>% 
#	st_make_grid(cellsize = c(median(abs(sp::coordinates(SpP_data)[1:100,1]-sp::coordinates(SpP_data)[2:101,1])), # 
#	median(abs(sp::coordinates(SpP_data)[1:100,2]-sp::coordinates(SpP_data)[2:101,2]))), what = "centers") 


	
	CoordsTable=as.data.frame(sp::coordinates(SpP_data))
	CoordsTable$Lon=CoordsTable$V1;CoordsTable$V1=NULL
	CoordsTable$Lat=CoordsTable$V2;CoordsTable$V2=NULL
	
	  for (i in 2:length(CoordsTable[,1])) { 
	  CoordsTable$LonStep[i]= abs(CoordsTable$Lon[i]-CoordsTable$Lon[i-1])
	   CoordsTable$LatStep[i]= abs(CoordsTable$Lat[i]-CoordsTable$Lat[i-1]) 
	  }
	
	
LatCellSize=median(CoordsTable$LatStep, na.rm = T)
LonCellSize=median(CoordsTable$LonStep, na.rm = T)

grid_1 <- SpP_data_1 %>% 
	st_make_grid(cellsize = c(LonCellSize,LatCellSize) , what = "centers")

	
	
	coords <- do.call(rbind, st_geometry(grid_1))
	grid=SpatialPoints(coords, proj4string = CRS(proj4string(SpP_data)))

#get anchor labels assigned form dataframe

	tile_placement=grid%over%SpP_data
	
#	tile_placement=tile_placement[is.na(tile_placement[,1])==F,]

#get number of columns
	coords1=coords
	coords1[,2]=c(coords[1,2],coords[1:(length(coords[,2])-1),2])

	dx=coords[,2]-coords1[,2]
	ncols=min((1:length(dx))[dx!=0])-1

	# generate anchor matrix
	tile_placement=as.character(tile_placement[,1])
	tile_placement1=matrix(tile_placement[(length(tile_placement):1)],ncol=ncols,byrow=TRUE)
	tile_placement1
}



Poly_Data=create_tile_polygons(labelInput,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Poly_matrix=generate_image_anchors(SpP_data_subset=Poly_Data,SpP_data=Poly_Data)
##############################################################################################################################
dimOPP=dim(Poly_matrix)
numImgs=dimOPP[1]*dimOPP[2]
OPPwidth=dimOPP[2]
OPPhight=dimOPP[1]
vec=rep(0,numImgs)
TableNear =data.frame(imgName=vec,leftName= vec,upName= vec, rightName=vec, downName=vec)
 
line1=1
 for (i in 1:OPPhight){       # обработка строчек
  for (y in 1:OPPwidth){     # обработка столбцов
TableNear$imgName[line1]=as.character(Poly_matrix[i,y])
if (y-1>0 & y < OPPwidth) { TableNear$leftName[line1]=as.character(Poly_matrix[i,y-1])} else { TableNear$leftName[line1]="edge"} #leftName  Y width
if (i-1>0 & i < OPPhight) {TableNear$upName[line1]=as.character(Poly_matrix[i-1,y])} else {TableNear$upName[line1]="edge"}      #upName   I  hight
if (i-1>0 & i < OPPhight) {TableNear$downName[line1]=as.character(Poly_matrix[i+1,y])} else {TableNear$downName[line1]="edge"}   #downName I   hight
if (y-1>0 & y < OPPwidth) { TableNear$rightName[line1]=as.character(Poly_matrix[i,y+1])} else { TableNear$rightName[line1]="edge"}  #rightName  Y width
line1 =line1 +1
}} 
TableNear1=TableNear[is.na(TableNear$imgName)==F,]
TableNear1$leftName=gsub("/","_",TableNear1$leftName)
TableNear1$upName=gsub("/","_",TableNear1$upName)
TableNear1$rightName=gsub("/","_",TableNear1$rightName)
TableNear1$downName=gsub("/","_",TableNear1$downName)
TableNear1$imgName=gsub("/","_",TableNear1$imgName)

TableNear<<-TableNear1
Poly_Data<<-Poly_Data
Poly_matrix <<-Poly_matrix
}

CreateTableNear()
# for (i in 1:length(TableNear1$imgName)) {
# ID=TableNear1$imgName
# bbox=Poly_Data[Poly_Data$ID==ID,]
# bbox=data.frame(bbox@bbox)
# TableNear1$west[i]=bbox$min[1]
# TableNear1$east[i]=bbox$max[1]
# TableNear1$south[i]=bbox$min[2]
# TableNear1$north[i]=bbox$max[2]
# }


