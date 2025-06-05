   
	   library(Rvcg)
	   library(rgl)
	   library(sf)
	   library(Morpho)
	   #library(geometry)    # v0.4.7: выпуклые оболочки
	   library(XML)
	   library(dplyr)
       library(dbscan)
       library(alphashape3d)
 crs    <- 32610 
 E=430000
 N=5451000
 A= -100


 #labelInput  =  "D:\\PV_DB\\2023_H0052A_OPP\\20230615_103330\\20230615_103330_MAVIC2PRO_40m"  
 labelInput  =  "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m" 
 

 Species = "LRG"
 date1=substr(basename(labelInput),1,15)
 predDir=paste0(labelInput,"\\Predict")
 dir.create(predDir,showWarnings = F)

 PolPth=paste0(labelInput,"\\Predict\\SpP_LRG_",date1,".kml")
# site_3d_local_pth= paste0(labelInput,"\\",date1,"_3D_local.ply")
 site_3d_gps_pth= paste0(labelInput,"\\",date1,"_3D_gps.ply")
# ground_pth= paste0(labelInput,"\\",date1,"_3D_gps_ground.ply")
# unzipDocFrom = paste0(labelInput,"\\",basename(labelInput),".files\\0\\0\\orthomosaic\\orthomosaic.zip")
# doc_pth=paste0(predDir,"\\doc.xml")

########################################################################
 # unzip(zipfile=unzipDocFrom,exdir=predDir)
  poly_sf <- st_read(PolPth) ;poly_sf= poly_sf %>% st_transform(crs)
  site_3d_gps = vcgImport(site_3d_gps_pth)
 # site_3d_local = vcgImport(site_3d_local_pth)
 # site_3d_local_ply = vcgImport(site_3d_local_ply_pth)
 # site_3d_ground <- vcgImport(ground_pth)
###############################################################
   site_3d_crs=site_3d_gps
   site_3d_crs$vb[1,]=site_3d_crs$vb[1,]+E
   site_3d_crs$vb[2,]=site_3d_crs$vb[2,]+N
   #site_3d_crs$vb[3,]=site_3d_crs$vb[3,]+A
   vertices_crs <- st_as_sf(data.frame(x = site_3d_crs$vb[1,], y = site_3d_crs$vb[2,], z = site_3d_crs$vb[3,]), 
                     coords = c("x", "y","z"), crs = crs)						 
	#thinned_points_local <- vertices_crs %>% 
  #   slice_sample(prop = 0.01)
	# st_write(thinned_points_local, "thinned_points_gps.kml", driver = "KML",delete_dsn = T)
	singlePol=poly_sf$geometry[1]
	inside <- st_intersects(vertices_crs, singlePol, sparse = FALSE)
    points_inside = vertices_crs[inside,]
	points_inside
#######################
   coords <- st_coordinates(points_inside)
   coords[,1]=coords[,1]- E    #min(site_3d_gps$vb[1,]) # convert to local to look
   coords[,2]=coords[,2]- N  #min(site_3d_gps$vb[2,]) # convert to local to look
  # coords[,3]=coords[,3]- A 

    MedianLevel = median(coords[,3]) 
	UpLevel= MedianLevel+1
	DownLevel=MedianLevel-1

		points_cut <- coords[coords[,3]> DownLevel, ]                   #обрезка по уровню земли
		points_cut <- points_cut[points_cut[,3]< UpLevel, ]                   #обрезка по уровню земли
		#points_cut
	
    mesh1 <- vcgBallPivoting(points_cut, radius = 0.05)  #  mesh create
    mesh2 <- vcgUpdateNormals(mesh1)
	boundary_edges <- Rvcg::vcgBorder(mesh2)[[1]]
    border_vertices <- vert2points(mesh2)[boundary_edges, ]  # Выделяем граничные вершины
    alpha_shape <- ashape3d(border_vertices, alpha = 0.4)  # Строим α-форму (подбираем alpha эмпирически)
    closed_mesh <- as.mesh3d(alpha_shape)
	
	
	
	# Объединение вершин и граней
   combined_mesh <- list(
  vb = cbind(closed_mesh$vb, mesh2$vb), # Объединение вершин
  it = cbind(closed_mesh$it, mesh2$it + ncol(closed_mesh$vb)) # Объединение граней с коррекцией индексов
)
   # Преобразование в класс mesh3d
    class(combined_mesh) <- "mesh3d"
    # Очистка меша (удаление дубликатов и нереферентных вершин)
    mesh4 <- vcgClean(combined_mesh, sel = c(1:6), iterate = TRUE)
	shade3d(mesh4 , color = "lightblue", alpha = 0.5)
		bbox_lines <- rgl::bbox3d(color = "gray")
	vcgVolume(mesh4) 

	
	
	
	
	
	
	
	
	
	
	
	
	
    closed_seal <- mergeMeshes(mesh2, closed_mesh)

	final_mesh <- vcgUpdateNormals(UniformRemesh)
	shade3d(final_mesh , color = "lightblue", alpha = 0.5)
	
	
	
    mesh3 <- vcgBallPivoting(closed_seal, radius = 0.05)
    shade3d(mesh3 , color = "lightblue", alpha = 0.5)
	mesh4 <- vcgClean(mesh3, sel = 6)
	shade3d(mesh4 , color = "lightblue", alpha = 0.6)
	bbox_lines <- rgl::bbox3d(color = "gray")
     mesh4 <- vcgClean(mesh3, sel = 7)
	
	
	closed_mesh <- vcgClean(mesh4, sel = c("fill"), iterate = TRUE)
	
	 vcgVolume(closed_mesh)            # volume seal with ground
	
	
	shade3d(closed_mesh, color = "gray", alpha = 0.6)
	shade3d(mesh4, color = "red", alpha = 0.4)
	points3d(border_vertices, col = "red", size = 7)
	bbox_lines <- rgl::bbox3d(color = "gray")
	
	

volume_ashape3d(alpha_shape) # volume of ground
 vcgVolume(closed_seal)            # volume seal with ground
	
	
	
	
	
	


draft=function(){

	
	
    shade3d(mesh4 , color = "lightblue", alpha = 0.5)
   vertices <- vert2points(mesh3)
   bbox <- apply(vertices, 2, range)  # Минимальные и максимальные координаты

# Генерация случайных точек в bounding box
n_points <- 1000000  # Желаемое количество точек
random_points <- matrix(
  runif(n_points * 3, min = bbox[1, ], max = bbox[2, ]),
  ncol = 3,
  byrow = TRUE
)
 
 kdtree <- vcgCreateKDtree(mesh3)
 
 # Находим ближайшие точки на меше и их расстояния (с учетом знака)
closest <- vcgClostKD(
  x = random_points,
  mesh = mesh3,
  sign = TRUE,          # Возвращает знаковое расстояние
  threads = 4          # Используем 4 потока для ускорения

)
 
 inside_points <- random_points[closest$quality > -0.001 &  closest$quality < 0.001 , ]

 shade3d(mesh4, color = "lightblue", alpha = 0.9)


# Меш (прозрачный)
shade3d(mesh3, color = "lightblue", alpha = 0.4)
# Ограничивающий бокс (для проверки)
bbox_lines <- rgl::bbox3d(color = "gray")
# Точки внутри меша
points3d(inside_points, col = "red", size = 2)

 
 
 
# Определяем размеры ограничивающего бокса
verts <- t(mesh3$vb[1:3,])
bbox <- apply(verts, 2, range)  # min и max по X,Y,Z

# Задаем разрешение сетки (например, 100x100x100)
grid_res <- 100

# Преобразуем меш в логическую 3D-матрицу (TRUE = внутри меша)
grid <- meshToGrid(
  mesh3,
  nx = grid_res, ny = grid_res, nz = grid_res,
  xmin = bbox[1,1], xmax = bbox[2,1],
  ymin = bbox[1,2], ymax = bbox[2,2],
  zmin = bbox[1,3], zmax = bbox[2,3]
)
 
 
 
 
 
 
 
 
 
 
 
 
 library(misc3d)

# Вокселизация меша
grid <- meshToGrid(mesh, nx = 50, ny = 50, nz = 50)

# Находим внешние воксели (через морфологическую эрозию)
library(EBImage)
grid_bin <- as.Image(grid)
grid_eroded <- erode(grid_bin, kern = makeBrush(3, shape = "box"))
outer_shell <- grid & !grid_eroded

# Преобразуем обратно в меш
verts <- which(outer_shell, arr.ind = TRUE)
mesh_shell <- vcgDelaunay(verts) # Триангуляция

# Визуализация
shade3d(mesh_shell, col = "red", alpha = 0.5)
wire3d(mesh, col = "gray") # Исходный меш для сравнения
 
 
 
 
 
 
 
 
 
 
 
 uniform_points <- vcgUniformRemesh(
  mesh3,
  voxelSize = 0.01,  # Размер вокселя (меньше = плотнее сетка)
  discretize = TRUE  # Создавать дискретные точки
)
 # Визуализация
shade3d(mesh3, color = "blue", alpha = 0.4)
points3d(t(uniform_points$vb[1:3,]), col = "green", size = 3, alpha = 0.4)
 bbox_lines <- rgl::bbox3d(color = "gray")
 
 
 
 
 
 
 
 
 
# Находим ближайшие точки на меше и их нормали
closest <- vcgClost(random_points, mesh3, sign = TRUE)

# Точки с отрицательным знаком лежат внутри меша
internal_points <- random_points[closest$quality > 0, ]

# Визуализация
library(rgl)
shade3d(mesh3, col = "lightblue", alpha = 0.2)
points3d(internal_points, col = "red", size = 2)



shade3d(random_points, col = "lightblue", alpha = 0.2)



 
   
   
   
 shade3d(NewShape1 , color = "lightblue")
 


volume_ashape3d(alpha_shape)

 vcgVolume(mesh3)
  
  

  mesh2 <- vcgUpdateNormals(seal_mesh)
 mesh1 <- vcgBallPivoting(seal_mesh, radius = 0.05)
  shade3d(seal_mesh , color = "lightblue")







  
  
  mesh4 <- vcgIsolated(mesh3)
  mesh5 <- vcgUpdateNormals(mesh4)

  mesh4=vcgClean(mesh3,sel=0:6,iterate=TRUE)
  
   mesh7 <- vcgUpdateNormals(mesh6)
   
  shade3d(mesh7 , color = "lightblue")
  
  
  vcgVolume(mesh3)
  
 ############################################################# 
  
#  open3d()
  
####
vcgBallPivoting
mesh=Seal_mesh
# Построение графа смежности
adj_matrix <- vcgVertexNeighbors(mesh) |> 
  as_adjacency_matrix()


graph_cut <- min_cut(
  graph_from_adjacency_matrix(adj_matrix),
  source = which.min(coords[,3]), # Нижние точки как "источник"
  target = which.max(coords[,3])  # Верхние точки как "сток"
)




################  
vertices <- t(Seal_mesh$vb[1:3,])# Получаем вершины меша
hull <- convhulln(vertices, options="Qt")# Строим выпуклую оболочку
convex_mesh <- list(vb=Seal_mesh$vb, it=t(hull))# Создаем новый меш
class(convex_mesh) <- "mesh3d"
shade3d(convex_mesh,  color = "lightblue")# Визуализация
   vcgVolume(convex_mesh)
   vcgArea(convex_mesh)
#####################################   

   
 ############################################################## 


#  doc <- htmlParse(doc_pth)
 # leftOPP=  as.numeric(xmlValue(getNodeSet(doc, "//extent//left")))
 # topOPP=  as.numeric(xmlValue(getNodeSet(doc, "//extent//top")))
 # rightOPP=  as.numeric(xmlValue(getNodeSet(doc, "//extent//right")))
 # bottomOPP=  as.numeric(xmlValue(getNodeSet(doc, "//extent//bottom")))
  
  # crs= xmlValue(getNodeSet(doc, "//crs"))
  
#  Width_mesh=  abs(min(site_3d_local$vb[1,])) + abs(max(site_3d_local$vb[1,]))
#   Hight_mesh=  abs(min(site_3d_local$vb[2,]))+ abs(max(site_3d_local$vb[2,]))
   
   #Width_mesh_gps=  max(site_3d_gps$vb[1,]) - min(site_3d_gps$vb[1,])
  # Hight_mesh_gps=  max(site_3d_gps$vb[2,]) - min(site_3d_gps$vb[2,])
   
 #  Width_OPP = rightOPP - leftOPP
 #  Hight_OPP= topOPP -bottomOPP
   
  # LagCRS1 = 430437.7+79.5   #  430517.2
 #  LagCRS2 = 5451033+35.2    # 5451068
   
   # widthNorm = site_3d_local$vb[1,] + abs(min(site_3d_local$vb[1,]))
	#hightNorm = site_3d_local$vb[2,] + abs(min(site_3d_local$vb[2,]))
	
   # width_proportion = widthNorm/Width_mesh
  #  hight_proportion = hightNorm/Hight_mesh

	 
	#width_convert= min(site_3d_gps$vb[1,]) +  (Width_mesh_gps * width_proportion)
	#hight_convert=  min(site_3d_gps$vb[2,]) +  (Hight_mesh_gps * hight_proportion)
	
#	width_convert= leftOPP +  (Width_OPP * width_proportion)
#	hight_convert=  bottomOPP +  (Hight_OPP * hight_proportion)
	
  # LagCRS1 = leftOPP+Width_mesh + 0.7627411   #  430437.7 Width_mesh
  # LagCRS2 = bottomOPP+Hight_mesh +  12.01926    #5451033 Hight_mesh
   
 #  site_3d_crs=site_3d_local
#   site_3d_crs$vb[1,] =width_convert #site_3d_crs$vb[1,] + LagCRS1
 #  site_3d_crs$vb[2,] =hight_convert #site_3d_crs$vb[2,] + LagCRS2
 #  site_3d_crs$vb[3,] =site_3d_crs$vb[3,] + 17.7
   
   	# 3. Пространственная кластеризация
	#coords <- t(mesh$vb[1:3,])
	dbscan_res <- dbscan(st_coordinates(points_inside), eps = 0.3, minPts = 50)  # параметры кластеризации
	# 4. Определение основного кластера (предположительно тюлень)
	clusters <- data.frame(cluster = dbscan_res$cluster)
	#main_cluster <- which.max(table(clusters))
	points_inside$cluster=clusters
	cluster_points <- points_inside[points_inside$cluster==0,]
   
	#cutoff_z <- median(st_coordinates(points_inside)[,3]) + 0.01*sd(st_coordinates(points_inside)[,3]) # вычисление уровня земли
   # filtered_points <- points_inside[st_coordinates(points_inside)[,3] > cutoff_z, ]                   #обрезка по уровню земли
	
	# 7. Геометрическая фильтрация
# Вычисляем расстояние до центра масс

  vertices <- Seal_mesh$vb[1:3,]  # Вершины (x,y,z)
  faces <- Seal_mesh$it           # Грани (индексы вершин)

# Создание rgl меша
rgl_mesh <- tmesh3d(
  vertices = vertices,
  indices = faces,
  homogeneous = T
)

   shade3d(rgl_mesh , color = "lightblue")
   
   
   clean_mesh <- vcgClean(
  Seal_mesh,
  sel = c(1, 1, 1, 1, 1), # Включить все виды очистки
  tol = 0.01 # Допуск в метрах
)

# Сглаживание (без потери деталей)
  smooth_mesh <- vcgSmooth(
  clean_mesh,
  type = "HC", # Алгоритм Taubin
  lambda = 0.5,
  mu = -0.5,
  iteration = 3
)



cln_mesh=vcgClean(smooth_mesh, sel=0:6) 

# Hole filling
filled_mesh <- vcgClean(cln_mesh, sel = 7)  # Option 7 fills holes
repaired <- vcgClean(filled_mesh, sel = 0) # Repair non-manifold edges
corrected <- vcgUpdateNormals(repaired) # Fix flipped normals
components <- vcgIsolated(corrected) # Handle disconnected components
###################################

vcgVolume(corrected)
vcgArea(mesh)
# Compute geodesic distances
dist_matrix <- vcgGeodist(mesh)





	  shade3d(smooth_mesh, color = "lightblue")
	 
st_write(points_inside, "points_inside_local.kml", driver = "KML",delete_dsn = T)
   
   ###########
    vertices_gps <- st_as_sf(data.frame(x = site_3d_gps$vb[1,], y = site_3d_gps$vb[2,], z = site_3d_gps$vb[3,]+17.7), 
                     coords = c("x", "y","z"), crs = 32610)	
					 
	thinned_points_gps <- vertices %>% 
     slice_sample(prop = 0.1)
	 # slice(1:10)	
	 	 
st_write(thinned_points_gps, "thinned_points_gps.kml", driver = "KML",delete_dsn = T)
   
   
   #NewRef=data.frame(meshvb1=mesh$vb[1,],meshvb2=mesh$vb[2,])
  # NewRef$meshvb1New = signif(Width_OPP*(abs( NewRef$meshvb1)/Width_mesh) + leftOPP,digits = 16)
  # NewRef$meshvb2New =   signif(Length_OPP*(abs( NewRef$meshvb2)/Length_mesh) + bottomOPP,digits = 16)
  
 #  mesh$vb[1,] = NewRef$meshvb1New
  # mesh$vb[2,] = NewRef$meshvb2New
    
  # mesh$vb[1,]=as.numeric(mesh$vb[1,])
  # mesh$vb[2,]=as.numeric(mesh$vb[2,])
 
 


 #  for (i in 1:length(vb1)){
 #  seq = vb1[i]
 #  seqabs=abs(seq) 
 #  seqProportion =  seqabs/Width_mesh
 #  CRSvalue = Width_OPP*seqProportion+ leftOPP
 #  vb1New=c(vb1New,CRSvalue) 
 #  }



MeshConvertToMeters=function(mesh,crs){
meshM=mesh
crs = 32610

vertices <- st_as_sf(data.frame(x = meshM$vb[1,], y = meshM$vb[2,]), 
                     coords = c("x", "y"), crs = 32610)				 
	thinned_points <- vertices %>% 
     slice_sample(prop = 0.9) 	
st_write(thinned_points, "thinned_points.kml", driver = "KML")

 # Преобразуем в UTM Zone 18N (EPSG:32618) для восточной Канады
 #UTM Zone 10N (Ванкувер): EPSG:32610
 
vertices_local <- st_transform(vertices, 32610)
# Извлекаем координаты в метрах
coords_local <- st_coordinates(vertices_local)
# Обновляем вершины меша
mesh$vb[1,] <- coords_local[,1]
mesh$vb[2,] <- coords_local[,2]


center_1 <- mean(mesh$vb[1,])
center_2 <- mean(mesh$vb[2,])


#centr=apply(mesh$vb[1:3,], 1, mean)
# Смещаем меш к началу координат
mesh$vb[1,] <- mesh$vb[1,] - center_1
mesh$vb[2,] <- mesh$vb[2,] - center_2

return(mesh)
}
shade3d(mesh, color="lightblue")

###



##################################################################################
#insite1=unlist(filtered_points)
 #a=c(1: 2808035)
 



min(site_3d_local$vb[1,]); max(site_3d_local$vb[1,])
min(site_3d_local$vb[2,]); max(site_3d_local$vb[2,])
min(site_3d_local$vb[3,]); max(site_3d_local$vb[3,])

min(site_3d_CONVERT$vb[1,]); max(site_3d_CONVERT$vb[1,])
min(site_3d_CONVERT$vb[2,]); max(site_3d_CONVERT$vb[2,])
min(site_3d_CONVERT$vb[3,]); max(site_3d_CONVERT$vb[3,])



open3d()
shade3d(cropped, color="lightblue")

min(site_3d_nativ$vb[1,]);max(site_3d_nativ$vb[1,])
min(site_3d_nativ$vb[2,]);max(site_3d_nativ$vb[1,])
min(site_3d_nativ$vb[3,]);max(site_3d_nativ$vb[1,])

min(site_3d_local$vb[1,]);max(site_3d_local$vb[1,])
min(site_3d_local$vb[2,]);max(site_3d_local$vb[1,])
min(site_3d_local$vb[3,]);max(site_3d_local$vb[1,])



min(site_3d_nativ$vb[1,]);max(site_3d_nativ$vb[1,])
min(site_3d_nativ$vb[2,]);max(site_3d_nativ$vb[1,])
min(site_3d_nativ$vb[3,]);max(site_3d_nativ$vb[1,])

length(site_3d_nativ$vb[1,])
length(site_3d_local$vb[1,])


length(site_3d_nativ$vb[2,])
length(site_3d_local$vb[2,])

length(site_3d_nativ$vb[3,])
length(site_3d_local$vb[3,])

length(unique(site_3d_local$vb[1,]))
length(unique(site_3d_local$vb[2,]))
length(unique(site_3d_local$vb[3,]))

 #site_3d_gps = vcgImport(site_3d_gps_pth)
length(unique(site_3d_nativ$vb[1,]))
length(unique(site_3d_nativ$vb[2,]))
length(unique(site_3d_nativ$vb[3,]))

length(site_3d_nativ$vb[1,])
length(site_3d_nativ$vb[2,])
length(site_3d_nativ$vb[3,])



open3d()
cropped <- rmVertex(site_3d, !points_id)


verts=data.frame(lat=site_3d_gps$vb[1,], lon=site_3d_gps$vb[2,])
verts$uniqverts=paste0(verts$lat,"_",verts$lon)
length(unique(verts$uniqverts))



########################################################################
# Crop vertices first before boolean op
#st_crs(current_poly)=crs
#st_transform(current_poly,crs)

poly_sf=st_transform(poly_sf,crs)

#current_poly <- poly_sf[i,]$geometry

verts <- t(site_3d_gps$vb[1:3,])
points=st_as_sf(data.frame(verts), coords = 1:2)
st_crs(points)=crs

points$points_id=c(1:length(points$X3))
singlePol = poly_sf$geometry

singlePol=st_transform(singlePol,crs)
points=st_transform(points,crs)

insite <- st_intersects(singlePol, points, sparse = FALSE)
ins=points[insite,]
points_id=ins$points_id
cropped <- rmVertex(site_3d, !points_id)





grid <- st_make_grid(points, n = c(10, 10))
grid <- st_as_sf(grid) 
grid$grid_id = c(1:length(grid$x))


grid_intersects <- grid[st_intersects(grid, poly_sf, sparse = FALSE),]
points_with_grid <- st_join(points, grid, join = st_intersects)

filtered_points <- points_with_grid %>% 
  filter(grid_id %in% grid_intersects$grid_id)
  

#plot(poly_sf)
#plot(filtered_points, add=T)
#plot(poly_sf, add=T)

plot(singlePol)

insite <- st_intersects(singlePol,filtered_points, sparse = FALSE)
ins=filtered_points[insite,]
points_id=ins$points_id
#inside <- st_intersection(poly_sf,verts1)

site_3d_CONVERT = MeshConvertToMeters(site_3d_gps)
open3d()
shade3d(site_3d_CONVERT, color="lightblue")






#################################################
MeshConvertToSnmetrs=function(mesh){
 lat=mesh$vb[1,]
 lon=mesh$vb[2,]
 alt=mesh$vb[3,]

latmin=min(lat)
latmax=max(lat)

lonmin=min(lon)
lonmax=max(lon)

points1=c(latmin,lonmin)
points2=c(latmax,lonmax)

p1 <- st_sfc(st_point(points1), crs = crs) 
p2 <- st_sfc(st_point(points2), crs = crs) 

dist = st_distance(p1, p2)
step=length(mesh$vb[1,])



}
##########

#############################################################################
########################################################################
min_z <- min(obj_mesh$vb[3,])
max_z <- max(obj_mesh$vb[3,])
z_range <- range(obj_mesh$vb[3,])

# Function to extrude 2D polygon to 3D volume
extrude_polygon <- function(poly, zmin, zmax) {
  # Create bottom face
  bottom <- cbind(st_coordinates(poly)[,1:2], zmin)
  # Create top face
  top <- cbind(st_coordinates(poly)[,1:2], zmax)
  # Combine to create prism
  list(
    vb = t(cbind(bottom, top)),
    it = NULL # Will generate faces later
  )
}
#------------------
# Process each polygon
results <- lapply(1:nrow(poly_sf), function(i) {
  # Get current polygon
  current_poly <- poly_sf[i,]$geometry
  
  # Create 3D clipping volume
  clip_volume <- extrude_polygon(current_poly, z_range[1], z_range[2])
  
  # Convert to proper mesh (using Morpho)
  clip_mesh <- list(
    vb = clip_volume$vb,
    it = matrix(1:ncol(clip_volume$vb), nrow = 3)
  )
  
  # Crop original mesh (using Rvcg)
  cropped <- vcgIntersect(obj_mesh, clip_mesh)
  
  # Make solid (close holes and ensure watertight)
  solid_mesh <- vcgClean(cropped, sel = c(0, 7)) # 0=clean, 7=fill holes
  
  # Add original colors if they exist
  if(!is.null(obj_mesh$material)) {
    solid_mesh$material <- obj_mesh$material
  }
  
  return(solid_mesh)
}
###########################
bbox <- st_bbox(poly_sf)

# Filter vertices within bounding box
vert <- t(obj_mesh$vb[1:3,])

keep <- vert[,1] >= bbox$xmin & vert[,1] <= bbox$xmax &
         vert[,2] >= bbox$ymin & vert[,2] <= bbox$ymax &
         vert[,3] >= min_z & vert[,3] <= max_z


cropped_mesh <- rmVertex(obj_mesh, !keep)




###################################
cln_mesh=vcgClean(obj_mesh, sel=0:6) 
smooth_mesh <- vcgSmooth(cln_mesh, iteration = 3)
# Hole filling
filled_mesh <- vcgClean(smooth_mesh, sel = 7)  # Option 7 fills holes
repaired <- vcgClean(filled_mesh, sel = 0) # Repair non-manifold edges
corrected <- vcgUpdateNormals(repaired) # Fix flipped normals
components <- vcgIsolated(corrected, type = "vert") # Handle disconnected components
###################################

vcgVolume(corrected)
vcgArea(mesh)
# Compute geodesic distances
dist_matrix <- vcgGeodist(mesh)



poly_3d <- vcgBorder(poly_sf)  # Convert to 3D border



#vcgMeshres(cln_mesh)  


# Transform to match OBJ coordinates if needed
# OBJs typically use local coordinates or UTM
#cropping_polygons <- st_transform(cropping_polygons, target_crs)


# Add elevation range (Z-values) to your polygons
min_z <- min(obj_mesh$vb[3,])
max_z <- max(obj_mesh$vb[3,])

# Create 3D extrusion from 2D polygons
polygons_3d <- extrude3d(cropping_polygons), zmin = min_z, zmax = max_z)
}
