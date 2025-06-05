   
	   library(Rvcg)
	   library(rgl)
	   library(sf)
	   library(Morpho)
  # library(rayshader)

 crs    = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
 labelInput  = "D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m"       
 Species = "LRG"
 date1=substr(basename(labelInput),1,15)
 PolPth=paste0(labelInput,"\\Predict\\SpP_LRG_",date1,".kml")
 Mesh3dPth= paste0(labelInput,"\\",date1,"_3D_Model.obj")



#site_3d_local <- vcgImport("D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m\\20230518_104633_3D_Model_local.obj")
site_3d_gps <- vcgImport("D:\\PV_DB\\2023_H0052A_OPP\\20230518_104633\\20230518_104633_MINI2_20m\\20230518_104633_3D_Model_GPS.obj")

poly_sf <- st_read(PolPth)
##################################################
MeshConvertToMeters=function(mesh){
vertices <- st_as_sf(data.frame(x = mesh$vb[1,], y = mesh$vb[2,], z = mesh$vb[3,]), 
                     coords = c("x", "y", "z"), crs = crs)

 # Преобразуем в UTM Zone 18N (EPSG:32618) для восточной Канады
 #UTM Zone 10N (Ванкувер): EPSG:32610
 
vertices_local <- st_transform(vertices, 32618)
# Извлекаем координаты в метрах
coords_local <- st_coordinates(vertices_local)
# Обновляем вершины меша
mesh$vb[1,] <- coords_local[,1]
mesh$vb[2,] <- coords_local[,2]
mesh$vb[3,] <- coords_local[,3]  # Высота также теперь в метрах

center_1 <- mean(mesh$vb[1,])
center_2 <- mean(mesh$vb[2,])
center_3 <- mean(mesh$vb[3,])

#centr=apply(mesh$vb[1:3,], 1, mean)
# Смещаем меш к началу координат
mesh$vb[1,] <- mesh$vb[1,] - center_1
mesh$vb[2,] <- mesh$vb[2,] - center_2
mesh$vb[3,] <- mesh$vb[3,] - center_3
return(mesh)
}


site_3d_CONVERT = MeshConvertToMeters(site_3d_gps)
open3d()
shade3d(site_3d_CONVERT, color="lightblue")


min(site_3d_local$vb[1,]); max(site_3d_local$vb[1,])
min(site_3d_local$vb[2,]); max(site_3d_local$vb[2,])
min(site_3d_local$vb[3,]); max(site_3d_local$vb[3,])

min(site_3d_CONVERT$vb[1,]); max(site_3d_CONVERT$vb[1,])
min(site_3d_CONVERT$vb[2,]); max(site_3d_CONVERT$vb[2,])
min(site_3d_CONVERT$vb[3,]); max(site_3d_CONVERT$vb[3,])





########################################################################
# Crop vertices first before boolean op
#st_crs(current_poly)=crs
#st_transform(current_poly,crs)

poly_sf=st_transform(poly_sf,crs)

#current_poly <- poly_sf[i,]$geometry

verts <- t(site_3d$vb[1:3,])
points=st_as_sf(data.frame(verts), coords = 1:2)
st_crs(points)=crs

points$points_id=c(1:length(points$X3))
singlePol = poly_sf[3,]$geometry

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
#####################################################################################
#insite1=unlist(filtered_points)
 #a=c(1: 2808035)
 
cropped <- rmVertex(site_3d, !points_id)

open3d()
shade3d(cropped, color="lightblue")












































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

