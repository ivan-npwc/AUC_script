 PredictionEvaluation=function (labelInput=labelInput,
                                exclude_labels=c("DP","P"),
								generate_combined_map=T,
								resolution=128,
                                ReAvaluation=T
								) {           #this is sufficient resolution to evaluate predictions, since the real informative window is 128 by 128 from 256 by 256 given 25% overlap on each side


library(magrittr)
library(XML)
library(foreach)
library(abind)
library(sp)
library(sf)
library(magick)

        date_1=basename(labelInput)
	#    if(!file.exists(paste0(labelInput,"\\report_",date_1,".csv"))) {
		if(dir.exists(paste0(labelInput,"\\",basename(labelInput)))==F) {source("Modules/Unzip.r")}
		predsDir=paste0(labelInput,"/Predict/Preds")
		shape_file=paste0(labelInput, "/Observer count/",date_1,".shp")
    	if (dir.exists(predsDir)==T & file.exists(shape_file)==T) {

	labelInput=normalizePath(labelInput,winslash="/")
	
#	check_rda_integrity=function(path) {
#	     if(file.exists(paste0(labelInput,"/Predict/",date_1,".rda"))) {integrity=TRUE
#	} else{ integrity=FALSE
#	}
#	}


#	if(!check_rda_integrity(paste0(labelInput,"/Predict/",date_1,".rda")))
    if (ReAvaluation==T){
	#get last kml folder level

		extract_kml=function(date_1,file_name="doc.kml",data_set="root")
		{
			kml_file=paste0(labelInput,"/", date_1 ,"/", file_name)
			data <- xmlParse(kml_file)
			xml_data <- xmlToList(data)


			xml_data_1=xml_data[[1]][[1]]

			level1=lapply(xml_data_1,length)
			folder_1=grep("Folder",names(level1))

			unlevel_xml=function(xml_data,folder_id,data_set=data_set)
			{
			dat=foreach(m = 1:length(folder_id)) %do%
			{
			k=folder_id[m]
			xml_data_2=xml_data[[k]]
			level_2=lapply(xml_data_2,length)
				if("Folder"%in%names(level_2))
				{
					folder_2=grep("Folder",names(level_2))
					dat1=unlevel_xml(xml_data_2,folder_2,data_set=data_set)
					dat1
				}else{
					if(data_set=="root")
					{
						link=grep("NetworkLink",names(level_2))
						dat1=foreach(n=1:length(link)) %do%
						{	
							l=link[n]
							coord=xml_data_2[[l]][[1]][[1]]
							href=xml_data_2[[l]][[2]][[1]]
							dat1=data.frame(west=coord[[1]],east=coord[[2]],south=coord[[3]],north=coord[[2]],href=href)		
						}
						dat1=do.call(abind, c(dat1, list(along = 1)))
						dat1
					}else{
							#coord=xml_data_2[["GroundOverlay"]][["LatLonBox"]][["west"]]
							href=xml_data_2[["name"]]
							dat1=data.frame(west=xml_data_2[["GroundOverlay"]][["LatLonBox"]][["west"]],east=xml_data_2[["GroundOverlay"]][["LatLonBox"]][["east"]],south=xml_data_2[["GroundOverlay"]][["LatLonBox"]][["south"]],north=xml_data_2[["GroundOverlay"]][["LatLonBox"]][["north"]],href=href,stringsAsFactors=FALSE)
					}
				}


			}
			dat
		}
			out=unlevel_xml(xml_data_1,folder_1,data_set=data_set)
		}

		out1=extract_kml(date_1=date_1)
		out1=sapply(out1,function(X) do.call(abind, c(X, list(along = 1))))
		out1=do.call(abind, c(out1, list(along = 1)))
		out1


	#extract last kml folder level for each subfolder

		out3=foreach(k = 1:length(out1[,1])) %do%
			{
					out2=extract_kml(date_1=date_1,
					 file_name=out1[k,"href"],
					 data_set="level_2")
					 
					 
				df <- data.frame(matrix(unlist(out2), ncol=5, byrow=TRUE),stringsAsFactors=FALSE)
				df$path=dirname(out1[k,"href"])
				df
			}


		out3=do.call(abind, c(out3, list(along = 1)))


		#create polygons to generate grid anchors for images
		require(sp)

		srPolygons=list()
		srPolygonsData=list()
		for(i in 1:length(out3[,1]))
		{
			srPolygons[[i]]=Polygons(list(Polygon(cbind(as.numeric(c(out3[i,1],out3[i,1],out3[i,2],out3[i,2])),as.numeric(c(out3[i,3],out3[i,4],out3[i,4],out3[i,3]))))),paste0(out3[i,6],"/",out3[i,5]))
			srPolygonsData[[i]]=data.frame(dx=abs(as.numeric(out3[i,2])-as.numeric(out3[i,1])),dy=abs(as.numeric(out3[i,4])-as.numeric(out3[i,3])))
		}

		SpP=SpatialPolygons(srPolygons)
		srPolygonsData=do.call(abind, c(srPolygonsData, list(along = 1)))

		rows=rownames(sp::coordinates(SpP))
		rownames(srPolygonsData)=rows
		srPolygonsData=as.data.frame(srPolygonsData)

		data1=as.data.frame(rownames(srPolygonsData))
		rownames(data1)=rows
		SpP_data=SpatialPolygonsDataFrame(SpP,data1)

		SpP_data=subset(SpP_data,srPolygonsData[,1]==min(srPolygonsData[,1]))

		init="+init=epsg:3832"
		proj4string(SpP_data) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
		SpP_data=spTransform(SpP_data,CRS(init))

		require(sf)
		SpP_data_1 <- st_as_sf(SpP_data)

		grid_1 <- SpP_data_1 %>% 
		  st_make_grid(cellsize = c(median(abs(sp::coordinates(SpP_data)[1:100,1]-sp::coordinates(SpP_data)[2:101,1])),
		median(abs(sp::coordinates(SpP_data)[1:100,2]-sp::coordinates(SpP_data)[2:101,2]))), what = "centers") 


		coords <- do.call(rbind, st_geometry(grid_1))
		grid=SpatialPoints(coords, proj4string = CRS(proj4string(SpP_data)))

	#get anchor labels assigned form dataframe

		tile_placement=grid%over%SpP_data

	#get number of columns
		coords1=coords
		coords1[,2]=c(coords[1,2],coords[1:(length(coords[,2])-1),2])

		dx=coords[,2]-coords1[,2]
		ncols=min((1:length(dx))[dx!=0])-1

		# generate anchor matrix
		tile_placement=as.character(tile_placement[,1])
		tile_placement=matrix(tile_placement[(length(tile_placement):1)],ncol=ncols,byrow=TRUE)



		#retrieving mask predictions
		
		listPreds=list.files(predsDir,full.names=TRUE)  
		listPreds=normalizePath(listPreds,winslash="/")


		Species=list()
		Preds=list()
		tile_id=list()
		for (f in 1:length(listPreds)) {
			Species[[f]]=strsplit(basename(listPreds[f]),split = "_")[[1]][2]
			PredsRDS=readRDS(listPreds[f])
			Preds[[f]]=PredsRDS$preds
			listImageBl=PredsRDS$listImageBl
			#listImageBl=normalizePath(listImageBl,winslash="/")
			file_name=basename(listImageBl)
			seps=gregexpr("_",file_name)
			sep_1=sapply(seps,function(X) X[1])
			sep_2=sapply(seps,function(X) X[2])
			tile_id[[f]]=paste0(substr(file_name,sep_1+1,sep_2-1),"/",substr(file_name,sep_2+1,nchar(file_name)))
		}

		Preds=do.call(abind, c(Preds, list(along = 1)))
		tile_id=do.call(abind, c(tile_id, list(along = 1)))

		#read observer points
		kml_file=paste0(labelInput, "/Observer count/",date_1,".kml")
		shape_file=paste0(labelInput, "/Observer count/",date_1,".shp")
		
		if(sum(c(file.exists(kml_file),file.exists(shape_file)))==0) stop(paste("There is no kml or shape file of Observer count:",date_1))
		
		if(file.exists(kml_file))
		{
			if(!"Point Features"%in%rgdal::ogrListLayers(kml_file)) stop(paste("There is no points in kml file:",kml_file))
			kml_points=rgdal::readOGR(kml_file,"Point Features")
			kml_points=subset(kml_points,!as.character(kml_points@data$Description)%in%exclude_labels)
			
		}else{
			kml_points=rgdal::readOGR(shape_file)
			kml_points=subset(kml_points,!as.character(kml_points@data$LAYER)%in%exclude_labels)
		}
			#set CRS to lat lon if missed
			if(is.na(proj4string(kml_points))) proj4string(kml_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
			
			kml_points=spTransform(kml_points,CRS(init))
			point_tiles=kml_points%over%SpP_data
			point_tiles=as.character(point_tiles[,1])
		
	#building up an whole image


	retrieve_mask=function(shape_file)
	{
	model_mask=NULL
		if(file.exists(shape_file))
		{

			#if(!"Point Features"%in%rgdal::ogrListLayers(shape_file)) stop(paste("There is no points in kml file:",kml_file))
			shape_1=rgdal::readOGR(shape_file)
			if(is.na(proj4string(shape_1))) proj4string(shape_1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
			
			shape_1=spTransform(shape_1,CRS(init))
			model_mask=foreach(model_id = 1:length(shape_1)) %do%
			{
			shape_2=shape_1[model_id,]
			
			#shape_2 <- st_as_sf(shape_2)
			#
			grid_1 <- makegrid(shape_2,cellsize = c(median(abs(sp::coordinates(SpP_data)[1:100,1]-sp::coordinates(SpP_data)[2:101,1]))/resolution,
			median(abs(sp::coordinates(SpP_data)[1:100,2]-sp::coordinates(SpP_data)[2:101,2]))/resolution))	
			spP=SpatialPoints(grid_1)
			proj4string(spP)=proj4string(shape_1)
			points_within=spP%over%shape_2
			out_points=coordinates(spP[!is.na(points_within[,1]),])
			out_points
			}
			model_mask=do.call(abind, c(model_mask, list(along = 1)))
			model_mask=SpatialPoints(model_mask)
			proj4string(model_mask)=proj4string(shape_1)
			dat1=data.frame(id=1:length(model_mask))
			rownames(dat1)=rownames(coordinates(model_mask))
			model_mask=SpatialPointsDataFrame(model_mask,dat1)
			model_mask
		}
	}

	read_shape=function(shape_file)
	{
	shape_1=NULL
		if(file.exists(shape_file))
		{

			#if(!"Point Features"%in%rgdal::ogrListLayers(shape_file)) stop(paste("There is no points in kml file:",kml_file))
			shape_1=rgdal::readOGR(shape_file)
			if(is.na(proj4string(shape_1))) proj4string(shape_1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
			
			shape_1=spTransform(shape_1,CRS(init))
		}
		shape_1
	}

		shape_file=paste0(labelInput, "/Polygons/Model/Model.shp")
		model_mask=read_shape(shape_file=shape_file)
		
		model_area=model_mask@polygons[[1]]@area
		
		shape_file=paste0(labelInput, "/Polygons/Houlout/Houlout.shp") #note Haulout not Houlout
		haulout_mask=read_shape(shape_file=shape_file)
		
		haulout_area=haulout_mask@polygons[[1]]@area
		
		if((model_area/haulout_area)<0.5)
		{
		shape_file=paste0(labelInput, "/Polygons/Model/Model.shp")
		model_mask=retrieve_mask(shape_file=shape_file)
		}else{
			model_mask=NULL
			warning("model site too lagre and not generated but error evaluation works")
		}
		
		
		if(!is.null(model_mask))
		{
			shape_file=paste0(labelInput, "/Polygons/Exlude/Exlude.shp") #note Exclude not Exlude
			exclude_mask=read_shape(shape_file=shape_file)
			exclude=model_mask%over%exclude_mask
			model_mask=model_mask[is.na(exclude[,1]),]
			
			include=model_mask%over%haulout_mask
			model_mask=model_mask[!is.na(include[,1]),]
			
			
			tiles_with_model_masks=model_mask%over%SpP_data
			tiles_with_model_masks=as.character(unique(tiles_with_model_masks[,1]))
		}
		
		
	#	n_cells=length(tile_placement)
	#	n_processed=0
		message(paste0("Start Image processing"),appendLF=TRUE)
		for(k in 1:dim(tile_placement)[1] ) 
		{
			for(l in 1:dim(tile_placement)[2] ) 
			{
				points_matrix=matrix(0,nrow=resolution,ncol=resolution)
				points_matrix_id=matrix(0,nrow=resolution,ncol=resolution)
				mask=matrix(0,ncol=resolution,nrow=resolution)
				model_matrix=matrix(0,nrow=resolution,ncol=resolution)
						
				
				if(is.na(tile_placement[k,l]))
				{
					img=matrix(0,ncol=resolution,nrow=resolution)
				}else{
					if(generate_combined_map)
					{
						file_tmp=paste0(labelInput,"/",date_1,"/",tile_placement[k,l])
						if(!file.exists(file_tmp))
						{
						
							extension=tools::file_ext(file_tmp)
							if(extension%in%c("png")) extension="jpg" else extension="png"
						
							file_tmp=paste0(tools::file_path_sans_ext(file_tmp),".",extension)
							if(!file.exists(file_tmp)) stop(paste0("There is no either jpg or png of ",file_tmp))
						
						}
						
						img=EBImage::readImage(file_tmp)
						if(length(dim(img))>2)
						{
							img=aperm(img,c(1,2,3))
							img=img[,,1]
						}
						img=EBImage::resize(img,resolution,resolution)
					}
					
					prediction_id=(1:length(tile_id))[tools::file_path_sans_ext(tile_id)%in%tools::file_path_sans_ext(tile_placement[k,l])]
					
					if(length(prediction_id)!=0)
					{
						mask=Preds[prediction_id, , , ]
						i_start=((length(mask[1,])*.5)/2)+1
						i_end=i_start+(length(mask[1,])*.5)-1
						mask=mask[i_start:i_end,i_start:i_end]
						mask=EBImage::resize(mask,resolution,resolution)
						
						#mask = EBImage::thresh(mask, 18, 18, 0.009)  
						#mask = EBImage::fillHull(mask)
						#mask = EBImage::opening(mask, EBImage::makeBrush(7,shape='disc') ) # shape='Gaussian', sigma=50
						#mask = EBImage::fillHull(mask)
						
						
						#mask[mask>0.999]=1
						#mask[mask!=1]=0
						
						#if(generate_combined_map) img[mask==1]=1
					}
					
					points_id=(1:length(point_tiles))[tools::file_path_sans_ext(point_tiles)%in%tools::file_path_sans_ext(tile_placement[k,l])]
					
					
					if(length(points_id)!=0)
					{
						#get_anchor_polygon
						tmp_pl=subset(SpP_data,rownames(SpP_data@data)==tile_placement[k,l])
						dx=abs(tmp_pl@bbox[1,1]-tmp_pl@bbox[1,2])
						dy=abs(tmp_pl@bbox[2,1]-tmp_pl@bbox[2,2])
							
						tmp_pp=subset(kml_points,point_tiles%in%point_tiles[points_id])
						xy=coordinates(tmp_pp)
						x=xy[,1]-tmp_pl@bbox[1,1]
						y=xy[,2]-tmp_pl@bbox[2,1]
						pixel_x=round((resolution*x)/dx)
						pixel_y=round((resolution*y)/dy)
						
						points_matrix=matrix(0,nrow=resolution,ncol=resolution)
						points_matrix_id=matrix(0,nrow=resolution,ncol=resolution)
						
						for(m in 1:length(pixel_x))
						{
							points_matrix_id[pixel_x[m],pixel_y[m]]=points_id[m]
							points_matrix[pixel_x[m],ifelse(pixel_y[m]-3<0,0,pixel_y[m]-3):ifelse(pixel_y[m]+3>resolution,resolution,pixel_y[m]+3)]=1
							points_matrix[ifelse(pixel_x[m]-3<0,0,pixel_x[m]-3):ifelse(pixel_x[m]+3>resolution,resolution,pixel_x[m]+3),pixel_y[m]]=1
						}
						
						points_matrix=points_matrix[,length(points_matrix[,2]):1]
						points_matrix_id=points_matrix_id[,length(points_matrix_id[,2]):1]
						
						#if(generate_combined_map) img[points_matrix==1]=0
					}
					
					#generating model area
					
					convert_polygon_to_pixels=function(model_mask)
						{
							tmp_pl=subset(SpP_data,rownames(SpP_data@data)==tile_placement[k,l])
							dx=abs(tmp_pl@bbox[1,1]-tmp_pl@bbox[1,2])
							dy=abs(tmp_pl@bbox[2,1]-tmp_pl@bbox[2,2])

							selected_mask=model_mask%over%tmp_pl
							tmp_model_mask=model_mask[!is.na(selected_mask[,1]),]
							
							xy=coordinates(tmp_model_mask)
							x=xy[,1]-tmp_pl@bbox[1,1]
							y=xy[,2]-tmp_pl@bbox[2,1]
							pixel_x=round((resolution*x)/dx)
							pixel_y=round((resolution*y)/dy)
							
							for(m in 1:length(pixel_x))
							{
								model_matrix[pixel_x[m],pixel_y[m]]=1
							}
						
							model_matrix=model_matrix[,length(model_matrix[,2]):1]
						}
					
					if(!is.null(model_mask))
					{
						if(tile_placement[k,l]%in%tiles_with_model_masks)
						{
							#get_anchor_polygon
							model_matrix=convert_polygon_to_pixels(model_mask)
						}
					}
					
				}
				
				if(generate_combined_map) if(l==1) image_stack=img else image_stack=rbind(img,image_stack)
				if(l==1) points_stack=points_matrix_id else points_stack=rbind(points_matrix_id,points_stack)
				if(l==1) predictoin_stack=mask else predictoin_stack=rbind(mask,predictoin_stack)
				if(l==1) model_stack=model_matrix else model_stack=rbind(model_matrix,model_stack)
				
				
				rm(list=c("img","points_matrix_id","mask","model_matrix"))
		    #n_processed=n_processed+1
			#message(paste0("Image compiled: ", round((n_processed/n_cells)*100,2),"%"),appendLF=TRUE)
			flush.console()
			}
			
			if(generate_combined_map) if(k==1) image_stack_full=image_stack else image_stack_full=cbind(image_stack_full,image_stack)
			if(k==1) points_stack_full=points_stack else points_stack_full=cbind(points_stack_full,points_stack)
			if(k==1) predictoin_stack_full=predictoin_stack else predictoin_stack_full=cbind(predictoin_stack_full,predictoin_stack)
			if(k==1) model_stack_full=model_stack else model_stack_full=cbind(model_stack_full,model_stack)
			
			rm(list=c("points_stack","predictoin_stack","image_stack"))
		}
		message(paste0("Image processing Done"),appendLF=TRUE)

		if(generate_combined_map)
		{
			gc()
			v.i=magick::image_read(EBImage::Image(image_stack_full))
			gc()
#			magick::image_write(v.i,paste0(labelInput,"/",date_1,".jpg"))
		}
		

		savelist=c("points_stack_full","predictoin_stack_full","model_stack_full","tile_placement")
		if(generate_combined_map) savelist=c(savelist,"image_stack_full")
		
		save(list=savelist,file=paste0(labelInput,"/",date_1,".rda"))
	}else{
		load(paste0(labelInput,"/Predict/",date_1,".rda"))
	}
	################################################
	################################################
	################################################

	#Code below Not yet completed


	#Evaluate errors
		predictoin_stack_full = EBImage::thresh(predictoin_stack_full, 18, 18, 0.009)  
		predictoin_stack_full = EBImage::fillHull(predictoin_stack_full)
		predictoin_stack_full = EBImage::opening(predictoin_stack_full, EBImage::makeBrush(7,shape='disc') ) # shape='Gaussian', sigma=50
		predictoin_stack_full = EBImage::fillHull(predictoin_stack_full)
		
		if(generate_combined_map)
		{
			image_stack_full_dummy=image_stack_full
			image_stack_full_dummy[predictoin_stack_full==1]=0
			EBImage::writeImage(EBImage::Image(image_stack_full_dummy), files=paste0(labelInput,"/Predict/prediction_overlap_",date_1,".jpg"))
		}	

	#if ther is no masked model sites set mask to 1	
	if(sum(model_stack_full)==0) 
	{
		model_stack_full[model_stack_full==0]=1
		compare_entire_site=TRUE
	}else{
		compare_entire_site=FALSE
	}
	#false_negative. e.g. there is observer point but no predictions

	#Get number of points by observer
		points_stack_full_dummy=points_stack_full
		points_stack_full_dummy[points_stack_full!=0]=1
		points_stack_full_dummy=points_stack_full_dummy*model_stack_full
		total_count=sum(points_stack_full_dummy)
		
	total_count
	rm(points_stack_full_dummy)
	gc()
	#

	#increase point size by 5 px
	   
	   poin_size=5
	   
	   points_stack_full_cross_vector=as.vector(points_stack_full)
	   points_stack_full_cross_vector=(1:length(points_stack_full_cross_vector))[points_stack_full_cross_vector!=0]
	   
	   points_stack_full_labels=unique(as.vector(points_stack_full))
	   points_stack_full_labels=points_stack_full_labels[points_stack_full_labels!=0]
	   points_stack_full_labels=sort(points_stack_full_labels)
	   
	   nrows=length(points_stack_full[,1])
	   ncols=length(points_stack_full[1,])
	   
	   get_row_id=function(X,nrows) if(X<=nrows) X else X-(floor(X/nrows)*nrows)
	   
	   get_column_id=function(X,nrows,row_id) if(X<=nrows) 1 else ((X-row_id)/nrows) +1
	   
	   points_stack_full_cross=points_stack_full
	   
	   for(point_id in 1:length(points_stack_full_cross_vector))
	   {
		   row_id=get_row_id(points_stack_full_cross_vector[point_id],nrows=nrows)
		   column_id=get_column_id(points_stack_full_cross_vector[point_id],nrows=nrows,row_id)
		   
		   point_val=points_stack_full[row_id,column_id]
		   points_stack_full_cross[row_id,ifelse(column_id-poin_size>0,column_id-poin_size,0):ifelse(column_id+poin_size<=ncols,column_id+poin_size,ncols)]=point_val
		   points_stack_full_cross[ifelse(row_id-poin_size>0,row_id-poin_size,0):ifelse(row_id+poin_size<=nrows,row_id+poin_size,nrows),column_id]=point_val
	   }
	rm(points_stack_full_cross_vector)
	rm(points_stack_full)

	##################################
	##################################
	##################################


		
	#Exclude blobs by treshhold
	gc()
		#create mask labels and remove blobs with less than 50 fixels
		mask_labels=EBImage::bwlabel(predictoin_stack_full)
		
		trashlevel=30
			points_labels_dummy=points_stack_full_cross
			points_labels_dummy[points_labels_dummy!=0]=1
			#mask blobs overlap with points
			mask_labels_overlap=unique(as.vector(points_labels_dummy*mask_labels))
			rm(points_labels_dummy)
			
			#mask_labels_dummy=mask_labels
			#mask_labels_dummy[!mask_labels_dummy%in%mask_labels_overlap]=0
			
			mask_labels_weights=tapply(as.vector(mask_labels),as.vector(mask_labels),length)
			mask_labels_weights=data.frame(label_id=names(mask_labels_weights),weight=mask_labels_weights,stringsAsFactors=FALSE)
			mask_labels_weights=mask_labels_weights[mask_labels_weights$label_id!=0,]
			
			trashlevel=min(mask_labels_weights$weight[mask_labels_weights$label_id%in%mask_labels_overlap])*0.8
			
			#treshhold should be the same among all days use 50 pixels size
			trashlevel=50
		
		mask_labels[mask_labels%in%mask_labels_weights$label_id[mask_labels_weights$weight<trashlevel]]=0
		

		mask_labels_list=unique(as.vector(mask_labels))
		mask_labels_list=mask_labels_list[mask_labels_list!=0]

	#exclude blobs out of model site

	if(!compare_entire_site)	
	{
		model_stack_full_invert=model_stack_full
		model_stack_full_invert[model_stack_full==0]=1
		model_stack_full_invert[model_stack_full==1]=0
		
		mask_labels_wihin=mask_labels*model_stack_full
		mask_labels_wihin_list=unique(as.vector(mask_labels_wihin))
		mask_labels_wihin_list=mask_labels_wihin_list[mask_labels_wihin_list!=0]
		
		mask_labels_outside=mask_labels*model_stack_full_invert
	gc()	
		crt_same=as.vector(mask_labels_wihin)%in%as.vector(mask_labels_outside)
		blobs_within_and_outside=unique(as.vector(mask_labels_wihin)[crt_same])
		blobs_within_and_outside=blobs_within_and_outside[blobs_within_and_outside!=0]
	#find were is larger part of the blobs
		mask_labels_wihin_selected=as.vector(mask_labels_wihin[mask_labels_wihin%in%blobs_within_and_outside])
		mask_labels_outside_selected=as.vector(mask_labels_outside[mask_labels_outside%in%blobs_within_and_outside])
		
		weight_within=tapply(mask_labels_wihin_selected,mask_labels_wihin_selected,length)
		weight_within=data.frame(label_id=names(weight_within),weight=weight_within,stringsAsFactors=FALSE)
		
		
		weight_outside=tapply(mask_labels_outside_selected,mask_labels_outside_selected,length)
		weight_outside=data.frame(label_id=names(weight_outside),weight=weight_outside,stringsAsFactors=FALSE)
		
		assign_position=function(X,weight_within=weight_within,weight_outside=weight_outside)
			{
			ifelse(weight_within[X,2]>weight_outside[weight_outside[,1]==weight_within[X,1],2],"in","out")
			}
		
		assignment=tapply(1:length(weight_within[,1]),1:length(weight_within[,1]),function(X) assign_position(X,weight_within,weight_outside))
		
		weight_within$assignment=assignment
		mask_labels_wihin_list=unique(as.vector(mask_labels_wihin_list[!mask_labels_wihin_list%in%weight_within$label_id[weight_within$assignment=="out"]]))

		verification_weight=length(mask_labels_wihin_list)/length(mask_labels_list)
		
	#exclude blobs that lay mainly outside of model site
		mask_labels_wihin[mask_labels_wihin%in%weight_within$label_id[weight_within$assignment=="out"]]=0
	}else{
		verification_weight=1
		mask_labels_wihin=mask_labels
	}

	#that is thrustwothy of verification. 1 is extreemly good (so all data used for verification) 0 is bad no data for verification
	verification_weight
		
	#get false neagtive. 
	   
	 


	use_exact_point_location=FALSE # do not use exact point locations 

	if(use_exact_point_location)
	{
		#This portion of code is only work when all observer points is inside of blob. 
		#However it may produce a overestimate of false_neagtive thus use use_exact_point_location=FALSE
		
		#remove all points that withtin predictions
		predictoin_stack_full_dummy=predictoin_stack_full
		predictoin_stack_full_dummy[predictoin_stack_full==0]=1
		predictoin_stack_full_dummy[predictoin_stack_full==1]=0
		summary_matrix=points_stack_full_dummy*predictoin_stack_full_dummy
		summary_matrix=summary_matrix*model_stack_full
		
		#sum of all remining points
		false_neagtive=sum(summary_matrix)
		
	}else{
		#Use extended point matrix
		
		predictoin_stack_full_dummy=predictoin_stack_full
		summary_matrix=points_stack_full_cross*predictoin_stack_full_dummy
		overlaped_points=unique(as.vector(summary_matrix))
		overlaped_points=overlaped_points[overlaped_points!=0]
		false_neagtive=sum(!points_stack_full_labels%in%overlaped_points)
		
	}
		
	false_neagtive

		
	#get false positive
		gc()
		#create mask labels
		points_stack_full_cross_dummy=points_stack_full_cross
		points_stack_full_cross_dummy[points_stack_full_cross_dummy!=0]=1
		
		mask_labels_wihin_list=unique(as.vector(mask_labels_wihin))

		mask_labels_wihin_list=mask_labels_wihin_list[mask_labels_wihin_list!=0]

		#get intersection of point labels with the masks

		predictoin_stack_full_dummy=mask_labels_wihin

		predictoin_stack_full_dummy=predictoin_stack_full_dummy*points_stack_full_cross_dummy

		labels_same=unique(as.vector(predictoin_stack_full_dummy))
		labels_same=labels_same[labels_same!=0]

		false_positive=sum(!mask_labels_wihin_list%in%labels_same)

	false_positive	

	#get counts affected by fused blobs

	#number of animals fused into one blob
		gc()
		predictoin_stack_full_dummy=mask_labels_wihin
		predictoin_stack_full_dummy[mask_labels_wihin!=0]=1
		predictoin_stack_full_dummy=predictoin_stack_full_dummy*points_stack_full_cross
		
		count_animals_within_blob=function(X)
		{
			X=unique(X)
			if(length(X[X!=0])==0)
				{
					n_animals=0
				}else{
					n_animals = length(X[X!=0])
				}
		n_animals
		}
		
		mask_vector=as.vector(mask_labels_wihin)
		points_vector=as.vector(predictoin_stack_full_dummy)
		points_vector=points_vector[mask_vector!=0]
		mask_vector=mask_vector[mask_vector!=0]
		
		
		animals_per_blob=tapply(points_vector,mask_vector,function(X) count_animals_within_blob(X) )
		
		merged_animals=animals_per_blob[animals_per_blob>1]
		merged_blob_occurances=length(merged_animals)

	merged_blob_occurances

		total_merged_animals=sum(merged_animals)

	total_merged_animals
		
	#total error

	#Trustworthy index

	message(paste("Verification weight: ",round((verification_weight)*100,2),"%"))

	#false negative

	message(paste("False negative: ",round((false_neagtive/total_count)*100,2),"%"))

	#false positive

	message(paste("False positive: ",round((false_positive/length(mask_labels_wihin_list))*100,2),"%"))

	#merged blob error

	message(paste("Merged animals: ",round((total_merged_animals/length(mask_labels_list))*100,2),"%"))

	report=data.frame(date=date_1,
				verification_weight=verification_weight,
				total_count_model=total_count,
				false_neagtive=false_neagtive,
				false_neagtive_prc=round((false_neagtive/total_count)*100,2),
				n_predicted_model=length(mask_labels_wihin_list),
				false_positive=false_positive,
				false_positive_prc=round((false_positive/length(mask_labels_wihin_list))*100,2),
				merged_animals=total_merged_animals,
				merged_animals_prc=round((total_merged_animals/length(mask_labels_wihin_list))*100,2)
				)
				

	write.csv(report,paste0(labelInput,"/Predict/report_",date_1,".csv"))

#	report_list[[list_id]]=report
		rm(
"image_stack_full",
"image_stack_full_dummy",
"mask_labels",
"mask_labels_outside",
"mask_labels_wihin",
"model_stack_full",
"model_stack_full_invert",
"points_stack_full_cross",
"points_stack_full_cross_dummy",
"predictoin_stack_full",
"predictoin_stack_full_dummy",
"summary_matrix",
"crt_same",
"Preds",
"coords",
"coords1",
"include"
)
gc()
	} else print(paste0("Data insufficient  for prediction evaluation  ", labelInput)) 
	

}

PredictionEvaluation(labelInput)