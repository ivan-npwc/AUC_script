library(magrittr)
library(XML)
library(foreach)
library(abind)
library(sp)
library(sf)

create_tile_polygons=function(labelInput,crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  #"+init=epsg:3832"
{
	date_1=basename(labelInput)
	extract_kml=function(date_1,file_name="doc.kml",data_set="root")
	{
		kml_file=paste0(labelInput,"/", date_1 ,"/", file_name)
		if (file.exists(kml_file)==F){stop("No unziped OPP found")}
		data <- xmlParse(kml_file)
		xml_data <- xmlToList(data)


		xml_data_1=xml_data[[1]][[1]]

		level1=lapply(xml_data_1,length)
		folder_1=grep("Folder",names(level1))
		if(length(folder_1)==0){stop("OPP is too small, export the OPP by increasing the resolution")} else
		{
			unlevel_xml=function(xml_data,folder_id,data_set=data_set)
			{
				dat=foreach(m = 1:length(folder_id)) %do%
				{
				k_id=folder_id[m]
				xml_data_2=xml_data[[k_id]]
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
								dat1=data.frame(west=coord[[1]],east=coord[[2]],south=coord[[3]],north=coord[[2]],href=href,stringsAsFactors=FALSE)		
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
			out=unlevel_xml(xml_data=xml_data_1,folder_id=folder_1,data_set=data_set)
		}
	}

	out1=extract_kml(date_1=date_1)
	
	unlevel_list=function(out1)
	{
	if("list"%in%is(out1))
		{
			for(k in 1:length(out1)) if(!exists("out2")) out2=unlevel_list(out1[[k]]) else out2=rbind(out2,unlevel_list(out1[[k]]))
		}else{
		out2=out1
		}
	out2
	}
	out1=unlevel_list(out1)
	
	#data.frame(matrix(unlist(out1), ncol=5, byrow=TRUE),stringsAsFactors=FALSE)
	
	#out1=sapply(out1,function(X) do.call(abind, c(X, list(along = 1))))
	#out1=sapply(out1,function(X) do.call(abind, c(X, list(along = 1))))
	#out1=do.call(abind, c(out1, list(along = 1)))
	out1=data.frame(out1,stringsAsFactors=FALSE)


#extract last kml folder level for each subfolder

	out3=foreach(k = 1:length(out1[,1])) %do%
		{
				out2=extract_kml(date_1=date_1,
                 file_name=out1[k,"href"],
				 data_set="level_2")
			if(!is.null(out2))
			{			
				#out2=unlevel_list(out2)	 
				df <- data.frame(matrix(unlist(out2),ncol=5,byrow=TRUE),stringsAsFactors=FALSE)
				df$path=dirname(out1[k,"href"])
				df
			}else{
			df <- data.frame(matrix(c(NA,NA,NA,NA,NA,NA),ncol=6,byrow=TRUE),stringsAsFactors=FALSE)
			}
		}


	out3=do.call(abind, c(out3, list(along = 1)))
	out3=out3[!is.na(out3[,1]),]
	#out3=out3[abs(as.numeric(out3[,1])-as.numeric(out3[,2]))==abs(median(as.numeric(out3[,1])-as.numeric(out3[,2]))),]
	
	
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

	SpP_data=subset(SpP_data,srPolygonsData[,1]<=quantile(srPolygonsData[,1],0.975))
	
	proj4string(SpP_data) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
	SpP_data=spTransform(SpP_data,CRS(crs))
	SpP_data
	}
