
listUniq <- list(
                  listOPP=listOPP,   
                  pthOPP=pthOPP,
                  labelInput = labelInput,
                  NFS_Adult_weight_pth= NFS_Adult_weight_pth,
                  NFS_Pup_weight_pth=NFS_Pup_weight_pth,
                  SSL_Adult_weight_pth=SSL_Adult_weight_pth,
                  SSL_Pup_weight_pth=SSL_Pup_weight_pth,
				  SSL_Age_Weight_Rookery=SSL_Age_Weight_Rookery,
				  LRG_pth=LRG_pth,
				  Weight=Weight,
				  
				  WLRS_Rocky_weight_pth=WLRS_Rocky_weight_pth,
				  WLRS_Sand_weight_pth=WLRS_Sand_weight_pth,
                  SQLite_path=SQLite_path,
                  KK_Effort=KK_Effort, 
                 # key=key,
                #  NanmlsOnTiles=NanmlsOnTiles,
				  
                       trainDir=trainDir,
                       weight1=Weight,
                       Model_base=Model_base,
					   BatchIntens=BatchIntens, 
					   Split=Split,        
					   Smooth=Smooth,        
					   batch_size=batch_size,
					   epochs=epochs,
					   Deformation=Deformation,
					   DirModelsCheck=DirModelsCheck,
					   System_data=System_data,
					   site=site,
					   LRGH_MSRMNTS=LRGH_MSRMNTS,
					 DarkTheme=DarkTheme,
					 type=type


                 


				  
               )
			    saveRDS(listUniq, "listUniq")
				
				
		