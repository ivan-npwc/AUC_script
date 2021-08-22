
listUniq <- list(
                  listOPP=listOPP,   
                  pthOPP=pthOPP,
                  labelInput = labelInput,
                  NFS_Adult_weight_pth= NFS_Adult_weight_pth,
                  NFS_Pup_weight_pth=NFS_Pup_weight_pth,
                  SSL_Adult_weight_pth=SSL_Adult_weight_pth,
                  SSL_Pup_weight_pth=SSL_Pup_weight_pth,
				  SSL_Age_Weight_Rookery=SSL_Age_Weight_Rookery,
				  Weight=Weight,
				  
				  WLRS_weight_pth=WLRS_weight_pth,
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
					   Deformation=Deformation


                 


				  
               )
			    saveRDS(listUniq, "System data/listUniq")
				
				
		