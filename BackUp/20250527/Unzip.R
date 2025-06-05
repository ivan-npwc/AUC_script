
 date_time=substr(basename(labelInput),1,15)
 
 from=  paste0(labelInput,"\\",date_time,".kmz")
if (file.exists(from)==T) {  #stop("Please export OPP first")}
 
  to=  paste0(labelInput,"\\",date_time);if(dir.exists(to)==T) {unlink(to,recursive=T);dir.create(to)}
  unzip(from,exdir=to)
  
 # PredictDir=paste0(labelInput,"\\Predict")
 # if (Species !="NFSPup512"){unlink(PredictDir,recursive=T)}
	
 Tpth=paste0(labelInput,"\\", date_time, "_table.csv"); if (file.exists(Tpth)==T){unlink(Tpth)}
  showNotification("Done")
}


