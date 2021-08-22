VGG_garem=function () {
  # VGG_garem_limit=VGG_garem_limit
  if(exists("VGG16model_garem")==F) {
    VGG16model_presence <<- load_model_hdf5("data/MODEL/VGG_NFS_Garem_20190103.h5")
    showNotification("Model VGG16 has loaded")}
  dirImgAnimalPresence=paste0(labelInput,"\\Predict\\Animals presence")
  dirGarem=paste0(labelInput,"\\Predict\\Garem")
  if (dir.exists(dirGarem)==F) {dir.create(dirGarem)}
  tableP=paste0(labelInput, "\\Predict\\",basename(labelInput),"_NFS_presence.csv")
  tablePresence<<-read.csv(tableP)
  Pth_tableGarem<<-paste0(labelInput, "\\Predict\\",basename(labelInput),"_NFS_garem.csv")
  if (file.exists(Pth_tableGarem)==F) {
    
    list.img<<-  list.files(dirImgAnimalPresence, full.names = T)                
    name.img<<-   basename(list.img) 
    withProgress(message = 'NFS Garem predict', value = 0, {
      
      for (y in 1: length(name.img)) {                                
        
        path=list.img[y]
        name=name.img[y]
        img <- image_load(path, target_size = c(224, 224))
        img_tensor <- image_to_array(img)
        img_tensor <- array_reshape(img_tensor, c(1, 224, 224, 3))
        img_tensor <- img_tensor / 255
        preds = c(VGG16model_presence %>% predict(img_tensor))
        ADD=data.frame(Garem=preds,name)
        if (y==1) {
          resultG=ADD } else {
            resultG=rbind(resultG,ADD)
          }
        incProgress(1/length(name.img), detail = paste("Doing part", y))
        Sys.sleep(0.1)
        resultG<<-resultG
      }
    })
    #
    tableGarem=merge(data.frame(tablePresence), data.frame(resultG), by.x='link', by.y='name',all.x = T)
    write.csv(tableGarem, Pth_tableGarem,row.names = F)
  } else { tableGarem=read.csv(Pth_tableGarem)} 
  tableGarem=tableGarem[is.na(tableGarem$Garem)==F,]
  limitGarem=VGG_garem_limit
  sortPresenceHarem=tableGarem[tableGarem$Garem <= limitGarem,]
  from=paste0(dirImgAnimalPresence, "\\",sortPresenceHarem$link)
  move_files(from,dirGarem)
}
VGG_garem()