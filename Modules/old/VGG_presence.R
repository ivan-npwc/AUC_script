VGG_presence=function() {
  # VGG_presence_limit=VGG_presence_limit
  if(exists("VGG16model_presence")==F) {
    VGG16model_presence <<- load_model_hdf5("data/MODEL/VGG_NFS_Pres-abs_20190103_02.h5")
    showNotification("Model VGG16 has loaded")}
  
  GeneralPath=paste0(labelInput, "\\", "Predict", "\\","Input") 
  date1<<- basename(labelInput)
  tableP<<-paste0(labelInput,"\\", date1,"_table.csv")
  table0<<-read.csv(tableP)
  table0$link=paste0(date1, "_",table0$link)
  VGA=paste0(GeneralPath)
  listImgPred=list.files(VGA)
  list.img<<-paste0(VGA,"\\",listImgPred)
  name.img=listImgPred
  
  tableNewP=paste0(labelInput, "\\Predict\\",basename(labelInput),"_NFS_presence.csv")
  if (file.exists(tableNewP)==F) {
    
    withProgress(message = 'NFS Presence predict', value = 0, {
      for (u in 1: length(list.img)) {
        path=list.img[u]
        name=name.img[u]
        img <- image_load(path, target_size = c(224, 224))
        img_tensor <- image_to_array(img)
        img_tensor <- array_reshape(img_tensor, c(1, 224, 224, 3))
        img_tensor <- img_tensor / 255
        preds = c(VGG16model_presence %>% predict(img_tensor))
        ADD=data.frame(presence=preds,name)
        if (u==1) {
          result=ADD } else {
            result<-rbind(result,ADD)
          }
        incProgress(1/length(list.img), detail = paste("Doing part", u))
        Sys.sleep(0.1)
        result<<-result
      }
    })
    table1=merge(data.frame(table0), data.frame(result), by.x='link', by.y='name')
    table2<<-data.frame(west=table1$west,east=table1$east,south=table1$south,north=table1$north,link=table1$link,presence=table1$presence)
    write.csv(table2, tableNewP,row.names = F)
  } 
  table2<<-read.csv(tableNewP)
  limitPresence<<-VGG_presence_limit
  sort1<<-table2[table2$presence < limitPresence,] # we filter image which contents animals in central part, 10 is percents probability of absents
  to=paste0(labelInput,"\\Predict\\Animals presence")
  if(dir.exists(to)==F)  {dir.create(to)}
  from=paste0(paste0(labelInput),"\\Predict\\Input\\",sort1$link)
  move_files(from,to) 
  table2$limitPresence=limitPresence
  write.csv(table2, tableNewP,row.names = F) 
}
VGG_presence()