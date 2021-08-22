
ImgsRenameTolabel=function (train_Gen_dir) {
    Small_train_dir= paste0(train_Gen_dir,"\\Small\\Train")
    Small_val_dir= paste0(train_Gen_dir,"\\Small\\Validate")
    Big_train_dir = paste0(train_Gen_dir,"\\Big\\Train")
    Big_val_dir= paste0(train_Gen_dir,"\\Big\\Validate")
  	listFolder=c(Small_train_dir,Small_val_dir,Big_train_dir,Big_val_dir)
      for (i in 1:length(listFolder)) {
           GenFolder=  listFolder[i]
           listAgeFold=list.files(GenFolder)
               for (u in 1:length(listAgeFold)) {
                 age= listAgeFold[u]
                 AgeDir= paste0(GenFolder,"\\",age)
                 listImgsFrom=list.files(AgeDir,full.names=T)
                 ListImgsTo=paste0(age,"_",basename(listImgsFrom))
                 ListImgsTo1=paste0(GenFolder,"\\",age,"\\",ListImgsTo)
   file.rename(listImgsFrom,ListImgsTo1)
}
}
}