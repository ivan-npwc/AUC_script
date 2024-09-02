library(EBImage)

imgs=NULL
dir1= "E:\\2021_19_OPP\\20210728_105310_DKV1996\\Predict\\PUP"

listTiles=list.files(dir1,full.names=T)

 for (i in 1:length(listTiles)) {
img= data.frame(img=strsplit(basename(listTiles[i]),"#")[[1]][2],
               tile=listTiles[i],
			   order=strsplit(basename(listTiles[i]),"#")[[1]][1])
imgs=rbind(img,imgs)
}

listImgs=unique(imgs$img)

for (i in 1:length(listImgs)) {
  img=listImgs[i]
  Tbtiles=imgs[imgs$img==img,]
  
  ##############################crop1   # get from blob analis how to convert predict into EBImage 
  pth1=Tbtiles$tile[Tbtiles$order==1]
  tile1=readImage(pth1)
  Crop1=tile1[0:384,1:512,1:3]
  ################################################crop2
  pth2=Tbtiles$tile[Tbtiles$order==2]
  tile2=readImage(pth2)
  Crop2=tile2[128:512,0:512,1:3]
  ########
Imgrow1=abind(Crop1,Crop2,along=1)
#############################################cpop 3
  pth3=Tbtiles$tile[Tbtiles$order==3]
  tile3=readImage(pth3)
  Crop3=tile3[0:384,0:512,1:3]
###########################################crop 4
 pth4=Tbtiles$tile[Tbtiles$order==4]
  tile4=readImage(pth4)
  Crop4=tile4[128:512,0:512,1:3]
Imgrow2=abind(Crop3,Crop4,along=1)
#####################
ImgBig=abind(Imgrow1[0:769,1:384,1:3],Imgrow2[0:769,128:512,1:3],along=2)
ImgBig1=image_read(ImgBig)

 left.blank=image_blank(128,896,color = "black")
 a=image_append(c(left.blank,ImgBig1))
 
 up.blank=image_blank(1024,128,color = "black")
 
 b=image_append(c(up.blank,a),stack = T)

 
 





}