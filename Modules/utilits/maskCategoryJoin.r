library(EBImage)

dirTottal="D:\\SSL_AUTO_COUNT\\SSL_Unet_blob\\SSL mask_Anna_20190510\\Mask"
dirSave= "D:\\SSL_AUTO_COUNT\\SSL_Unet_blob\\SSL mask_Anna_20190510\\Mask_con"

listFoder=list.files(dirTottal,full.names=T)


for (i in 1: length(listFoder)) {

folder= paste0(listFoder[i])
list.category = list.files(folder,full.names=T)
 
 one=readImage(list.category[1])
 An=readImage(list.category[2])
 F=readImage(list.category[3])
 J=readImage(list.category[4])
 SA=readImage(list.category[5])
 P=readImage(list.category[6])
 TF=readImage(list.category[7])
 TN=readImage(list.category[8])
 
 ALL=one+An+F+J+SA+P+TF+TN
 pthSave=paste0(dirSave,"\\",basename(folder),".png")
 writeImage(ALL,pthSave)
 
 }
 ###################################################################
dirColorImg= "D:\\SSL_AUTO_COUNT\\SSL_Unet_blob\\SSL mask_Anna_20190510\\Mask_con"
dirSave="D:\\SSL_AUTO_COUNT\\SSL_Unet_blob\\SSL mask_Anna_20190510\\Mask_con_gray"
list.img=list.files(dirColorImg,full.names=T)
for (i in 1 : length(list.img)) {
img=readImage(list.img[i])
 colorMode(img)=Grayscale
 img[img !=0]=1
 img=img[,,1]+img[,,2]+img[,,3]+img[,,4]
pthSave= paste0(dirSave,"\\",basename(list.img[i]))
 writeImage(img,pthSave)
}
 