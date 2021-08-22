###################################### LEVEL 1 (1 chart)
level1=matrix(data=c(0,1,2,3), nrow=2, ncol=2,byrow=T)
level1=rbind(level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1)
level1=cbind(level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1,level1)

level1=cbind(level1,level1,level1,level1,level1,level1,level1,level1)
level1=rbind(level1,level1,level1,level1,level1,level1,level1,level1)
############################### LEVEL 2 (2 charts)
zero=matrix(data=0, nrow=2, ncol=2)
one=matrix(data=1, nrow=2, ncol=2)
two=matrix(data=2, nrow=2, ncol=2)
thre=matrix(data=3, nrow=2, ncol=2)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level2 =rbind(zeroone,twothre)
level2=rbind(level2,level2,level2,level2)
level2=rbind(level2,level2)
level2=cbind(level2,level2,level2,level2)
level2=cbind(level2,level2)

level2=cbind(level2,level2,level2,level2,level2,level2,level2,level2)
level2=rbind(level2,level2,level2,level2,level2,level2,level2,level2)

############################################### LEVEL 3 (4 charts)
zero=matrix(data=0, nrow=4, ncol=4)
one=matrix(data=1, nrow=4, ncol=4)
two=matrix(data=2, nrow=4, ncol=4)
thre=matrix(data=3, nrow=4, ncol=4)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level3 =rbind(zeroone,twothre)
level3=rbind(level3,level3,level3,level3)
level3=cbind(level3,level3,level3,level3)


level3=cbind(level3,level3,level3,level3,level3,level3,level3,level3)
level3=rbind(level3,level3,level3,level3,level3,level3,level3,level3)

################################################# LEVEL 4
zero=matrix(data=0,nrow=8,ncol=8)
one=matrix(data=1, nrow=8, ncol=8)
two=matrix(data=2, nrow=8, ncol=8)
thre=matrix(data=3, nrow=8, ncol=8)

zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level4 =rbind(zeroone,twothre)
level4=rbind(level4,level4)
level4=cbind(level4,level4)

level4=cbind(level4,level4,level4,level4,level4,level4,level4,level4)
level4=rbind(level4,level4,level4,level4,level4,level4,level4,level4)


######################################################### LEVEL 5
zero=matrix(data=0,nrow=16,ncol=16)
one=matrix(data=1, nrow=16, ncol=16)
two=matrix(data=2, nrow=16, ncol=16)
thre=matrix(data=3, nrow=16, ncol=16)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level5 =rbind(zeroone,twothre)

level5=cbind(level5,level5,level5,level5,level5,level5,level5,level5)
level5=rbind(level5,level5,level5,level5,level5,level5,level5,level5)


########################################################################## JOIN 1-5 LEVEL
imgName =matrix(nrow=256, ncol=256)
for (i in 1:256){
for (y in 1:256)
  {
imgName[i,y]= paste0(level5[i,y],level4[i,y],level3[i,y],level2[i,y],level1[i,y],sep="") 
}}

imgName=rbind(imgName,imgName,imgName,imgName)
imgName=cbind(imgName,imgName,imgName,imgName)
################################################################################################### FOLDER LEVEL 1
zero=matrix(data=0,nrow=32,ncol=32)
one=matrix(data=1, nrow=32, ncol=32)
two=matrix(data=2, nrow=32, ncol=32)
thre=matrix(data=3, nrow=32, ncol=32)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level6 =rbind(zeroone,twothre)

level6=rbind(level6,level6,level6,level6,level6,level6,level6,level6)
level6=cbind(level6,level6,level6,level6,level6,level6,level6,level6)
level6=rbind(level6,level6)
level6=cbind(level6,level6)
################################################################################################ FOLDER LEVEL 2
zero=matrix(data=0,nrow=64,ncol=64)
one=matrix(data=1, nrow=64, ncol=64)
two=matrix(data=2, nrow=64, ncol=64)
thre=matrix(data=3, nrow=64, ncol=64)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level7 =rbind(zeroone,twothre)
level7=rbind(level7,level7)
level7=cbind(level7,level7)

level7=rbind(level7,level7,level7,level7)
level7=cbind(level7,level7,level7,level7)
############################################################################################### FOLDER LEVEL 3
zero=matrix(data=0,nrow=128,ncol=128)
one=matrix(data=1, nrow=128, ncol=128)
two=matrix(data=2, nrow=128, ncol=128)
thre=matrix(data=3, nrow=128, ncol=128)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level8 =rbind(zeroone,twothre)
level8=rbind(level8,level8,level8,level8)
level8=cbind(level8,level8,level8,level8)
############################################################################################ FOLDER LEVEL 4
zero=matrix(data=0,nrow=256,ncol=256)
one=matrix(data=1, nrow=256, ncol=256)
two=matrix(data=2, nrow=256, ncol=256)
thre=matrix(data=3, nrow=256, ncol=256)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level9 =rbind(zeroone,twothre)
level9=cbind(level9,level9)
level9=rbind(level9,level9)
##########################################################################################  FOLDER LEVEL 5
zero=matrix(data=0,nrow=512,ncol=512)
one=matrix(data=1, nrow=512, ncol=512)
two=matrix(data=2, nrow=512, ncol=512)
thre=matrix(data=3, nrow=512, ncol=512)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level10 =rbind(zeroone,twothre)
########################################################################################### JOIN FOLDER NAME
folderName = matrix(nrow=1024, ncol=1024)
for (i in 1:1024) {
for (y in 1:1024) { 
folderName[i,y]= paste0(level10[i,y],level9[i,y],level8[i,y],level7[i,y],level6[i,y],sep="") 
}}
#########################################################################################JOIN IMG-FOLDER NAME
 nameImg=matrix(nrow=1024, ncol=1024)
 for (i in 1:1024){
   for (y in 1:1024) 
   {
     nameImg[i,y]= paste0("f",folderName[i,y],"_f", imgName[i,y],".png", sep="")  
   }}
########################################################################################  CREATE EMPTY FILD PERIMETR
zero=c(rep("NA",1024))
mtrx=rbind(zero,nameImg)
mtrx=rbind(mtrx,zero)
mtrx=cbind(mtrx,c(zero,"NA","NA"))
mtrx=cbind(c(zero,"NA","NA"),mtrx)
#########################################################@########################################## CREATE TABLE NEAR FROM MATRIX
vec=rep(0,1048576)
TableNear =data.frame(imgName=vec,leftName= vec,upName= vec, rightName=vec, downName=vec)
line=1
for (i in 1:1024){       # обработка строчек
  for (y in 1:1024){     # обработка столбцов

TableNear$imgName[line]=mtrx[i,y]
if (y-1>0)
{
TableNear$leftName[line]=mtrx[i,y-1] 
}
else {
TableNear$leftName[line]="edge"
}
if (i-1>0) 
{
TableNear$upName[line]=mtrx[i-1,y]
}
else {
TableNear$upName[line]="edge"
}
TableNear$rightName[line]=mtrx[i,y+1]
TableNear$downName[line]=mtrx[i+1,y]
line =line +1
}}
#################################################################




