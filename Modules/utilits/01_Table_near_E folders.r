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

################################################################################################### LEVEL 6 (FOLDER)
zero=matrix(data=0,nrow=32,ncol=32)
one=matrix(data=1, nrow=32, ncol=32)
two=matrix(data=2, nrow=32, ncol=32)
thre=matrix(data=3, nrow=32, ncol=32)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level6 =rbind(zeroone,twothre)

level6=rbind(level6,level6,level6,level6)
level6=cbind(level6,level6,level6,level6)

################################################################################################ LEVEL 7 (FOLDER)
zero=matrix(data=0,nrow=64,ncol=64)
one=matrix(data=1, nrow=64, ncol=64)
two=matrix(data=2, nrow=64, ncol=64)
thre=matrix(data=3, nrow=64, ncol=64)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level7 =rbind(zeroone,twothre)
level7=rbind(level7,level7)
level7=cbind(level7,level7)
############################################################################################### LEVEL 8 (FOLDER)
zero=matrix(data=0,nrow=128,ncol=128)
one=matrix(data=1, nrow=128, ncol=128)
two=matrix(data=2, nrow=128, ncol=128)
thre=matrix(data=3, nrow=128, ncol=128)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level8 =rbind(zeroone,twothre)
############################################################################################ LEVEL 9 (FOLDER)
zero=matrix(data=0,nrow=256,ncol=256)
one=matrix(data=1, nrow=256, ncol=256)
two=matrix(data=2, nrow=256, ncol=256)
thre=matrix(data=3, nrow=256, ncol=256)
zeroone=cbind(zero, one)
twothre=cbind(two,thre)
level9 =rbind(zeroone,twothre)
########################################################################################### JOIN FOLDER NAME
folderName = matrix(nrow=512, ncol=512)
for (i in 1:512) {
for (y in 1:512) {
for (x in 1:512) {
  
folderName[i,y,x]= paste0(level9[i,y,x],level8[i,y,x],level7[i,y,x],level6[i,y,x],sep="") 
}}}
#########################################################################################JOIN IMG-FOLDER NAME
 nameImg=matrix(nrow=512, ncol=512)
 for (i in 1:512){
   for (y in 1:512) 
   {
     nameImg[i,y]= paste0("d",folderName[i,y],"_f", imgName[i,y],".png", sep="")  
   }}
########################################################################################  CREATE EMPTY FILD PERIMETR
zero=c(rep("NA",256))
matr256=rbind(zero,nameImg)
matr256=rbind(matr256,zero)
matr256=cbind(matr256,c(zero,"NA","NA"))
matr256=cbind(c(zero,"NA","NA"),matr256)
#########################################################@########################################## CREATE TABLE NEAR FROM MATRIX
vec=rep(0,65536)
TableNear =data.frame(imgName=vec,leftName= vec,upName= vec, rightName=vec, downName=vec)
line=1
for (i in 1:256){       # обработка строчек
  for (y in 1:256){     # обработка столбцов

TableNear$imgName[line]=matr256[i,y]
if (y-1>0)
{
TableNear$leftName[line]=matr256[i,y-1] 
}
else {
TableNear$leftName[line]="edge"
}
if (i-1>0) 
{
TableNear$upName[line]=matr256[i-1,y]
}
else {
TableNear$upName[line]="edge"
}
TableNear$rightName[line]=matr256[i,y+1]
TableNear$downName[line]=matr256[i+1,y]
line =line +1
}}
#################################################################




