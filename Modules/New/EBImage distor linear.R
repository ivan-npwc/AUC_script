library(EBImage)
img = readImage(system.file("images", "sample-color.png", package="EBImage"))

SrnLR= sample(seq(0.9,1.1,0.01))[1] 
AngLR=sample(seq(-0.1,0.1,0.01))[1]   
HSh=  1     
AngDU=sample(seq(-0.1,0.1,0.01))[1]     
SrnDU=sample(seq(0.9,1.1,0.01))[1] 
VSh=1       



m <- matrix(c(SrnLR, # shrink or srench from right if less 1- shrink, more 1- strench  # range= c(0.9,1.1)
              AngLR,    #angel from left down if more 0 and down right if less 0 # range= c(-0.1,0.1)
              HSh,  # horizontal shift in pixels # range= c(1,1)
              AngDU, # angel from right down- up     # range= c(-0.1,0.1)
              SrnDU, # srink-strench down-top    # range= c(0.9,1.1)
              VSh), #vertical shift  # range= c(1,1) 
            nrow=3)

m
img_affine = affine(img, m)
display( img_affine )
#cbind(img, 1) %*% m
