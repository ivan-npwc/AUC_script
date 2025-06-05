https://heads0rtai1s.github.io/2021/02/25/gpu-setup-r-python-ubuntu/
https://tensorflow.rstudio.com/install/local_gpu
######################################################### NVIDIA driver instalation
nvidia-smi

cat /proc/driver/nvidia/version

#sudo sh ./NVIDIA-Linux-x86_64-550.144.03.run
#sudo ubuntu-drivers install --gpgpu
#sudo ubuntu-drivers install --gpgpu nvidia:560   #-server
#sudo ubuntu-drivers install nvidia:560



##################################  R INSTALATION
sudo apt update
sudo apt install software-properties-common dirmngr -y
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt update
sudo apt install r-base r-base-dev -y
######################################## DOWNLOAD TENSORFLOW AND PYTHON
R
install.packages("remotes")
install.packages("reticulate")
remotes::install_github("rstudio/tensorflow") # download tensorflow
reticulate::install_python()                  #python instalation IF dont work try to install other ways
#Sys.setenv(RETICULATE_PYTHON="/usr/local/bin/python") # set the path to python IW PUTHON WAS INSTALED MANUALY
q()
n
######################################### PYTHON INSTALATION (IN CASE)
#reticulate::install_miniconda()  
#Don't change the default python version. You might end up destroying Ubuntu. Instead, use Miniconda/Anaconda to create a virtual environment. 
#Do not install TensorFlow with conda. It may not have the latest stable version. pip is recommended since TensorFlow is only officially released to PyPI.
############################# CUDA INSTALATION
wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2204/x86_64/cuda-keyring_1.1-1_all.deb
sudo dpkg -i cuda-keyring_1.1-1_all.deb
sudo apt-get update
sudo apt-get -y install cuda-toolkit-11-8
nvidia-smi
############################################################## CudNN instalation (with tensorflow in some time)
R
install_tensorflow() #cuDNN 8.6 is automatically installed by install_tensorflow() via pip if a GPU is detected.

tf$constant("Hello TensorFlow!")
tf$device_lib$list_local_devices()
tf$config$list_physical_devices("GPU")


library(keras)
install_keras()
is_keras_available()
################################################################t    TEST

#######################################################################check cuda and video driver instalation
cat /proc/driver/nvidia/version
nvidia-smi
#################################################################### clean up all CUDA and NVIDIA drivers (by shure display can error)
# Remove CUDA Toolkit:
sudo apt-get --purge remove "*cublas*" "*cufft*" "*curand*" "*cusolver*" "*cusparse*" "*npp*" "*nvjpeg*" "cuda*" "nsight*" 
# Remove Nvidia Drivers:
sudo apt-get --purge remove "*nvidia*"
# Clean up the uninstall:
sudo apt-get autoremove
########################################################################################### else way to install cuda --change version 



sudo rstudio-server start
http://localhost:8787

library(tensorflow)
library(reticulate)

#reticulate::install_python(version="3.10") 
install_tensorflow(version=2.15) 
install_tensorflow(version="gpu") 
tf$config$list_physical_devices("GPU")

library(tensorflow)
tf$constant("Hello TensorFlow!")








