########## run once ############
# download latest version of R:
# https://cran.r-project.org/mirrors.html 
# and RStudio
# https://www.rstudio.com/products/rstudio/download/

### Mac OSX users ###
# download XQuartz for Mac to read images if not already installed. It's the standard image processing app for Mac OSX.
# https://www.xquartz.org/ 

#load packages. Select 'y' to download dependencies. 
packages <- c("rgeos","sp","maptools", "raster","rgdal","dplyr","zoo","plyr","spatstat", "digitize","jpeg","devtools","imager","dplyr","ggplot2")       
install.packages(packages,dependencies = T) ; install.packages("svDialogs")
lapply(packages,library,character.only = T)

#set working dir
wd <- dlgInput("Set working directory", "Your working directory (without quotations)")$res
setwd(wd)
################################

#do you want to see all the intermediate steps and progress plots (0 = saves time)?
# 1 = yes, 0 = no
see_progress_plots <- 0

#################################

# enter file name
fh <- dlgInput("Enter file name", "E.g. XXX.jpg")$res; fh
# set threshold value between 99 and 100 to isolate points. lower = coarser 
threshold <- dlgInput("Set threshold value for isolating points (btw 99 and 100). Lower = coarser", "E.g. 99")$res ; threshold <- paste0(threshold,"%"); threshold   
# value to change blurriness of pixels. Between 9 and 12 works well. 
sigma <-dlgInput("Sigma value for blurriness of pixels (btw 0 and 15; 0 = most clear and sensitive)", "E.g. 11")$res; sigma <- as.numeric(sigma); sigma

#################################

### now run everthing from here ###
# run function for getting centre of points 
get.centers <- function(im,thr=threshold){
  dt <- imhessian(im) %$% { xx*yy - xy^2 } %>% threshold(thr) %>% label
  as.data.frame(dt) %>% subset(value>0) %>% dplyr::group_by(value) %>% dplyr::summarise(mx=mean(x),my=mean(y))      
  }

img <- load.image(fh) # import image
img <- grayscale(img,method="Luma",drop=T) # convert to grayscale to isolate points later
# increase gamma (make darker to highlight schistos)
img <- img ^ 1.5

# view original image
plot(img, main=paste0("Original image")) # original image 
if(see_progress_plots==1){plot(isoblur(img,sigma), main=paste0("Original image with, \n threshold = ", threshold, " sigma = ",sigma))} # what sigma looks like with original image

# find centre points and plot img with sigma 
if(see_progress_plots==1){isoblur(img,sigma) %>% get.centers(threshold) %$% points(mx,my,pch=2,cex=1.5,col="blue", main=paste0("Original image with, \n threshold = ", threshold, " sigma = ",sigma))} 
# get schisto count 
sc <- round(get.centers(isoblur(img,sigma),threshold)) #round up to full integers
sc_final <- length(unique(sc$my)) # and get final count with any overlapping points removed 

# now plot xy coords on original, clear image
scdf <- as.data.frame(sc) # turn xy coords into dataframe
plot(img, main=paste0("Updated image with, \n threshold = ", threshold, " sigma = ",sigma)) ; with(scdf, points(mx,my,pch=2,cex=2,col="red")) # plot points on original image

# if happy with the point coverage, get final schisto count
print(paste0("Final schisto count is ",sc_final))
# add script count to manual count
manual_count <- dlgInput("Manual schisto count", "manual_count")$res; print(paste0("Final manual schisto count is ",as.numeric(manual_count) + sc_final))


