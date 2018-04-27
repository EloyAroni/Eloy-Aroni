
####Cuantificación del número de embarcaciones totales (NPP-VIIRS)########
library(raster) 
library(sp)
library(rgdal)
library(igraph)
library(tiff)
library(base)#Delete Inf and -Inf
library(imager) # gradient similar to Filter Sobel 
library(matrixcalc)# convertir 
library(sp)
library(reshape2)# MATRIX
library(rasterVis)  # raster visualisation
library(maptools)
#library(rgeos)


inputo_NPP <- raster("C:/Users/earoni/Desktop/Prueba R Tesis/GDNBO-SVDNB_npp_d20130114_t0722469_e0728273_b06298_c20170503150230436532_noaa_ops.tif")
inputo_NPP

inputo_NPP <- reclassify(inputo_NPP, cbind(-9999.000000, NA)) # convert value "NA" to 0
#values(inputo_NPP)[values(inputo_NPP) = -9999.000000] = NA
inputo_NPP
#1.- Log10 transformation#####################################################################################################
inputo_NPP <- log10(inputo_NPP) # For each pixel of the image a log10 is applied over the radiance value
inputo_NPP

inputo_NPP[is.infinite(inputo_NPP)] <- NA 
inputo_NPP

#2.- Transformation [0-255]###################################################################################################
max=maxValue(inputo_NPP) 
max
min=minValue(inputo_NPP)
min

slope=254/(max-min)
slope

intercept=1-(slope*min)
intercept


image_grey=inputo_NPP*slope+intercept
image_grey

max=maxValue(image_grey) 
max
min=minValue(image_grey)
min
#3.- Threshold detection######################################################################################################

image_grey[ image_grey[] < 100 ] <- NA # delete value low 100 because this pixel is the black oceans.
image_grey <- reclassify(image_grey, cbind(-NA, NA)) # convert value "NA" to 0

image_grey
saveimage_grey=image_grey
saveimage_grey<- writeRaster(image_grey, 'C:/Users/earoni/Desktop/borrar/Han Xu/image_grey.tif', overwrite=TRUE)

image_grey=image_grey


#############################################################################################################################

#XY <- raster("C:/Users/earoni/Desktop/borrar/Han Xu/GDNBO.tif")
XY <- image_grey

XY
which.min(XY)
which.max(XY)
head(XY)


########### Extract lat/long values ##########################
GEO=XY
GEO <- reclassify(GEO, cbind(NA, 0)) # convert value "NA" to 0
GEO= rasterToPoints( GEO )
colnames(GEO) <- c("xx","yy","value") # changue name column
GEO <- as.data.frame(GEO)
head(GEO)


###### Move one position (i+1) / extract cell values #########
A <- as.matrix(XY)   # move in matrix
A = shift.right( A, 1 )  # (i+1)
A <- raster(A)

A <- reclassify(A, cbind(NA, 0)) # convert value "NA" to 0
A <- rasterToPoints(A)
A <- as.data.frame(A)
head(A)

########### Creating data.frame (lat/long/value) #############
A<-data.frame(GEO$xx, GEO$yy, A$layer)
colnames(A)<-c('x',"y","z")
head(A)

################ Create mask of raster #######################
#sat<-raster("C:/Users/earoni/Desktop/borrar/Han Xu/GDNBO.tif")
sat<- image_grey
r<-raster(sat) 
r<-setValues(r,0) # cell values egual = 0

x <- rasterize(A[, 1:2], r, A[,3], fun=mean) # rasterize data frame
head(x)

x <- reclassify(x, cbind(0, NA)) # convert value "0" to NA

x <- writeRaster(x, 'C:/Users/earoni/Desktop/borrar/Han Xu/image_grey1.tif', overwrite=TRUE)

image_grey1=x

###############################################################

image_grey=image_grey # imagen gris (i)
image_grey1=image_grey1  # imagen gris (i+1)

##### similar filter Soble #################################### 

spole <- raster("C:/Users/earoni/Desktop/Prueba R Tesis/gradiente.tif")

#r1 = init(r, function(x) sample(5:6, x, replace = TRUE))
#r1
#r2 = init(r, function(x) sample(1:2, x, replace = TRUE))
#r2
#imagen_gradiente = overlay(r1, r2, fun=function(x,y){return(x-y)})
#r3
imagen_gradiente = overlay(image_grey,image_grey1, fun=function(x,y){return(x-y)})
imagen_gradiente

















 
 
 
 
 

 
