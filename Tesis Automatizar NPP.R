#
####Cuantificación del número de embarcaciones totales (NPP-VIIRS)#########
###########################################################################
library(raster) 
library(igraph)
library(base)
library(matrixcalc)
library(reshape2)
library(rasterVis)
library(maptools)
library(rgeos)
####1.Estandarización######################################################
inputo_NPP <- raster("C:/Tesis/GDNBO-SVDNB_npp_d20130114_t0722469.tif")
inputo_NPP <- reclassify(inputo_NPP, cbind(-9999.000000, NA))
inputo_NPP <- log10(inputo_NPP)       #valores de radiancia del cada pixel
inputo_NPP[is.infinite(inputo_NPP)] <- NA 
max=maxValue(inputo_NPP)         
min=minValue(inputo_NPP)
slope=254/(max-min)                   #transformar a escala [0-255]
intercept=1-(slope*min)
image_grey=inputo_NPP*slope+intercept
####2.Umbral de detección##################################################
image_grey <- reclassify(image_grey, cbind(-NA, NA))
image_grey1 <- as.matrix(image_grey)   
image_grey1 = shift.right(image_grey,1)      
image_grey1 = raster(image_grey1)     #(i+1)
imagen_gradiente = overlay(image_grey,image_grey1, 
                           fun=function(x,y){return(x-y)})
umbralmax=maxValue(imagen_gradiente)         
umbralmin=minValue(imagen_gradiente)

umbral = umbralmin - (umbralmax-umbralmin)/2  
                                      #valor del umbral 
image_umbral[ imagen_gradiente[] < umbral ] <- NA 
                                      #eliminando valores<umbral  
image_umbral
#####3.Segmentación######################################################## 
require(sp)
require(rgdal)
library(tiff)
imagen_seg1 <- clump(image_umbral)
matriz <- as.matrix(imagen_seg1)
lapply(seq_len(imagen_seg1@data@max), function(x) {
  inds <- which(matriz == x, arr.ind = TRUE)
  nrow <- diff(range(inds[, "row"])) + 1
  ncol <- diff(range(inds[, "col"])) + 1
  matrix(1, ncol = ncol, nrow = nrow)
})

lista_emb<- hclust(dist(data.frame(rownames=rownames(matriz@data),
              x=coordinates(matriz)[,1],
              y=coordinates(matriz)[,2])), method="complete")

lista_emb <- SpatialPointsDataFrame(matrix(c(x,y), ncol=2),
              data.frame(ID=seq(1:length(x))),
              proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
imagen_seg1<-as.raster(matrix)
embarcaciones1 <- write.csv(lista_emb,
              'C:/Tesis/listaEmbarcaciones1.csv',overwrite=TRUE)
####4.Validación###########################################################
embarcaciones1 <- read.csv("C:/Tesis/listaEmbarcaciones1.csv")
imput_dataAIS <-read.csv("C:/Tesis/embarccionesAIS.csv")

regresion <- lm(embarcaciones1 ~ imput_dataAIS,data=embarcaciones1)
imagen_seg1
####5.Re-Segmentación####6.Categorización#####7.Cuantificación#############
library(imager)
imagen_seg1 <- mutate(imagen_seg1,index=as.data.frame(i.max)$value)
regs <- dplyr::group_by(imagen_seg1,value) %>% 
  dplyr::summarise(mx=mean(x),my=mean(y),scale.index=mean(index))
p <- ggplot(as.data.frame(imagen_seg1),aes(x,y))
     +geom_raster(aes(fill=value))
     +geom_point(data=regs,aes(mx,my,size=scale.index),pch=2,col="red")
p+scale_fill_gradient(low="black",high="white")
     +scale_x_continuous(expand=c(0,0))
     +scale_y_continuous(expand=c(0,0),trans=scales::reverse_trans())

centers <- ddply(imagen_seg2,.(value),summarise,mx=mean(x),my=mean(y))
centers <- dplyr::group_by(imagen_seg2,value) %>% 
           dplyr::summarise(mx=mean(x),my=mean(y))
imagen_seg2 <- as.data.frame(imagen_seg2) %>% subset(value>0)
embarcaciones2 <- write.csv(imagen_seg2,
              'C:/Tesis/listaEmbarcaciones2.csv',overwrite=TRUE)
###########################################################################
