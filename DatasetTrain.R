rm(list = ls())

library(imager)
library(stringr)
devtools::install_github("dahtah/fixedpoints")
library(fixedpoints)
# Set wd where images are located
setwd("C:\\Users\\LAPTOP\\Desktop\\I.A.A\\PROYECTO\\train")
#setwd("/Users/kevin/Desktop/Master/InteligenciaArtificial/R/Proyecto/train")
images <- list.files(pattern = ".jpg") # Load images names, only JPGs) # Load images names
fpath<-"C:\\Users\\LAPTOP\\Desktop\\I.A.A\\PROYECTO\\train\\"
#fpath<-"/Users/kevin/Desktop/Master/InteligenciaArtificial/R/Proyecto/train/"


#Variables para la dimension de la imagen
height=300
width=300

images<-as.data.frame(images)

#new columns
images$animaltype<- NA 
images$feature_mean_color<-NA
images$feature_mean_red<- NA
images$feature_mean_green<- NA
images$feature_mean_blue<- NA
images$feature_mean_gradient<-NA
images$feature_mean_gradient_channel1<-NA
images$feature_mean_gradient_channel2<-NA
images$feature_mean_gradient_channel3<-NA
images$feature_numberOfLittleObjects<-NA
images$feature_numberOfMediumObjects<-NA
images$feature_detectedStrongEdges<-NA
images$feature_detectedWeakEdges<-NA
images$feature_numberOfStringPixelsForEdges<-NA


for(i in 1:nrow(images)){ 
  
  # Ponemos el valor de cada animal, 0 cat, 1 dog  
  chars <- as.character(images$images[i])
  value <- "cat"
  animaltype<-grepl(value, chars)
  if (animaltype==TRUE){
    images[i,2] <- 0
  }else{
    images[i,2] <- 1
  }
  
  ####### UTILIZAMOS LA LIBRERIA IMAGER ########################################################
  
  #library(imager)
  pet <- load.image(str_c(fpath,images[i,1])) # leer con libreria imager
  pet<- resize(pet,height,width)
  #plot(pet,main="foto")
  #Redimensionamos la imagen
  value<-R(pet)
  pet<- as.data.frame(pet)
  pet <- plyr::mutate(pet,channel=factor(cc,labels=c('R','G','B')))
  
  #Sacar valores RGB
  #Color promedio de toda la imagen
  feature_mean_color<-mean(pet$value)
  images$feature_mean_color[i]<-feature_mean_color
  
  #Sacar valor Rojo promedio, sumar los primeros 40000 valores del canal 1 y sacar la media
  red<-head(pet,40000)
  feature_mean_red<-mean(red$value)
  images$feature_mean_red[i]<-feature_mean_red
  #Sacar valor Verde promedio, sumar los valores entre 40000 y 80000 del canal 2 y sacar la media
  green<-subset(pet,cc==2)
  feature_mean_green<-mean(green$value)
  images$feature_mean_green[i]<-feature_mean_green
  #Sacar valor Azul promedio, sumar los ultimos 40000 valores del canal 3 y sacar la media
  blue<-tail(pet,40000)
  feature_mean_blue<-mean(blue$value)
  images$feature_mean_blue[i]<-feature_mean_blue
  
  #Sacar la luminosidad
  pet <- load.image(str_c(fpath,images[i,1])) # leer con libreria imager  58
  pet<- resize(pet,height,width)
  
  grayimage=grayscale(pet)
  #plot(grayscale(pet))
  pet<- as.data.frame(pet)
  feature_mean_color_grayimage<-mean(pet$value)
  #La media nos da lo mismo que con la imagen en color, PREGUNTAR PORQUE
  
  ###########  Image Gradient ####################################################
  pet <- load.image(str_c(fpath,images[i,1])) # leer con libreria imager
  pet<- resize(pet,height,width)
  gradient<-imgradient(pet,"xy")
  #plot(gradient)
  gradient<-as.data.frame(gradient)
  
  #Sacar el valor promedio del gradiente, todos los canales
  subset_gradient<-subset(gradient,gradient$value>=0)
  feature_mean_gradient<-mean(subset_gradient$value)
  images$feature_mean_gradient[i]<-feature_mean_gradient
  
  #Sacar el valor promedio del gradiente en el canal 1 (red)
  subset_gradient_channel1<-subset(subset_gradient, subset_gradient$cc==1)
  feature_mean_gradient_channel1<-mean(subset_gradient_channel1$value)
  images$feature_mean_gradient_channel1[i]<-feature_mean_gradient_channel1
  #Sacar el valor promedio del gradiente en el canal 2 (green)
  subset_gradient_channel2<-subset(subset_gradient, subset_gradient$cc==2)
  feature_mean_gradient_channel2<-mean(subset_gradient_channel2$value)
  images$feature_mean_gradient_channel2[i]<-feature_mean_gradient_channel2
  #Sacar el valor promedio del gradiente en el canal 3 (blue)
  subset_gradient_channel3<-subset(subset_gradient, subset_gradient$cc==3)
  feature_mean_gradient_channel3<-mean(subset_gradient_channel3$value)
  images$feature_mean_gradient_channel3[i]<-feature_mean_gradient_channel3
  
  
  ################## Determinant of Gesian ###################################################
  
  #Detectar objetos cuantos objectos pequenos
  get.centers <- function(im,thr="99%"){
    dt <- imhessian(im) %$% { xx*yy - xy^2 } %>% threshold(thr) %>% label
    as.data.frame(dt) %>% subset(value>0) %>% dplyr::group_by(value) %>% dplyr::summarise(mx=mean(x),my=mean(y))
  }
  
  #plot(grayimage)
  get.centers(grayimage,"99%") %$% points(mx,my,col="red")
  feature_numberOfLittleObjects = dim(get.centers(grayimage,"99%"))[1]
  images$feature_numberOfLittleObjects[i]<-feature_numberOfLittleObjects
  
  
  #Detectar objetos de diferente escala
  #Compute determinant at scale "scale". 
  hessdet <- function(im,scale=1) isoblur(im,scale) %>% imhessian %$% { scale^2*(xx*yy - xy^2) }
  #Note the scaling (scale^2) factor in the determinant
  result_scale=as.data.frame(hessdet(grayimage,1))
  #plot(hessdet(grayimage,1),main="Determinant of the Hessian at scale 1")
  get.centers(grayimage,"95.5%") %$% points(mx,my,col="red")
  feature_numberOfMediumObjects = dim(get.centers(grayimage,"95.5%"))[1]
  images$feature_numberOfMediumObjects[i]<-feature_numberOfMediumObjects
  
  prueba<-as.data.frame(get.centers(grayimage,"98.5%"))
  
  ################## Canny Edge ###################################################
  #funciones alternativas para el mismo uso >>>>>>>>> http://dahtah.github.io/imager/canny.html
  
  pet <- load.image(str_c(fpath,images[i,1]))
  
  #PASO1: denoising
  im <- grayscale(pet) %>% isoblur(2)
  #plot(im)
  
  #PASO2: image gradient, magnitude and angle
  gr <- imgradient(im,"xy")
  #plot(gr,layout="row")
  
  mag <- with(gr,sqrt(x^2+y^2))
  #plot(mag)
  
  ang <- with(gr,atan2(y,x))
  #plot(ang)
  
  #PASO 3: Cleaning using non-maxima suppression
  threshold(mag) %>% plot
  
  #Going along the (normalised) gradient
  #Xc(im) is an image containing the x coordinates of the image
  nX <- Xc(im) + gr$x/mag 
  nY <- Yc(im) + gr$y/mag
  #nX and nY are not integer values, so we can't use them directly as indices.
  #We can use interpolation, though:
  val.fwd <- interp(mag,data.frame(x=as.vector(nX),y=as.vector(nY)))
  
  nX <- Xc(im) - gr$x/mag 
  nY <- Yc(im) - gr$y/mag
  val.bwd <- interp(mag,data.frame(x=as.vector(nX),y=as.vector(nY)))
  
  
  #CONSEGUIMOS NEUTRALIZAR LOS NO MAXIMOS, COMPROBANDO SUS DOS VECINOS A CADA LADO
  throw <- (mag < val.bwd) | (mag < val.fwd)
  mag[throw] <- 0
  #plot(mag)
  
  #PASO4: Hysteresis
  #Detectamos los bordes mediante t1 y t2
  #Las magnitudes por encima de t1 son bordes fuertes (prominentes)
  #Las magnitudes por encima de t2 son bordes debiles (suaves)
  #strong threshold
  t2 <- quantile(mag,.96)
  #weak threshold 
  t1 <- quantile(mag,.90)
  layout(t(1:2))
  
  strong <- as.cimg(mag>t2)
  #plot(strong,main="Initial set of strong edges")
  weak <- as.cimg(mag %inr% c(t1,t2))
  #plot(weak,main="Initial set of weak edges")
  
  #El proceso de Hysteresis normal se haria mediante un búcle, comparando vecinos
  #pero no es optimo para R. Utilizaré otro enfoque que han comentado: Morphological Dilatation
  overlap <- dilate_square(strong,3)*weak 
  strong.new <- strong + overlap
  overlap2 <- dilate_square(weak, 3) *strong
  weak.new <- weak + overlap2
  #plot(strong.new,main="New set of strong edges")
  #plot(weak, main="New set of weak edges")
  
  delta <- sum(strong.new)-sum(strong)
  weakones <- sum(weak.new)-sum(weak)
  
  #Numero de bordes prominentes detectados en la imagen
  feature_detectedStrongEdges = delta
  feature_detectedWeakEdges = weakones
  
  images$feature_detectedStrongEdges[i]<-feature_detectedStrongEdges
  images$feature_detectedWeakEdges[i]<-feature_detectedWeakEdges
  
  #Permite realizar una iteracion hasta llegar a un punto en el que el valor no cambia
  #NECESARIO PARA EL PROCESO
  
  #ws is a list containing two fields, "weak" and "strong"
  #which are images where all pixels with value 1 are "weak edges" (resp. strong)
  #the function expands the set of strong pixels via dilation and overlap
  #and returns the expanded strong set and the (shrunk) weak set
  expandStrong <- function(ws)
  {
    overlap <- dilate_square(ws$strong,3)*ws$weak
    ws$strong <- ws$strong + overlap
    ws$weak <- ws$weak-overlap
    ws
  }
  
  
  #hystFP is a new function that will call expandStrong repeatedly until
  #the weak and strong sets don't change anymore
  hystFP <- fp(expandStrong)
  
  #Call hystFP
  out <- list(strong=strong,weak=weak) %>% hystFP
  out
  
  canny <- out$strong
  #plot(canny,main="Canny edges")
  
  lab <- label(strong,TRUE)*strong
  loc <- as.data.frame(lab) %>% dplyr::filter(value > 0)%>%
    dplyr::group_by(value) %>%
    dplyr::summarize(x=x[1],y=y[1])
  #plot(strong)
  points(loc$x,loc$y,col="red")
  
  feature_numberOfStringPixelsForEdges = dim(loc)[1]  
  images$feature_numberOfStringPixelsForEdges[i]<-feature_numberOfStringPixelsForEdges 
  
} #fin del bucle FOR


#DESCOMENTAR ESTA PARTE PARA GENERAR LOS DATASET SIN NORMALIZAR PARA USAR EN LA SHINY APP

############### Guardamos dos dataset (train y test) con los valores max min del dataset generado ##################
# ------------- Sirve para normalizar la imagen en la shiny app ----------------- #
# ------------- Hay que hacerlo con los atributos aun sin normalizar ------------ #

#write.csv(images,file="DatasetTrainSinNormalizar.csv") #Guardar dataset images sin normalizar

#Generar dataset de train con los valores max min para normalizar en la shiny app
#colmax=function(data)sapply(data, max, na.rm=TRUE)
#colmin=function(data)sapply(data, min, na.rm=TRUE)
#imagesporsiacaso<-images
#images<-imagesporsiacaso 
#images[1:2]<-NULL
#datamax<-as.data.frame(colmax(images))
#datamax.T <- t(datamax[,1:ncol(datamax)]) # Transpose dataset
#datamax.T <-as.data.frame(datamax.T)
#datamin<-as.data.frame(colmin(images))
#datamin.T <- t(datamin[,1:ncol(datamin)]) # Transpose dataset
#datamin.T <-as.data.frame(datamin.T)
#DatasetTrain_max_min<-merge(datamin.T,datamax.T,all=TRUE)
#nms  <- names(images[1:13])
#DatasetTrain_max_min<-setNames(DatasetTrain_max_min,  nms)
#write.csv(DatasetTrain_max_min,file="DatasetTrainMaxMin.csv")

#Generar dataset de test con los valores max min para normalizar en la shiny app
#setwd("C:\\Users\\LAPTOP\\Desktop\\I.A.A\\PROYECTO")
#data_tra=read.csv("DatasetTrain.csv")
#DataTest <- data_tra[-c(2501:22500), ]
#DataTest[1:3]<-NULL
#datamax<-as.data.frame(colmax(DataTest))
#datamax.T <- t(datamax[,1:ncol(datamax)]) # Transpose dataset
#datamax.T <-as.data.frame(datamax.T)
#datamin<-as.data.frame(colmin(DataTest))
#datamin.T <- t(datamin[,1:ncol(datamin)]) # Transpose dataset
#datamin.T <-as.data.frame(datamin.T)
#DatasetTest_max_min<-merge(datamin.T,datamax.T,all=TRUE)
#nms  <- names(DataTest[1:13])
#DatasetTest_max_min<-setNames(DatasetTest_max_min,  nms)
#write.csv(DatasetTest_max_min,file="DatasetTestMaxMin.csv")

#Guardar dataset de test sin normalizar
#setwd("C:\\Users\\LAPTOP\\Desktop\\I.A.A\\PROYECTO")
#data_tra=read.csv("DatasetTrain.csv")
#DataTest <- images[-c(2501:22500), ]
#DataTest[1:2]<-NULL
#write.csv(DataTest,file="DatasetTestSinNormalizar.csv")


############### NORMALIZACION DE ATRIBUTOS ############
# -----------------------------------------------------
doit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))*1}
images2 <- as.data.frame(lapply(images[3:15], doit))
images[3:15]<-NULL
DatasetTrain <- cbind(images, images2)

############### Guardamos el dataset ##################
# -----------------------------------------------------
write.csv(DatasetTrain,file="DatasetTrain.csv")


# DIVISION DATASET para obtener el dataset de test
# -----------------------------------------------------
#Dividimos los datos en train (primer 80%)y test (ultimo 20%)
setwd("C:\\Users\\LAPTOP\\Desktop\\I.A.A\\PROYECTO")
data_tra=read.csv("DatasetTrain.csv")
#cutoff = round(0.8*nrow(data_tra))
#images_train<-data_tra[1:cutoff,]
#images_test<-data_tra[-(1:cutoff),]



#Cogemos 20% para test (5000) los 2500 primeros gatos y los 2500 ultimos perros del dataset que tiene 25000
myDataTest <- data_tra[-c(2501:22500), ]

write.csv(myDataTest,file="DatasetTest.csv")


