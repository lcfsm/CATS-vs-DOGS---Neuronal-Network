rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
datos_train=read.csv("DatasetTrain.csv")
datos_train[1]<-NULL
datos_test=read.csv("DatasetTest.csv")
datos_test[1:3]<-NULL

library(neuralnet)
# RED NEURONAL
# -----------------------------------------------------
nms  <- names(datos_train[3:15])
frml <- as.formula(paste("animaltype ~", paste(nms, collapse = " + ")))

# VARIABLES DE CONFIGURACION
# -----------------------------------------------------
numeroCapasOcultas=c(13,6)
thres=0.2

# MODELO
# -----------------------------------------------------
modelo <- neuralnet(frml,
                    data = datos_train,
                    hidden = numeroCapasOcultas,
                    stepmax = 1e+06,
                    rep = 1, #numero de iteraciones
                    lifesign = "full",
                    linear.output = FALSE,
                    threshold     = thres,
                    algorithm     = "rprop+")

#GRAFICO DEL MODELO
# -----------------------------------------------------
plot(modelo,rep = "best") #best muestra el mejor de todas las iteracciones, rep=numero iteraciones

# PREDICCION
# -----------------------------------------------------
prediccion  <- compute(modelo,within(datos_test,rm(animaltype)))
table<-data.frame(Real = datos_test$animaltype, Predicted = prediccion$net.result, Error = abs(datos_test$animaltype - prediccion$net.result) / datos_test$animaltype)

#MEDIR LA CORRELACION ENTRE LOS RESULTADOS DE LA RED
# -----------------------------------------------------
#prediccion.animaltype <- prediccion$net.result
#cor(prediccion.animaltype,datos_test$animaltype)

gatos_acertados=0
cantidad_gatos=0
cantidad_perros=0
perros_acertados=0

#Comprobar el porcentaje de acierto y error
for(i in 1:nrow(table)){
  if(table[i,1]==0){
    cantidad_gatos=cantidad_gatos+1
  }else{
    cantidad_perros=cantidad_perros+1
  }
  if(table[i,1]==0 && table[i,2]<=0.5){
    gatos_acertados=gatos_acertados+1
  }
  if(table[i,1]==1 && table[i,2]>0.5){
    perros_acertados=perros_acertados+1
  }
}

porcentaje_acierto_gatos=gatos_acertados*100/cantidad_gatos
porcentaje_acierto_perros=perros_acertados*100/cantidad_perros
porcentaje_acierto_total<-(gatos_acertados+perros_acertados)*100/nrow(table)

cantidad_gatos
cantidad_perros
gatos_acertados
perros_acertados
porcentaje_acierto_gatos
porcentaje_acierto_perros
porcentaje_acierto_total


#SAVE OUR MODEL TO DISK
saveRDS(modelo, "./final_model.rds")

#Load the model
super_model <- readRDS("./final_model.rds")
print(super_model)


prediccion  <- compute(super_model,within(datos_test,rm(animaltype)))
table<-data.frame(Real = datos_test$animaltype, Predicted = prediccion$net.result, Error = abs(datos_test$animaltype - prediccion$net.result) / datos_test$animaltype)

#MEDIR LA CORRELACION ENTRE LO

