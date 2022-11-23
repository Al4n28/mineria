datos <- read.csv("rentBike_data.csv",sep = ",",header = TRUE)
datos2 = datos[,-c(1, 12, 13, 14)]
selectrows <- sample(1:nrow(datos2),round(0.80*nrow(datos2)))
dat.train <- datos2[selectrows,]
dat.test <- datos2[-selectrows,]
library(ggplot2)
library(class)
library(caret)
library(e1071)
library(FNN)
library(MASS)
library(tidyverse)
library(visdat)#tipos de variables
library(dlookr)#resumen medidas estadísticas
library(funModeling)
library(flextable) 
library(inspectdf) 
library(qqplotr) 
library(ggpmisc)
library(PerformanceAnalytics)
library(corrplot)
library(VIM) #datos faltantes
library(mice)
library(knitr)
library(MASS)


control <- trainControl(method="cv", number=10)
set.seed(7)

#Regresión Lineal Múltiple
st.time<-Sys.time()
fitR.lm <- train(Rented_Bike_Count~., data=dat.train, method="lm", metric="RMSE", trControl=control)
end.time<- Sys.time()
tim.lm<-end.time-st.time
tim.lm

#árbol de decisión
st.time<-Sys.time()
fitR.dt <- train(Rented_Bike_Count~., data=dat.train, method="rpart", metric="RMSE", trControl=control,na.action=na.omit,tuneLength=5)
end.time<-Sys.time()
tim.dt<-end.time-st.time
tim.dt
#redes neuronales
st.time<-Sys.time()
fitR.nn <- train(Rented_Bike_Count~., data=dat.train,
                 method = "nnet",metric="RMSE",trControl = control,linout=TRUE,
                 preProcess=c("scale","center"),na.action = na.omit,trace=F,maxit = 1000,tunelength=9)
end.time<-Sys.time()
tim.nn<-end.time-st.time
tim.nn


##K-vecinos más cercanos
st.time<-Sys.time()
fitR.knn <- train(Rented_Bike_Count~., data=dat.train, method="knn", metric="RMSE",
                  linout=TRUE, preProcess=c("scale","center"), trControl=control,na.action=na.omit)
end.time<-Sys.time()
tim.knn<-end.time-st.time

#tiempo cada algoritmo (agregar de cada estudiantes)
time.all<-data.frame(Algoritmos=c("LM","DT","NN","KNN"),
                     Procesador1=c(tim.lm,tim.dt,tim.nn,tim.knn),
                     Procesador2=c(1,2,3,4))

library(flextable)
time.all%>% flextable()

##gráfico comparativo
list_reg<-list(lm=fitR.lm,dt=fitR.dt, nn=fitR.nn,knn=fitR.knn)
all_reg <- resamples(list_reg)
summary(all_reg)
dotplot(all_reg,scales="free")

##Predicción
med.reg<-function(obs,pred){
  e = obs-pred
  bias = mean(e)
  mse = mean((e)^2)
  mae = mean(abs(e))
  rmse = sqrt(mse)
  R2 = 1-(sum((e)^2)/sum((obs-mean(obs))^2))
  medidas = data.frame(bias,mse,mae,rmse,R2)
  medidas
}

pred.lm<-predict(fitR.lm,dat.test)
val.lm<-med.reg(dat.test$Rented_Bike_Count,pred.lm)

pred.dt<-predict(fitR.dt,dat.test)
val.dt<-med.reg(dat.test$Rented_Bike_Count,pred.dt)

pred.nn<-predict(fitR.nn,dat.test)
val.nn<-med.reg(dat.test$Rented_Bike_Count,pred.nn)

pred.knn<-predict(fitR.knn,dat.test)
val.knn<-med.reg(dat.test$Rented_Bike_Count,pred.knn)

all.medR<-data.frame(Algoritmos=c("LM","DT","NN","KNN"),rbind(val.lm,val.dt,val.nn,val.knn))
all.medR%>% flextable()

##gráficos
plot.predlm<-ggplot(dat.test,aes(x=Rented_Bike_Count,y=pred.lm))+
  geom_point()+
  geom_line(aes(x=Rented_Bike_Count, y=Rented_Bike_Count),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("LM")

plot.preddt<-ggplot(dat.test,aes(x=Rented_Bike_Count,y=pred.dt))+
  geom_point()+
  geom_line(aes(x=Rented_Bike_Count, y=Rented_Bike_Count),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("DT")

plot.prednn<-ggplot(dat.test,aes(x=Rented_Bike_Count,y=pred.nn))+
  geom_point()+
  geom_line(aes(x=Rented_Bike_Count, y=Rented_Bike_Count),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("NN")

plot.predknn<-ggplot(dat.test,aes(x=Rented_Bike_Count,y=pred.knn))+
  geom_point()+
  geom_line(aes(x=Rented_Bike_Count, y=Rented_Bike_Count),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("KNN")

library(gridExtra) 
grid.arrange(plot.predlm, plot.preddt,plot.prednn,plot.predknn, ncol = 2)

##gráfico comparativo
all.predR<-data.frame(nobs=1:length(dat.test$Rented_Bike_Count),obs=dat.test$Rented_Bike_Count,LM=pred.lm,DT=pred.dt,NN=pred.nn,KNN=pred.knn)

ggplot(data=all.predR)+
  geom_line(aes(x=nobs,y=LM,color="LM"))+
  geom_line(aes(x=nobs,y=DT,color="DT"))+
  geom_line(aes(x=nobs,y=NN,color="NN"))+
  geom_line(aes(x=nobs,y=KNN,color="KNN"))+
  geom_line(aes(x=nobs,y=obs,color="Obs"))+
  labs(x = " ", y = "MEDV",)+
  scale_color_manual(name = " ",values = c(
    'Obs' = 'darkblue',
    'LM' = 'red',
    'DT' = 'pink',
    'NN' = 'orange',
    'KNN' = 'green')) +
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

