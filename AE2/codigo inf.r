library(dlookr)
library(visdat)
library(ggplot2)
library(funModeling)
library(flextable)
library(factoextra)
library(gridExtra)
library(MASS)
library(dplyr)
install.packages("factoextra")
getwd()
setwd("C:/Users/sipoj/Desktop/codigo informe 2")
datos <- read.csv("rentBike_data.csv",sep = ",",header = TRUE)

summary (datos)


datos2 = datos[,-c(1, 12, 13, 14)]


names(datos2)
#pca_data <- prcomp(data_ej2[,-7], scale = TRUE)
pca_df <- prcomp(datos2[,-1], center = TRUE, scale = TRUE) #scale=T estandariza
pca_df
VE <- pca_df$sdev^2
PVE <- VE / sum(VE)
round(PVE,2)

PVEp<- fviz_eig(pca_df,geom="bar")

# Cumulative PVE plot
PVEa <- qplot(c(1:length(pca_df$sdev)), cumsum(PVE)) + 
  geom_line() + 
  geom_hline(yintercept = 0.7,linetype = "dashed")+
  xlab("Principal Component") + 
  ylab("PVE-acumulado") + 
  ylim(0,1)

grid.arrange(PVEp, PVEa, ncol = 2)

fviz_pca_var(pca_df,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", 
                                                             "#FC4E07"),repel = TRUE)


#media
pca_df$center
# standard deviations
pca_df$scale

##vectores
pca_df$rotation
summary(pca_df)
plot(pca_df,type="l")
#PC5 explica mas del 70% de la proporcion acumulada

# PVE
library(factoextra)
PVEp<- fviz_eig(pca_df)#,geom="bar")
PVEp

# Cumulative PVE plot
PVEa <- qplot(c(1:ncol(datos2)), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE-acumulado") + 
  # ggtitle("Cumulative Scree Plot") +
  ylim(0,1)
PVEa

##resultados
# Eigenvalues
eig.val <- get_eigenvalue(pca_df)
eig.val

# resultado por variable
res.var <- get_pca_var(pca_df)
summary(pca_df)

fviz_pca_var(pca_df,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", 
                                                            "#FC4E07"),repel = TRUE,axes = c(2,1))

library(corrplot)
corrplot(res.var$cos2, is.corr=FALSE)

##Q-Q plot
install.packages("qqplotr")
library(qqplotr)

ggplot(data = datos[!is.na(datos$Rented_Bike_Count),], mapping = aes(sample = Rented_Bike_Count)) +
  stat_qq_line()+
  stat_qq_band()+
  stat_qq_point()+
  labs(x = "Q-Normal", y = "Q-Rented_Bike_Count (ppb)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

#respecto al grafico se procede con la regresion linear multiple

#Regresion linear multiple

#modelo1 

install.packages("caret")
library(caret)
#Regresión lineal múltiple

library(MASS)
data(Boston)
str(Boston)

#dividir datos de entrenamiento y prueba
set.seed(100)
selectrows <- sample(1:nrow(datos2),round(0.80*nrow(datos2)))
dat.train <- datos2[selectrows,]
dat.test <- datos2[-selectrows,]

head(dat.train)
mod1<-lm(Rented_Bike_Count~.,data = dat.train)
#inferencia de los coeficientes
coefic.lm<-summary(mod1)$coefficients
coefic.lm<-data.frame(Variables=row.names(coefic.lm),round(coefic.lm,4))
colnames(coefic.lm)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
coefic.lm %>% flextable()

r2.mod1<-summary(mod1)$adj.r.squared*100

confint(mod1)#intervalos de confianza
#gráfico Q-Q
plot(mod1,which = 2,col=c("blue"))

dat.train$resid<-mod1$residuals
shapiro.test(dat.train$resid)
dat.train$obs<-1:length(dat.train$resid)
summary(dat.train$resid)

ggplot(dat.train,aes(x=obs,y=resid))+
  geom_point()+
  geom_hline(yintercept = 0,linetype="dashed", color = "red")+
  labs(x = "No. Observaciones", y = "Residuos")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)
###Seleccionar el mejor modelo
mod2.lm <- step(mod1, direction="backward",trace = 0) #trace=1 muestra los pasos que realiza.
#inferencia de los coeficientes
coefic.lm2<-summary(mod2.lm)$coefficients
coefic.lm2<-data.frame(Variables=row.names(coefic.lm2),round(coefic.lm2,4))
colnames(coefic.lm2)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
coefic.lm2 %>% flextable()

r2.mod2<-summary(mod2.lm)$adj.r.squared*100

#predicción para ambos modelos con datos prueba
dat.test$pred1<-predict(mod1,dat.test)
cor1<-cor(dat.test$Rented_Bike_Count,dat.test$pred1)
# error mod
error1 <- dat.test$pred1 - dat.test$Rented_Bike_Count
#bias
bias1<-mean(error1)
#RMSE
rmse1<-sqrt(mean(error1^2))

plot.pred1<-ggplot(dat.test,aes(x=Rented_Bike_Count,y=pred1))+
  geom_point()+
  geom_line(aes(x=Rented_Bike_Count, y=Rented_Bike_Count),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("Modelo 1")
plot.pred1
#evaluar predicción 2
dat.test$pred2<-predict(mod2.lm,dat.test)
cor2<-cor(dat.test$Rented_Bike_Count,dat.test$pred2)

# error mod
error2 <- dat.test$pred2 - dat.test$Rented_Bike_Count
#bias
bias2<-mean(error2)
#RMSE
rmse2<-sqrt(mean(error2^2))

#comparación modelos
medidas.lm=data.frame(Medidas=c("R2.ajustado","COR","BIAS","RMSE"),
                      Modelo_1=c(r2.mod1,cor1,bias1,rmse1),
                      Modelo_2=c(r2.mod2,cor2,bias2,rmse2))

medidas.lm %>% flextable()

plot.pred2<-ggplot(dat.test,aes(x=Rented_Bike_Count,y=pred2))+
  geom_point()+
  geom_line(aes(x=Rented_Bike_Count, y=Rented_Bike_Count),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("Modelo 2")


plot(mod1,which = 2,col=c("blue"))
plot(mod2.lm,which = 2,col=c("blue"))
grid.arrange(plot.pred1, plot.pred2, ncol = 2)



set.seed(16)
training.samples <- createDataPartition(datos2$Rented_Bike_Count,p = 0.8, list = F)
train.data  <- datos2[training.samples, ]
test.data <- datos2[-training.samples, ]
mod.lr <- lm(Rented_Bike_Count ~., data = train.data)
mod.lr 
summary (mod.lr)

# Los mas significantes son hora, humedad, radiacion solar, lluvia 


#modelo 2

mod.lr2 <- lm(Rented_Bike_Count ~ Hour + Humidity + Solar_Radiation + Rainfall, data = train.data)
mod.lr2
summary(mod.lr2)

#Multiple R-squared:  0.3371,    Adjusted R-squared:  0.3366

#Reciduos
library(flextable)

hist(resid(mod.lr))
summary(resid(mod.lr))

datos2$resid<-mod.lr$residuals
shapiro.test(datos2$resid)
ks.test(datos2$Rented_Bike_Count,"pnorm",mean=mean(datos$Rented_Bike_Count,na.rm = T),sd=sd(datos2$Rented_Bike_Count,na.rm=T))
datos2$obs<-1:length(datos2$resid)
summary(datos2$resid)

hist(datos2$resid,main = "Histograma residuos Modelo 1")
shapiro.test(datos2$resid)
plot(model,which = 2,col=c("blue"))
plot(datos2$resid)
plot(datos2$resid2)
library(lmtest)
bptest(model)

df$resid2<-model2$residuals
df$obs<-1:length(df$resid2)

summary(df$resid2)
df$fit2<-model2$fitted.values

hist(df$resid2,main = "Histograma residuos Modelo 2")
shapiro.test(df$resid2)
plot(model2,which = 2,col=c("blue"))
library(lmtest)
bptest(model2)
bptest(model)
#predicción para ambos modelos con datos prueba

dat.test$pred1<-predict(mod1,dat.test)
cor1<-cor(dat.test$medv,dat.test$pred1)
# error mod

# error mod
error1 <- dat.test$pred1 - dat.test$medv

#bias
bias1<-mean(error1)
#RMSE
rmse1<-sqrt(mean(error1^2))




