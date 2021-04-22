setwd("C:/Users/Kevin Macario/Desktop/Uvg/9no Semestre/Mineria de Datos/HT-6.-Regresi-n-log-stica/house-prices-advanced-regression-techniques")
#datatest <- read.csv('test.csv')
#datatrain <- read.csv('train.csv')
#prices <- read.csv('sample_submission.csv')

library(caret)
library(dummies)
library(e1071)
#setwd("D:/AxelFolder/University/mineria_de_datos/HT-6.-Regresi-n-log-stica")


#setwd("C:/Users/LENOVO/Desktop/Clases/Minería de datos/Github/HT7SVM")

porcentaje<-0.8
set.seed(666)

datatest <- read.csv("house-prices-advanced-regression-techniques/test.csv")
prices <- read.csv("house-prices-advanced-regression-techniques/sample_submission.csv")
datatrain <- read.csv("house-prices-advanced-regression-techniques/train.csv")


datatestc = merge(x = datatest, y = prices, by = "Id")


datos = rbind(datatestc, datatrain)
colSums(is.na(datos))
datos$PoolQC<-NULL
datos$Fence <- NULL
datos$MiscFeature<-NULL
datos$FireplaceQu<-NULL
datos$Alley<-NULL
datos<- datos[complete.cases(datos), ]
mask <- unlist(lapply(datos, is.numeric))
datos_numericos <- datos[,mask]
cluster<-kmeans(datos_numericos ,3, iter.max = 200 )
datos$grupo <- cluster$cluster

summary(datos$SalePrice[datos$grupo==1])
summary(datos$SalePrice[datos$grupo==2])
summary(datos$SalePrice[datos$grupo==3])

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(120,160,180, max = 255, alpha = 80, names = "lt.hola")
break1 = floor(sqrt(nrow(datos[datos$grupo ==1,])))
break2 = floor(sqrt(nrow(datos[datos$grupo ==2,])))
break3 = floor(sqrt(nrow(datos[datos$grupo ==3,])))

histo1 <- hist(datos$SalePrice[datos$grupo ==1], breaks  = break1,plot = FALSE )
histo2 <- hist(datos$SalePrice[datos$grupo ==2],breaks = break2, plot = FALSE )
histo3 <- hist(datos$SalePrice[datos$grupo ==3],breaks = break3, plot = FALSE )
plot(histo1, c = c1, xlim = c(4000, 500000 ), ylim = c(0,200), main = "Histogramas 3 clústers", xlab ="Precios")
plot(histo2, c = c2,add = TRUE)
plot(histo3, c = c3,add = TRUE)

###################

library(caret)
library(dummies)

#datos<-cbind(datos,dummy(datos$grupo), verbose = T)
mask <- unlist(lapply(datos, is.numeric))
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,mask]
test<-datos[-corte,mask]

################# 
### Lineal 
modeloTuneado<-tune.svm(as.factor(grupo)~., data=train, cost=c(0.01,0.1,0.5,1,5), kernel="linear")
modeloTuneado$performances
modeloTuneado$best.model
t <- proc.time()
pred<-predict(modeloTuneado$best.model,newdata = test[,1:38], type = "response")
proc.time()-t
confusionMatrix(as.factor(test$grupo),as.factor(pred))

### Polynomial
modeloTuneado<-tune.svm(as.factor(grupo)~., data=train, coef0 =c(0.01,0.1,0.5,1,5),gamma =c(0.01,0.1,1,10), degree =c(3,4,5,6),   kernel="polynomial")
modeloTuneado$performances
modeloTuneado$best.model
t <- proc.time()
pred<-predict(modeloTuneado$best.model,newdata = test[,1:38], type = "response")
proc.time()-t
confusionMatrix(as.factor(test$grupo),as.factor(pred))

### sigmoid

modeloTuneadosigmoid<-tune.svm(as.factor(grupo)~., data=train, cost=c(0.01,0.5,1,1.5), gamma = c(0.01,1,1.5,11), coef0 = 0, kernel="sigmoid")

modeloTuneadosigmoid$performances
modeloTuneadosigmoid$best.model
timeSig <- proc.time()
pred<-predict(modeloTuneadosigmoid$best.model,newdata = test[,1:38], type = "response")
timeSig<- proc.time()-timeSig
timeSig
confusionMatrix(as.factor(test$grupo),as.factor(pred))


