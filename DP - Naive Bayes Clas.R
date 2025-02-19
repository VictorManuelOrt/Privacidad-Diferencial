library(VGAM)
library(PerformanceAnalytics)

library(e1071)
library(caret)
library(mlbench)



# Cargar el conjunto de datos iris
data(iris)



#iris=data.frame(conjunto_de_datos_esep_hospla_2022$SHP40_H,conjunto_de_datos_esep_hospla_2022$SHP41_H)


DATA <- read.csv("diabetes.csv")
PPlot=DATA[,c(2,6,9)]
iris=PPlot



#En que columna se encuentran la categoria 
label_col <- 3


# Dividir los datos en conjuntos de entrenamiento y prueba
sample_index <- sample(1:nrow(iris), 0.6 * nrow(iris))

train_data <- iris[sample_index, ]
plot(train_data, col=train_data[,label_col]+1, cex=1.5, pch=19)


test_data <- iris[-sample_index, ]

# Calcular estadísticas (media y desviación estándar) por clase
stats <- list()


#En el siguiente ciclo, construrimos stats y guarda la media, sd en cada clase
for (label in unique(train_data[[label_col]])) {
  subset <- train_data[train_data[[label_col]] == label,]
  stats[[as.character(label)]] <- lapply(subset[, -label_col], function(x) c(mean = mean(x), sd = sd(x)))
}

#test_data[,c(2,3)]
#train_data[train_data[[label_col]] == 0,]
#train_data[train_data[[label_col]] == 0,2]
#train_data[train_data[[label_col]] == 0,c(2,3)]
#hist(train_data[train_data[[label_col]] == 0,2])
ks.test(train_data[train_data[[label_col]] == 0,1], "pnorm", mean = mean(train_data[train_data[[label_col]] == 0,1]), sd = sd(train_data[train_data[[label_col]] == 0,1]))
ks.test(train_data[train_data[[label_col]] == 1,1], "pnorm", mean = mean(train_data[train_data[[label_col]] == 1,1]), sd = sd(train_data[train_data[[label_col]] == 1,1]))
ks.test(train_data[train_data[[label_col]] == 0,2], "pnorm", mean = mean(train_data[train_data[[label_col]] == 0,2]), sd = sd(train_data[train_data[[label_col]] == 0,2]))
ks.test(train_data[train_data[[label_col]] == 1,2], "pnorm", mean = mean(train_data[train_data[[label_col]] == 1,2]), sd = sd(train_data[train_data[[label_col]] == 1,2]))



# Calcular probabilidades a priori ( p(c_i)  )
total <- nrow(train_data)
priors <- table(train_data[[label_col]]) / total
priors <- as.list(priors)

# Calcular la probabilidad de la distribución normal
gaussian_prob <- function(x, mean, sd) {
  if (sd == 0) {
    return(ifelse(x == mean, 1, 0))
  }
  return((1 / (sqrt(2 * pi) * sd)) * exp(-((x - mean)^2 / (2 * sd^2))))
}

# Predecir las etiquetas del conjunto de prueba
ProbaPost<- list()
predictions=list()

 for (i in 1:nrow(test_data)){
  x <- test_data[i, -label_col]
  
  # prob a posteiori (solo numerador) 
  #esto es (P(c_j|X_1,\dots X_n)=P(c_j)\prod P(X_i|c_j))
  posteriors <- list()
  
  for (label in names(stats)) {
    likelihood <- 1
    for (feature in names(stats[[label]])) {
      prob <- gaussian_prob(x[[feature]], stats[[label]][[feature]]["mean"], stats[[label]][[feature]]["sd"])
      likelihood <- likelihood * prob
    }
    posteriors[[label]] <- likelihood * priors[[label]]
  }
  ProbaPost[[i]]=posteriors
  predictions[i]=names(which.max(unlist(posteriors)))
}



# Verificar las predicciones y la precisión
print(ProbaPost)
print(predictions)
print(test_data[,label_col])

CategPred=substr(as.character(predictions), 1, nchar(as.character(predictions)) - 3)


accuracy <- sum(substr(as.character(predictions), 1, nchar(as.character(predictions)) - 3) == as.character(test_data[,label_col])) / nrow(test_data)

print(paste("Accuracy:", round(accuracy, 2)))

plot(test_data[,c(1,2)], col=test_data[,label_col]+2,ylab="IMC", xlab="Glucosa", cex=1.5, pch=19)
legend("topleft", legend = c("No Positivo", "Positivo"), 
       col = c(2, 3), pch = 19, cex = 1.2)

plot(test_data[,c(1,2)], col=as.numeric(CategPred)+2,ylab="IMC",xlab="Glucosa", cex=1.5, pch=19)
legend("topleft", legend = c("No Positivo", "Positivo"), 
       col = c(2, 3), pch = 19, cex = 1.2)

plot(test_data, col=(test_data[,label_col]==CategPred)+2, cex=1.5, pch=19)

#################
##########
######
###
##
#
#PARTE CON DP 

epsilon=0.5
n=10000


#Calculo de Max y Min
Upper=rep(0,ncol(iris)-1)
Lower=rep(0,ncol(iris)-1)


for (j in 1:(ncol(iris)-1)) {
  Upper[j]=max(iris[j])
  Lower[j]=min(iris[j])
}




Smean=rep(0,ncol(iris)-1)
Ssd=rep(0,ncol(iris)-1)
#Sensibilidades
for (j in 1:(ncol(iris)-1)) {
  Smean[j]=(Upper[j]-Lower[j])/(n+1)
  Ssd[j]=sqrt(n)*(Upper[j]-Lower[j])/(n+1)
}





#En el siguiente ciclo, construrimos stats y guarda la media, sd en cada clase
for (label in unique(train_data[[label_col]])) {
  subset <- train_data[train_data[[label_col]] == label,]
  stats[[as.character(label)]] <- lapply(subset[, -label_col], function(x) c(mean = mean(x), sd = sd(x)))
}


#Statsdp
statsdp <- stats


for (i in 1:length(stats)) {
  for (j in 1:(ncol(iris)-1)) {
    
      statsdp[[i]][[j]][["mean"]]=stats[[i]][[j]][["mean"]]+rlaplace(1,0,Smean[j]/epsilon)
      
      statsdp[[i]][[j]][["sd"]]=stats[[i]][[j]][["sd"]]+rlaplace(1,0,Ssd[j]/epsilon)
    
  }
  
}




# Calcular probabilidades a priori ( p(c_i)  )
total <- nrow(train_data)
priors <- table(train_data[[label_col]]) / total
priors <- as.list(priors)

# Calcular la probabilidad de la distribución normal
gaussian_prob <- function(x, mean, sd) {
  if (sd == 0) {
    return(ifelse(x == mean, 1, 0))
  }
  return((1 / (sqrt(2 * pi) * sd)) * exp(-((x - mean)^2 / (2 * sd^2))))
}

# Predecir las etiquetas del conjunto de prueba
ProbaPost<- list()
predictions=list()


for (i in 1:nrow(test_data)){
  x <- test_data[i, -label_col]
  
  # prob a posteiori (solo numerador) 
  #esto es (P(c_j|X_1,\dots X_n)=P(c_j)\prod P(X_i|c_j))
  posteriors <- list()
  
  for (label in names(statsdp)) {
    likelihood <- 1
    for (feature in names(statsdp[[label]])) {
      prob <- gaussian_prob(x[[feature]], statsdp[[label]][[feature]]["mean"], statsdp[[label]][[feature]]["sd"])
      likelihood <- likelihood * prob
    }
    posteriors[[label]] <- likelihood * priors[[label]]
  }
  ProbaPost[[i]]=posteriors
  predictions[i]=names(which.max(unlist(posteriors)))
}



# Verificar las predicciones y la precisión
print(ProbaPost)
print(predictions)
print(test_data[,label_col])


dpCategPred=substr(as.character(predictions), 1, nchar(as.character(predictions)) - 3)

dpaccuracy <- sum(substr(as.character(predictions), 1, nchar(as.character(predictions)) - 3) == as.character(test_data[,label_col])) / nrow(test_data)


print(paste("Accuracy:", round(accuracy, 2)))

print(paste("DP Accuracy:", round(dpaccuracy, 2)))


plot(test_data[,c(1,2)], col=test_data[,label_col]+2,ylab="IMC",xlab="Glucosa", cex=1.5, pch=19)
legend("topleft", legend = c("No Positivo", "Positivo"), 
       col = c(2, 3), pch = 19, cex = 1.2)

plot(test_data[,c(1,2)], col=as.numeric(dpCategPred)+2,ylab="IMC",xlab="Glucosa", cex=1.5, pch=19)
legend("topleft", legend = c("No Positivo", "Positivo"), 
       col = c(2, 3), pch = 19, cex = 1.2)







plot(test_data, col=(test_data[,label_col]==CategPred)+2, cex=1.5, pch=19)


plot(test_data, col=(test_data[,label_col]==dpCategPred)+2, cex=1.5, pch=19)



plot(test_data, col=test_data[,label_col]+1, cex=1.5, pch=19)









plot(test_data, col=CategPred, cex=1.5, pch=19)
plot(test_data, col=dpCategPred, cex=1.5, pch=19)

