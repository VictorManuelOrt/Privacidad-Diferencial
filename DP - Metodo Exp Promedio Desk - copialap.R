library(VGAM)
library(ggplot2)
library(stats)

Dataset <- read.csv("C:/Users/victo/OneDrive - Benem閞ita Universidad Aut髇oma de Puebla/Documents/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)

Causa=Dataset[,14]
Causa=Causa[-1]
Causa=as.numeric(Causa)




# Funci贸n para el Mecanismo Exponencial para la moda
exponential_mechanism_mode <- function(data, epsilon) {
  sensitivity <- as.numeric(max(Causa))-as.numeric(min(Causa))  # Sensibilidad de la consulta de moda
  unique_values <- unique(data)
  
  # Calcula la puntuaci贸n para cada valor 煤nico
  scores <- sapply(unique_values, function(value) {
    count <- -abs(sum(data-value))
    return(count)
  })
  
  # Calcula la distribuci贸n de probabilidad usando el Mecanismo Exponencial
  scaled_scores <- (epsilon * scores) / (2 * sensitivity)
  probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))
  
  # Elije aleatoriamente la moda basada en las probabilidades
  
  return(data.frame(unique_values, probabilities))
#Devuelve en un dataframe el vector con los valores unicos y el vector de probs
  }


privacy_parameter <- 0.01
Res=exponential_mechanism_mode(Causa, privacy_parameter)


noisy_mode <- sample(Res[,1], size = 1, prob = Res[,2])



o=mean(as.numeric(Causa))


cat("Promedio real:", as.character(o), "\n")
cat("Promedio con ruido:", as.character(noisy_mode), "\n")

#length(which(Causa==mode(Causa)))

#length(which(Causa==noisy_mode))

N=rep(0,1000)

for (i in 1:1000) {
  N[i] <- sample(Res[,1], size = 1, prob = Res[,2])
}


Nunique_values <- unique(N)
Nscores <- sapply(Nunique_values, function(value) {
  count <- sum(N == value)
  return(count)
})

NApar <- sapply(Nunique_values, function(value) {
  count <- sum(Causa == value)
  return(count)
})

INFo=data.frame(Nunique_values, Nscores, NApar)

INFo[order(INFo[,2], decreasing =TRUE),]

####################################
#########################
###################
###########
#####
##
#Usando composicion

Suma=sum(Causa)+rlaplace(1, 0, 2*(max(Causa)-min(Causa))/privacy_parameter)
Cantidad=length(Causa)+rlaplace(1, 0, 2/privacy_parameter)

Promediocompues=Suma/Cantidad
Promediocompues

NC=rep(0,1000)

for (i in 1:1000) {
  Suma=sum(Causa)+rlaplace(1, 0, 2*(max(Causa)-min(Causa))/privacy_parameter)
  Cantidad=length(Causa)+rlaplace(1, 0, 2/privacy_parameter)
  
  NC[i] <-Suma/Cantidad
}

hist(NC)


NCunique=unique(NC)

NCscores <- sapply(NCunique, function(value) {
  count <- sum(NC == value)
  return(count)
})


INFoo=data.frame(NCunique)

INFoo[order(NC, decreasing =TRUE),]
max(NC)-min(NC)
###################
###########
#####
##

#Funcion alternativa considerando el rango del promedio continuo




# Funci贸n de utilidad Mecanismo Exponencial para la 
Score <- function(obs) {
  C=0
  for (i in 1:length(Causa)) {
    C=C+Causa[i]-obs
  }
  utilidad=-abs(C)
  #utilidad <- -abs(sum(Causa-obs))
  
  
  return(utilidad)
}
#sensibilidad 
sensitivity <- as.numeric(max(Causa))-as.numeric(min(Causa))

#funcion de utilidad escalada
scaled_scores <- function(obs){
  scal=(privacy_parameter * Score(obs)) / (2 * sensitivity)
  return(scal)
}
#funcion de utilidad eponencial
exp_scores <- function(obs){
  ex=exp(scaled_scores(obs))
  return(ex)
}


I=integrate(exp_scores,min(Causa),max(Causa))
integrate(exp_scores,4036,4038)
#Es complicado hacer esta integral, pues compare con 

#Funcion de densidad
Densidad<- function(obs){
  dens=exp_scores(obs)/as.numeric(I[1])
  return(dens)
}
  



plot(seq(4020,4050,length=100), exp_scores(seq(4020,4050,length=100)), type="l")  
  
probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))

# Funci贸n de densidad
Score <- function(obs) {
  
  utilidad <- -abs(sum(datos-obs))
  
  
  return(utilidad)
}



# Ejemplo de uso
#set.seed(123)  # Para reproducibilidad
dataset <- c(5, 7, 5, 8, 7, 5, 6, 8, 5, 7)  # Conjunto de datos
privacy_parameter <- 0.0001  # Par谩metro de privacidad elegido

# Divulgar la moda con privacidad usando el Mecanismo Exponencial
noisy_mode <- exponential_mechanism_mode(EEst, privacy_parameter)


#funcion para calcular la moda
mode<- function(Data){
  return(names(which.max(table(Data))))
}

cat("Moda real:", as.character(mode(EEst)), "\n")
cat("Moda con ruido:", as.character(noisy_mode), "\n")


length(which(EEst==mode(EEst)))

length(which(EEst==noisy_mode))

N=rep(0,100)

for (i in 1:100) {
  N[i] <- exponential_mechanism_mode(EEst, privacy_parameter)
}


Nunique_values <- unique(N)
Nscores <- sapply(Nunique_values, function(value) {
  count <- sum(N == value)
  return(count)
})

NApar <- sapply(Nunique_values, function(value) {
  count <- sum(EEst == value)
  return(count)
})

INFo=data.frame(Nunique_values, Nscores, NApar)







