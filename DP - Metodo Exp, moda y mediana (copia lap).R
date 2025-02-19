library(VGAM)
library(ggplot2)
library(stats)

Dataset <- read.csv("C:/Users/victo/OneDrive - Benem閞ita Universidad Aut髇oma de Puebla/Documents/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)

Causa=Dataset[,11]





# Funci贸n para el Mecanismo Exponencial para la moda
exponential_mechanism_mode <- function(data, epsilon) {
  sensitivity <- 1  # Sensibilidad de la consulta de moda
  unique_values <- unique(data)
  
  # Calcula la puntuaci贸n para cada valor 煤nico
  scores <- sapply(unique_values, function(value) {
    count <- sum(data == value)
    return(count)
  })
  
  # Calcula la distribuci贸n de probabilidad usando el Mecanismo Exponencial
  scaled_scores <- (epsilon * scores) / (2 * sensitivity)
  probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))
  
  # Elije aleatoriamente la moda basada en las probabilidades
  
  return(data.frame(unique_values, probabilities))
#Devuelve en un dataframe el vector con los valores unicos y el vector de probs
  }


privacy_parameter <- 0.0001
Res=exponential_mechanism_mode(Causa, privacy_parameter)


noisy_mode <- sample(Res[,1], size = 1, prob = Res[,2])



#funcion para calcular la moda
mode<- function(Data){
  return(names(which.max(table(Data))))
}

cat("Moda real:", as.character(mode(Causa)), "\n")
cat("Moda con ruido:", as.character(noisy_mode), "\n")


length(which(Causa==mode(Causa)))

length(which(Causa==noisy_mode))

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

INFo[order(INFo[,3], decreasing =TRUE),]

####################################
#########################
###################
###########
#####
##
#



Dataset <- read.csv("C:/Users/5/OneDrive - Benem茅rita Universidad Aut贸noma de Puebla/Documents/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)

EEst=Dataset[,12]








# Funci贸n para el Mecanismo Exponencial para la moda
exponential_mechanism_mode <- function(data, epsilon) {
  sensitivity <- 1  # Sensibilidad de la consulta de moda
  unique_values <- unique(data)
  
  # Calcula la puntuaci贸n para cada valor 煤nico
  scores <- sapply(unique_values, function(value) {
    count <- sum(data == value)
    return(count)
  })
  
  # Calcula la distribuci贸n de probabilidad usando el Mecanismo Exponencial
  scaled_scores <- (epsilon * scores) / (2 * sensitivity)
  probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))
  
  # Elije aleatoriamente la moda basada en las probabilidades
  noisy_mode <- sample(unique_values, size = 1, prob = probabilities)
  
  return(noisy_mode)
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


































#C:\Users\victo\OneDrive - Benem閞ita Universidad Aut髇oma de Puebla\Documents\TESIS Ms Cs Mt\Differential Privacy Codigos y bases de datos\conjunto_de_datos_defunciones_registradas_2021_csv\conjunto_de_datos


Dataset <- read.csv("C:/Users/victo/OneDrive - Benem閞ita Universidad Aut髇oma de Puebla/Documents/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)

Causa=Dataset[,11]













# Funci髇 para el Mecanismo Exponencial para la moda
exponential_mechanism_mode <- function(data, epsilon) {
  sensitivity <- 1  # Sensibilidad de la consulta de moda
  unique_values <- unique(data)
  
  # Calcula la puntuaci髇 para cada valor 鷑ico
  scores <- sapply(unique_values, function(value) {
    count <- sum(data == value)
    return(count)
  })
  
  # Calcula la distribuci髇 de probabilidad usando el Mecanismo Exponencial
  scaled_scores <- (epsilon * scores) / (2 * sensitivity)
  probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))
  
  # Elije aleatoriamente la moda basada en las probabilidades
  
  return(data.frame(unique_values, probabilities))
  #Devuelve en un dataframe el vector con los valores unicos y el vector de probs
}


privacy_parameter <- 0.0001
Res=exponential_mechanism_mode(Causa, privacy_parameter)


noisy_mode <- sample(Res[,1], size = 1, prob = Res[,2])



#funcion para calcular la moda
mode<- function(Data){
  return(names(which.max(table(Data))))
}

cat("Moda real:", as.character(mode(Causa)), "\n")
cat("Moda con ruido:", as.character(noisy_mode), "\n")


length(which(Causa==mode(Causa)))

length(which(Causa==noisy_mode))

N=rep(0,10000)

for (i in 1:10000) {
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

INFo[order(INFo[,3], decreasing =TRUE),]

####################################
#########################
###################
###########
#####
#
















# Funci髇 para el Mecanismo Exponencial para la moda
exponential_mechanism_mode <- function(data, epsilon) {
  sensitivity <- 1  # Sensibilidad de la consulta de moda
  unique_values <- unique(data)
  
  # Calcula la puntuaci髇 para cada valor 鷑ico
  scores <- sapply(unique_values, function(value) {
    count <- sum(data == value)
    return(count)
  })
  
  # Calcula la distribuci髇 de probabilidad usando el Mecanismo Exponencial
  scaled_scores <- (epsilon * scores) / (2 * sensitivity)
  probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))
  
  # Elije aleatoriamente la moda basada en las probabilidades
  
  return(data.frame(unique_values, probabilities))
  #Devuelve en un dataframe el vector con los valores unicos y el vector de probs
}


privacy_parameter <- 0.0001
Res=exponential_mechanism_mode(Causa, privacy_parameter)


noisy_mode <- sample(Res[,1], size = 1, prob = Res[,2])



#funcion para calcular la moda
mode<- function(Data){
  return(names(which.max(table(Data))))
}

cat("Moda real:", as.character(mode(Causa)), "\n")
cat("Moda con ruido:", as.character(noisy_mode), "\n")


length(which(Causa==mode(Causa)))

length(which(Causa==noisy_mode))

N=rep(0,10000)

for (i in 1:10000) {
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

INFo[order(INFo[,3], decreasing =TRUE),]

####################################
#########################
###################
###########
#####
#

