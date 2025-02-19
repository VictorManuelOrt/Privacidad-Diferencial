Dataset <- read.csv("C:/Users/5/OneDrive - Benemérita Universidad Autónoma de Puebla/Documents/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)

Causa=Dataset[,14]


Causa=as.numeric(Causa[-1])



Causa=Causa[which(Causa>=4000& Causa<= 4200)]

# Función para el Mecanismo Exponencial para la moda
exponential_mechanism_median <- function(data, epsilon) {
  sensitivity <- 1  # Sensibilidad de la consulta de moda
  unique_values <- unique(data)
  conteosmen<- sapply(unique_values, function(value) {
    count <- sum(data <= value)
    return(count)
  })
  
  
  # Calcula la puntuación para cada valor único
  scores <- sapply(conteosmen, function(value) {
    count <- -abs(value-length(data)/2)
    return(count)
  })
  
  # Calcula la distribución de probabilidad usando el Mecanismo Exponencial
  scaled_scores <- (epsilon * scores) / (2 * sensitivity)
  probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))
  
  # Elije aleatoriamente la moda basada en las probabilidades
  
  return(data.frame(unique_values, probabilities))
  #Devuelve en un dataframe el vector con los valores unicos y el vector de probs
}




privacy_parameter <- 0.0001
Res=exponential_mechanism_median(Causa, privacy_parameter)


noisy_median <- sample(Res[,1], size = 1, prob = Res[,2])
real_median <- median(Causa)



cat("Mediana real:", as.character(median(real_median)), "\n")
cat("Mediana con ruido:", as.character(noisy_median), "\n")




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
  count <- sum(Causa <= value)
  return(count)
})

INFo=data.frame(Nunique_values, Nscores, NApar, NApar/length(Causa))

INFo[order(INFo[,2], decreasing =TRUE),]
