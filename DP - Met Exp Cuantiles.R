library(tibble)
library(ggplot2)
library(VGAM)



Dataset <- read.csv("conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)

Causa=Dataset[2:1000,14]


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




privacy_parameter <- 0.1
Res=exponential_mechanism_median(Causa, privacy_parameter)


noisy_median <- sample(Res[,1], size = 1, prob = Res[,2])
real_median <- median(Causa)



cat("Mediana real:", as.character(median(real_median)), "\n")
cat("Mediana con ruido:", as.character(noisy_median), "\n")




N=rep(0,100000)

for (i in 1:100000) {
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







#Generalizando a cuantiles




# Función para el Mecanismo Exponencial para la moda
exponential_mechanism_median <- function(data, epsilon, qq ) {
  sensitivity <- 1  # Sensibilidad de la consulta de moda
  unique_values <- unique(data)
  conteosmen<- sapply(unique_values, function(value) {
    count <- sum(data <= value)
    return(count)
  })
  
  
  # Calcula la puntuación para cada valor único
  scores <- sapply(conteosmen, function(value) {
    count <- -abs(value-length(data)*qq)
    return(count)
  })
  
  # Calcula la distribución de probabilidad usando el Mecanismo Exponencial
  scaled_scores <- (epsilon * scores) / (2 * sensitivity)
  probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))
  
  # Elije aleatoriamente la moda basada en las probabilidades
  
  return(data.frame(unique_values, probabilities))
  #Devuelve en un dataframe el vector con los valores unicos y el vector de probs
}




qq=.75
Res=exponential_mechanism_median(Causa, privacy_parameter, qq)


noisy_cuantil <- sample(Res[,1], size = 1, prob = Res[,2])
real_cuantil <- quantile(Causa, c(qq), type = 1)



cat("Cuantil real:", as.character(median(real_cuantil)), "\n")
cat("Cuantil con ruido:", as.character(noisy_cuantil), "\n")




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





DPQuantil =function(qq){
  Res=exponential_mechanism_median(Causa, privacy_parameter, qq)
  
  
  noisy_cuantil <- sample(Res[,1], size = 1, prob = Res[,2])
  return(noisy_cuantil)
}







df=data.frame(x = 1,
y0 = DPQuantil(0),
y25 = DPQuantil(.25),
y50 = DPQuantil(.50),
 y75 = DPQuantil(.75),
y100 = DPQuantil(1))

ggplot(df, aes(x)) +
geom_boxplot(aes(ymin = y0, 
lower = y25, 
middle = y50, 
upper = y75, 
ymax = y100),
stat = "identity")
legend(x = "topleft" ,        # Posición
       legend = c("No Positivos",
                  "positivos"), # Textos de la leyenda
       fill = c(2, 3))     








#Diagrama de cuantiles con DP

df=data.frame(
              Min = DPQuantil(0),
              Q1 = DPQuantil(.25),
              Q2 = DPQuantil(.50),
              Q3 = DPQuantil(.75),
              Max = DPQuantil(1))


Mydataq <- t(df) %>%
  as.data.frame() %>% 
  setNames("value") %>% 
  rownames_to_column(var = "quantile")
Mydataq$quantile=paste(c( "Min",   " Q1","  Q2", "   Q3" ,"   Max" )
                       ,"=",Mydataq$value)

Mydataq %>% 
  ggplot() + 
  geom_hline(aes(yintercept = value, color = quantile)) + 
  geom_jitter(data = tibble(x = "", y = Causa), 
              aes(x = x, y = y))+ 
  labs(title ="Cuantiles con DP")+
  theme(
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"))+
  labs(y="Edad",x="")






#Diagrama de cuantiles sin DP






Mydataq <- c(min(Causa),quantile(Causa, probs = c(0.25, 0.5, 0.75)),max(Causa)) %>%
  as.data.frame() %>% 
  setNames("value") %>% 
  rownames_to_column(var = "quantile")
Mydataq$quantile=paste(c( "Min",   " Q1","  Q2", "   Q3" ,"   Max" )
                       ,"=",Mydataq$value)



Mydataq %>% 
  ggplot() + 
  geom_hline(aes(yintercept = value, color = quantile)) + 
  geom_jitter(data = tibble(x = "", y = Causa), 
              aes(x = x, y = y))+ 
  labs(title ="Cuantiles")+
  theme(
    plot.title = element_text(hjust = 0.5,size = 14, face = "bold"))+
  labs(y="Edad",x="")


