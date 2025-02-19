library(VGAM)
library(ggplot2)
library(stats)

Dataset <- read.csv("C:/Users/5/OneDrive - Benemérita Universidad Autónoma de Puebla/Documents/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)


Causa=Dataset[,14]
Causa=Causa[-1]
Causa=as.numeric(Causa)

Causa=Causa[-which(Causa==4998)]
Causa=Causa[-which(Causa<4000)]
privacy_parameter <- 0.01



# Función para el Mecanismo Exponencial para la moda
exponential_mechanism_mode <- function(data, epsilon) {
  sensitivity <- as.numeric(max(Causa))-as.numeric(min(Causa))  # Sensibilidad de la consulta de moda
  unique_values <- unique(data)
  
  # Calcula la puntuación para cada valor único
  scores <- sapply(unique_values, function(value) {
    count <- -abs(sum(data-value))
    return(count)
  })
  
  # Calcula la distribución de probabilidad usando el Mecanismo Exponencial
  scaled_scores <- (epsilon * scores) / (2 * sensitivity)
  probabilities <- exp(scaled_scores) / sum(exp(scaled_scores))
  
  # Elije aleatoriamente la moda basada en las probabilidades
  
  return(data.frame(unique_values, probabilities))
#Devuelve en un dataframe el vector con los valores unicos y el vector de probs
  }


Res=exponential_mechanism_mode(Causa, privacy_parameter)


noisy_mode <- sample(Res[,1], size = 1, prob = Res[,2])



o=mean(as.numeric(Causa))


cat("Promedio real:", as.character(o), "\n")
cat("Promedio con ruido:", as.character(noisy_mode), "\n")

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



cat(" Promedio real:", as.character(round(mean(as.numeric(Causa)),2)), "\n",
    "Ejemplos de Promedio con ruido: ",
    as.character(round(NC[1],2)), ", ",
    as.character(round(NC[2],2)), "y ",
    as.character(round(NC[3],2))
    )





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




# Función de utilidad Mecanismo Exponencial para la moda
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


Intcoc= function(y){
  II=integrate(exp_scores,min(Causa), y)
  return(as.numeric(II[1]))
}


I=integrate(exp_scores,min(Causa),max(Causa))
integrate(exp_scores,4060,4070)
#Es complicado hacer esta integral, pues compare con 

xseq<-seq(min(Causa),max(Causa),len=1000)
lines(xseq,sapply(xseq,Intcoc))


#Funcion de densidad
Densidad<- function(obs){
  dens=exp_scores(obs)/as.numeric(I[1])
  return(dens)
}
  

plot(seq(4020,4080,length=200), Densidad(seq(4020,4080,length=200)), type="l")  



#Generando muestras de esta densidad

#Funcion de distribucion acumuada
cdf<-function(x) integrate(Densidad,min(Causa),x)[1]
#Inversa de la funcion
qdf<-function(x) optimize(function(z)(as.numeric(cdf(z))-x)^2,c(min(Causa),max(Causa)))$minimum

#Funcion que genera n muestras de la densidad
rdf<-function(n) sapply(runif(n),qdf)

x<-rdf(20) #Generamos una muestra aleatoria
hist(x,freq=F) #histograma de la muestra

#Agregamos la curva al histograma
xseq<-seq(-8,8,len=1000)
lines(xseq,sapply(xseq,df))



#Grafica de la funcion de densidad
plot(seq(4020,4080,length=200), Densidad(seq(4020,4080,length=200)), type="l")  

#Grafica de la funcion de distrbucion
plot(seq(4020,4080,length=20), as.numeric(cdf(seq(4020,4080,length=20))), type="l")  


#Grafica de una muestra aleatoria
SS=sapply(seq(4020,4080,length=20),cdf)
plot(seq(4020,4080,length=20),SS)




Lseq=seq(4050,4090,length=200)
Cer=seq(0,0,length=200)
for (i in 1:length(seq(4050,4090,length=200))) {
  Cer[i]=Intcoc(Lseq[i])
}

plot(Lseq, Cer, type="l")  



