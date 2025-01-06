library(gridExtra)
library(grid)


#deuda_publica_2024_01 <- read.csv("C:/Users/5/Downloads/deuda_publica_2024_01.csv")

deuda_publica_2024_01 <- read.csv("~/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/DeudaPCDMX/deuda_publica_2024_01.csv")


#Seleccion de variables

DATA=deuda_publica_2024_01[,c(7,11:22)]

DATANUM=DATA[,c (1:4, 6:9)]

#Limpieza de datos vacios

DATANUM <- DATANUM[rowSums(DATANUM == "**") == 0 & rowSums(DATANUM == "") == 0 & complete.cases(DATANUM), ]

DATANUM1=DATANUM[,1]

# EstandarizaciÃ³n (Z-score)
DATANUM <- as.data.frame(lapply(DATANUM[,-1], function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}))

# Ver el data frame estandarizado
head(DATANUM)

DATANUM=cbind(DATANUM1,DATANUM)

NamesBancos=unique(DATANUM[,1])


Etiquetas=list()


#for (i in 1:length(NamesBancos)) {
#  Etiquetas[[i]]=which(DATANUM[,1]==NamesBancos[[i]])
#}

DatosPorBanco=list()


for (i in 1:length(NamesBancos)) {
  DatosPorBanco[[i]]=DATANUM[which(DATANUM[,1]==NamesBancos[[i]]),]
}


#Buscamos si hay correlacion

#correlation_matrix <- cor(DatosPorBanco[[2]][,-1], use = "complete.obs")

correlation_matrix <- cor(DATANUM[,-1], use = "complete.obs")

print(correlation_matrix)




##############
############
##########
##########
#########
##########

#PPA


# Funcion de error cuadr?tico medio
mse <- function(omega, X, Y) {
  n <- length(Y)
  error <- Y - X %*% omega
  return(sum(error^2) / n)
}

# Proximal Point Algorithm
ppa <- function(X, Y, omega) {
  
  # Inicializaci?n
  n_local <- nrow(X)
  p_local <- ncol(X)
  
  # Minimizamos mse + termino de proximidad
  XtX <- t(X) %*% X
  XtY <- t(X) %*% Y
  term <- XtX + (n_local * mu / 2) * diag(p_local)
  rhs <- XtY + (n_local * mu / 2) * omega
  
  # Actualiza omega usando la formula derivada
  omega_new <- solve(term) %*% rhs
  
  return(omega_new)
}





#Iniciamos con una base de datos #D

#Usando base de datos mtcars

library(dplyr)



#DAtos

X <- as.matrix(DATANUM[,c(2,3,5,6)])  # Variables independientes
Y <- DATANUM[,8]  # Variable dependiente


X_list=list() 
for (j in 1:length(NamesBancos)) {
  X_list[[j]]=as.matrix(DatosPorBanco[[j]][,c(2,3,5,6)])
}

Y_list =list()
for (j in 1:length(NamesBancos)) {
  Y_list[[j]]=as.matrix(DatosPorBanco[[j]][,8])
}


set.seed(NULL)



#Tamano de la base de datos
n <- nrow(X)
#Cantidad de atributos o caracterÃ­sticas de la base
p <- ncol(X)


n_clients=length(X_list)
mu = 20

epsilon=5

C=1
m=n/n_clients
DeltaSU=2*C/(m*epsilon)

SigmaU=DeltaSU

DP=0
#Algorithm 1: Noising before Aggregation FL 

#InicializaciÃ³n:

#NÃºmero de iteraciones

T=100

#Guardalas actualizaciones globales de Omega (como vectores columna)
Acz<- matrix(0, nrow = p, ncol = T)


#Guaradara historial de errores
Error<- rep(0,T)


# Inicializa omega en 0
omega <- Acz[,1]


ParLocales=matrix(0, nrow = p, ncol = length(X_list))



for (l in 1:T) {
  omega_global <- Acz[,l]
  
  for (m in 1:length(X_list)) {
    LocalData=as.matrix(X_list[[m]])
    rownames(LocalData) <- NULL
    colnames(LocalData) <- NULL
    
    ResultadoLocal <- ppa(LocalData, Y_list[[m]], omega_global)
    
    if (DP==1){
    ResultadoLocal=ResultadoLocal+
      rnorm(length(ResultadoLocal), mean = 0, sd = SigmaU)
    }
    
    ParLocales[,m]=ResultadoLocal
    
  }
  
  #Promedio de parametros
  Promedio=apply(ParLocales[ ,1:5], 1, mean, na.rm = TRUE)
  
  #Actualizamos omega global
  if (l==T){
    print(Promedio)
  } else{
    Acz[,l+1]<- Promedio
  }
  
  
  Data=as.matrix(X)
  rownames(Data) <- NULL
  colnames(Data) <- NULL
  Error[l]=mse(Promedio,X,as.vector(Y))
  
}


Error
plot(Error, type="l", xlab  =paste0("Iteración"), 
     main = expression("Error "),
     #ylim = c(0,130)
     )



if (DP==1){
  plot(Error, type="l", xlab  =paste0("Número de Iteración"), ylab="Error" ,
       main = paste0(expression("Evolución del Error DP")))
  legend("topright", # PosiciÃ³n de la leyenda
         legend=c(paste("Epsilon =", epsilon), 
                   #paste("# de iteraciones =", IT),
                  paste("Umbral de recorte=", C),
                  paste("Coeficiente de penalización=", mu)
         ), # Texto de la leyenda
         #col="black", # Color de la lÃ­nea
         #lty=1, # Tipo de lÃ­nea
         bty="n") # Eliminar el marco de la leyenda
} else{
  plot(Error, type="l", xlab  =paste0("Número de Iteración"), ylab="Error" ,
       main = paste0(expression("Evolución del Error")))
  legend("topright", # PosiciÃ³n de la leyenda
         legend=c(#paste("Epsilon =", epsilon), 
         #   paste("# de iteraciones =", IT),
           paste("Umbral de recorte=", C),
           paste("Coeficiente de penalización=", mu)
         ),
           #paste("sigma=", sigma)
         #), # Texto de la leyenda
         #col="black", # Color de la lÃ­nea
         #lty=1, # Tipo de lÃ­nea
         bty="n") # Eliminar el marco de la leyenda
           
}








#Graficando diferentes valores de epsilon
Valepsilon=c(0.4,0.9,1.5,2,10,100)
CostosEpsilon=list()
for (LL in 1:length(Valepsilon) ) {
  #Tamano de la base de datos
  n <- nrow(X)
  #Cantidad de atributos o caracterÃ­sticas de la base
  p <- ncol(X)
  n_clients=length(X_list)
  epsilon=Valepsilon[LL] 
  mu = 20
  C=1
  m=n/n_clients
  DeltaSU=2*C/(m*epsilon)
  SigmaU=DeltaSU
  DP=1
  #Algorithm 1: Noising before Aggregation FL 
  
  #InicializaciÃ³n:
  
  #NÃºmero de iteraciones
  
  T=100
  
  #Guardalas actualizaciones globales de Omega (como vectores columna)
  Acz<- matrix(0, nrow = p, ncol = T)
  
  
  #Guaradara historial de errores
  Error<- rep(0,T)
  
  
  # Inicializa omega en 0
  omega <- Acz[,1]
  
  
  ParLocales=matrix(0, nrow = p, ncol = length(X_list))
  
  
  
  for (l in 1:T) {
    omega_global <- Acz[,l]
    
    for (m in 1:length(X_list)) {
      LocalData=as.matrix(X_list[[m]])
      rownames(LocalData) <- NULL
      colnames(LocalData) <- NULL
      
      ResultadoLocal <- ppa(LocalData, Y_list[[m]], omega_global)
      
      if (DP==1){
        ResultadoLocal=ResultadoLocal+
          rnorm(length(ResultadoLocal), mean = 0, sd = SigmaU)
      }
      
      ParLocales[,m]=ResultadoLocal
      
    }
    
    #Promedio de parametros
    Promedio=apply(ParLocales[ ,1:5], 1, mean, na.rm = TRUE)
    
    #Actualizamos omega global
    if (l==T){
      print(Promedio)
    } else{
      Acz[,l+1]<- Promedio
    }
    
    
    Data=as.matrix(X)
    rownames(Data) <- NULL
    colnames(Data) <- NULL
    Error[l]=mse(Promedio,X,as.vector(Y))
    
  }
  CostosEpsilon[[LL]]=Error
}



# Configura el grÃ¡fico
plot(CostosEpsilon[[1]], type = "l",lwd=2, col = 1, ylim = range(unlist(CostosEpsilon)),
     xlab = "Iteración", ylab = "Costos", main = "Costos variando Epsilón")

# AÃ±ade los demÃ¡s vectores
for (i in 2:length(CostosEpsilon)) {
  lines(CostosEpsilon[[i]], type = "l", col = i,lwd=2)
}

# Agrega la leyenda
legend("topright", title = "Valor de epsilon", legend = Valepsilon, col = 1:length(CostosEpsilon),
       pch = NA, lty = 1, lwd=2,cex = 0.7)




#Validacion cruzada
mu_val=c(0.001, 0.01, 0.1, 1,2,5,10,20,100,200,500, 1000,2000)

#desde aca para valid Cruzada
NITCrossVal=5
ErorCrossValidation=matrix(rep(0,length(mu_val)*NITCrossVal), ncol = NITCrossVal, nrow = length(mu_val))

######
#####

for (cross in 1:NITCrossVal) {
  
contadormiu=1

for (miu in mu_val) {
  mu = miu
  #Guardalas actualizaciones globales de Omega (como vectores columna)
  Acz<- matrix(0, nrow = p, ncol = T)
  #Guaradara historial de errores
  Error<- rep(0,T)
  # Inicializa omega en 0
  omega <- Acz[,1]
  ParLocales=matrix(0, nrow = p, ncol = length(X_list))
  for (l in 1:T) {
    omega_global <- Acz[,l]
    
    for (m in 1:length(X_list)) {
      
      LocalData=as.matrix(X_list[[m]])
      rownames(LocalData) <- NULL
      colnames(LocalData) <- NULL
      
      ResultadoLocal <- ppa(LocalData, Y_list[[m]], omega_global)
      
      if (DP==1){
        ResultadoLocal=ResultadoLocal+
          rnorm(length(ResultadoLocal), mean = 0, sd = SigmaU)
      }
      
      ParLocales[,m]=ResultadoLocal
    }

    
    #Promedio de parametros
    Promedio=apply(ParLocales[ ,1:5], 1, mean, na.rm = TRUE)
    
    #Actualizamos omega global
    if (l==T){
      print(Promedio)
    } else{
      Acz[,l+1]<- Promedio
    }
    Data=as.matrix(X)
    rownames(Data) <- NULL
    colnames(Data) <- NULL
    Error[l]=mse(Promedio,X,as.vector(Y))
    
  }
  
  #Error
  #plot(Error, type="l", xlab  =paste0("IteraciÃ³n"), 
  #    main = paste0("Error ", "( con mu=", mu, ")" ))
  
  
  #print(paste0("Error final:", Error[length(Error)], "con mu=",mu ))
  
  #Paso Cross Validation
  ErorCrossValidation[contadormiu,cross]=Error[length(Error)]
  contadormiu=contadormiu+1
  
}

}




rowMeans(ErorCrossValidation)

Validiacion_cruzada=data.frame(Coeficiente_de_Penalización=mu_val, Error_Promedio_Obtenido=rowMeans(ErorCrossValidation)
)


print(Validiacion_cruzada)



library(png)

# Crear una imagen PNG donde dibujaremos la tabla
png("tabla_completa.png", width = 800, height = 600)

# Iniciar el gráfico para poder usar 'text'
plot.new()

# Definir las coordenadas y márgenes para dibujar el texto
par(mar = c(5, 5, 2, 2))  # Ajustamos márgenes

# Dibujar los encabezados de la tabla
text(0.1, 0.9, "Coeficiente_de_Penalización", cex = 1.0)
text(0.6, 0.9, "Error_Promedio_Obtenido", cex = 1.0)

# Dibujar los datos del dataframe en las filas (ajustamos la posición)
for (i in 1:nrow(Validiacion_cruzada)) {
  # Dibujar las celdas de la primera columna (Nombre)
  text(0.1, 0.8 - (i * 0.06), Validiacion_cruzada$Coeficiente_de_Penalización[i], cex = 1)
  # Dibujar las celdas de la segunda columna (Edad)
  text(0.6, 0.8 - (i * 0.06), Validiacion_cruzada$Error_Promedio_Obtenido[i], cex = 1)
}

# Finalizar el dispositivo gráfico y guardar la imagen
dev.off()


