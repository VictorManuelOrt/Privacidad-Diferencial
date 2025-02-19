####
####
####
# Algoritmo por Punto Proximal (PPA)

par(mfrow = c(3, 3))




# Funcion de error cuadr?tico medio
mse <- function(omega, X, y) {
  n <- length(y)
  error <- y - X %*% omega
  return(sum(error^2) / n)
}

# Proximal Point Algorithm
ppa <- function(X, y, omega) {
  
  # Inicializaci?n
  n_local <- nrow(X)
  p_local <- ncol(X)
  
  # Minimizamos mse + termino de proximidad
  XtX <- t(X) %*% X
  XtY <- t(X) %*% y
  term <- XtX + (n_local * mu / 2) * diag(p_local)
  rhs <- XtY + (n_local * mu / 2) * omega
  
  # Actualiza omega usando la formula derivada
  omega_new <- solve(term) %*% rhs
  
  return(omega_new)
}





#Iniciamos con una base de datos #D

#Usando base de datos mtcars

library(dplyr)

# Preparar el conjunto de datos mtcars
data(mtcars)
mtcars=mtcars[1:30,]
#mtcars=mtcars[,-1]

X <- as.matrix(mtcars[,-1])  # Variables independientes
y <- mtcars[,1]  # Variable dependiente




#desde aca para valid Cruzada
ErorCrossValidation=list()


#Suponemos que el conjunto de datos está particionado en partes iguales
# y guardado en una lista

# Dividir el conjunto de datos en clientes (aquí, simplemente dividimos en 5 partes)
set.seed(NULL)
n_clients <- 5
split_indices <- split(sample(1:nrow(X)), 1:n_clients)
X_list <- lapply(split_indices, function(idx) X[idx, ])
y_list <- lapply(split_indices, function(idx) y[idx])



#Tamano de la base de datos
n <- nrow(mtcars)
#Cantidad de atributos o características de la base
p <- ncol(mtcars)-1





#Algorithm 1: Noising before Aggregation FL 

#Inicialización:

#Número de iteraciones

T=1000

#Guardalas actualizaciones globales de Omega (como vectores columna)
Acz<- matrix(0, nrow = p, ncol = T)


#Guaradara historial de errores
Error<- rep(0,T)


# Inicializa omega en 0
omega <- Acz[,1]


ParLocales=matrix(0, nrow = p, ncol = length(X_list))


mu = 100

for (l in 1:T) {
  omega_global <- Acz[,l]
  
  for (m in 1:length(X_list)) {
    LocalData=as.matrix(X_list[[m]])
    rownames(LocalData) <- NULL
    colnames(LocalData) <- NULL
    
    ResultadoLocal <- ppa(LocalData, y_list[[m]], omega_global)
    
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
  
  
  Data=as.matrix(mtcars[,2:11])
  rownames(Data) <- NULL
  colnames(Data) <- NULL
  Error[l]=mse(Promedio,Data,as.vector(mtcars$mpg))
  
}

par(mfrow = c(1, 1))

Error
plot(Error, type="l", xlab  =paste0("Iteración"), 
     main = expression("Error "), ylim = c(0,130))


par(mfrow = c(3, 4))

mu_val=c(0.01, 0.1, 1,2,5,10,20,50,100,200,500, 1000,2000)

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
      
      ResultadoLocal <- ppa(LocalData, y_list[[m]], omega_global)
      
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
    
    
    Data=as.matrix(mtcars[,2:11])
    rownames(Data) <- NULL
    colnames(Data) <- NULL
    Error[l]=mse(Promedio,Data,as.vector(mtcars$mpg))
    
  }
  
  Error
  #plot(Error, type="l", xlab  =paste0("Iteración"), 
   #    main = paste0("Error ", "( con mu=", mu, ")" ))
  print(paste0("Error final:", Error[length(Error)], "con mu=",mu ))
  
  #Paso Cross Validation
  ErorCrossValidation=c(ErorCrossValidation,Error[length(Error)])

  
}





CrossVal=as.matrix(matrix(unlist(ErorCrossValidation), nrow = 13))
rowMeans(CrossVal)

#,ylim = c(0,50)





























#Calculo del error

Data=as.matrix(mtcars[,2:11])
rownames(Data) <- NULL
colnames(Data) <- NULL
mse(Promedio,Data,as.vector(mtcars$mpg))





############
########
###
#Parte con DP

epsilon=1000

C=5
m=n/n_clients
DeltaSU=2*C/(m*epsilon)

SigmaU=DeltaSU


L=T

if (T<= L*sqrt(n)){
  SigmaD=0
} else {
  SigmaD=2*C*sqrt(T^2-T^2*n)/(m*n*epsilon)
}

set.seed(NULL)


#Guardalas actualizaciones globales de Omega (como vectores columna)
Acz<- matrix(0, nrow = p, ncol = T)


#Guaradara historial de errores
Error<- rep(0,T)


# Inicializa omega en 0
omega <- Acz[,1]


for (l in 1:T) {
  omega_global <- Acz[,l]
  
  for (m in 1:length(X_list)) {
    LocalData=as.matrix(X_list[[m]])
    rownames(LocalData) <- NULL
    colnames(LocalData) <- NULL
    
    ResultadoLocal <- ppa(LocalData, y_list[[m]], omega_global)
    
    #Actualizacion local con ruido
    
    ResultadoLocal=ResultadoLocal
      +rnorm(length(ResultadoLocal), mean = 0, sd = SigmaU)
    
    ParLocales[,m]=ResultadoLocal+rnorm(length(ResultadoLocal), mean = 0, sd = SigmaU)
    
  }
  
  #Promedio de parametros
  Promedio=apply(ParLocales[ ,1:5], 1, mean, na.rm = TRUE)
  
  #Actualizacion global con ruido
  Promedio=Promedio+rnorm(length(Promedio), mean = 0, sd = SigmaD)
  
  #Actualizamos omega global
  if (l==T){
    print(Promedio)
  } else{
    Acz[,l+1]<- Promedio
  }
  
  
  Data=as.matrix(mtcars[,2:11])
  rownames(Data) <- NULL
  colnames(Data) <- NULL
  Error[l]=mse(Promedio,Data,as.vector(mtcars$mpg))
  
}

Error
plot(Error, type="l", xlab  =paste0("Iteración"), 
     main = expression("Error ("*epsilon* "=1000)"))





par(mfrow = c(3, 4))

mu_val=c(0.01, 0.1, 1,2,5,10,20,50,100,200,500, 1000)

for (miu in mu_val) {
  mu = miu
  
  
  
  #Guardalas actualizaciones globales de Omega (como vectores columna)
  Acz<- matrix(0, nrow = p, ncol = T)
  
  
  #Guaradara historial de errores
  Error<- rep(0,T)
  
  
  # Inicializa omega en 0
  omega <- Acz[,1]
  
  
  for (l in 1:T) {
    omega_global <- Acz[,l]
    
    for (m in 1:length(X_list)) {
      LocalData=as.matrix(X_list[[m]])
      rownames(LocalData) <- NULL
      colnames(LocalData) <- NULL
      
      ResultadoLocal <- ppa(LocalData, y_list[[m]], omega_global)
      
      #Actualizacion local con ruido
      
      ResultadoLocal=ResultadoLocal
      +rnorm(length(ResultadoLocal), mean = 0, sd = SigmaU)
      
      ParLocales[,m]=ResultadoLocal+rnorm(length(ResultadoLocal), mean = 0, sd = SigmaU)
      
    }
    
    #Promedio de parametros
    Promedio=apply(ParLocales[ ,1:5], 1, mean, na.rm = TRUE)
    
    #Actualizacion global con ruido
    Promedio=Promedio+rnorm(length(Promedio), mean = 0, sd = SigmaD)
    
    #Actualizamos omega global
    if (l==T){
      print(Promedio)
    } else{
      Acz[,l+1]<- Promedio
    }
    
    
    Data=as.matrix(mtcars[,2:11])
    rownames(Data) <- NULL
    colnames(Data) <- NULL
    Error[l]=mse(Promedio,Data,as.vector(mtcars$mpg))
    
  }
  
  Error
  plot(Error, type="l", xlab  =paste0("Iteración"), 
       main = paste0(expression("Error ("*epsilon* "=1000)"*"(mu=)"),mu),ylim = c(0,50))
  
  
  
}

























y=as.vector(mtcars$mpg)
X=Data
XtX <- t(X) %*% X
XtY <- t(X) %*% y
term <- XtX
rhs <- XtY 

# Actualiza omega usando la formula derivada
omega_new <- solve(term) %*% rhs



#BAse completa
t(omega_new)
mse(omega_new,Data,as.vector(mtcars$mpg))


#Usando PPA
Promedio
mse(Promedio,Data,as.vector(mtcars$mpg))


#Usando SDG
t(w_global)
mse(w_global,Data,as.vector(mtcars$mpg))





