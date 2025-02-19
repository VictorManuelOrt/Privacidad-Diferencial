#deuda_publica_2024_01 <- read.csv("C:/Users/5/Downloads/deuda_publica_2024_01.csv")

deuda_publica_2024_01 <- read.csv("~/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/DeudaPCDMX/deuda_publica_2024_01.csv")


#Seleccion de variables

DATA=deuda_publica_2024_01[,c(7,11:22)]

DATANUM=DATA[,c (1:4, 6:9)]

#Limpieza de datos vacios

DATANUM <- DATANUM[rowSums(DATANUM == "**") == 0 & rowSums(DATANUM == "") == 0 & complete.cases(DATANUM), ]

DATANUM1=DATANUM[,1]

# Estandarización (Z-score)
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



# Paquetes necesarios
library(MASS)
library(dplyr)

# Función para recortar gradientes
clip_gradient <- function(grad, C) {
  norm_grad <- sqrt(sum(grad^2))
  if (norm_grad > C) {
    return(C * grad / norm_grad)
  } else {
    return(grad)
  }
}

# Función para añadir ruido gaussiano
add_noise <- function(grad, sigma, C, B) {
  noise <- rnorm(length(grad), mean = 0, sd = sigma * C / (B*epsilon))
  return(grad + noise)
}

# Función de costo: Error Cuadrático Medio (MSE)
mse_cost <- function(y_true, y_pred) {
  return(mean((y_true - y_pred)^2))
}








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






# Parámetros de entrada
eta <- 0.01  # Tasa de aprendizaje
C <- 1  # Umbral de recorte
epsilon=1
sigma <- 1/epsilon  # Escala de ruido
B <- 6  # Tamaño del lote
T <- 500  # Número de iteraciones
DP=0

# Parámetros
n_clients <- length(NamesBancos) #Cantidad de Clientes
n_features <- ncol(X) #Cantidad de caracteristicas

# Inicialización de los parámetros de los 7 clientes (cada uno con un vector de 10 parámetros)
w_t <- matrix(rep(0, n_clients * n_features), nrow = n_clients)  # Inicializando 7 clientes, cada uno con 10 parámetros (aquí solo ejemplos)


# Matriz de pesos (W) que relaciona a los 7 clientes
W <- matrix(c(
  1/4, 1/4, 1/4, 0, 0, 1/4, 0,
  1/4, 1/2, 1/4, 0, 0, 0, 0,
  1/4, 1/4, 1/4, 1/4, 0, 0, 0,
  0, 0, 1/4, 1/4, 1/4, 0, 1/4,
  0, 0, 0, 1/4, 3/4, 0, 0,
  1/4, 0, 0, 0, 0, 5/12, 1/3,
  0, 0, 0, 1/4, 0, 1/3, 5/12
), nrow = 7, byrow = TRUE)





# Para almacenar el error promedio global a lo largo del tiempo
error_promedio_global <- numeric(T)

# Algoritmo DP-D-SGD
for (t in 1:T) {
  # 1. Muestrear al azar un lote de tamaño B de cada base de datos local
  lote=list()
  lotey=list()
  
  for (client in 1:n_clients) {
    index=sample(1:nrow(X_list[[client]]),B)
    lote[[client]]=X_list[[client]][index,]
    lotey[[client]]=Y_list[[client]][index,]
    
  }
  
  # 2. Construir el conjunto de datos global
  D_global <- X # Concatenamos todos los datos locales de los 7 clientes
  
  # 3. Calcular los gradientes y aplicar recorte
  gradientes_recortados <- list()
  for (client in 1:n_clients) {
    gradientes_cliente <- list()
    for (l in 1:B) {
      g=2*lote[[client]][l,]*(sum(lote[[client]][l,]*w_t[client,])-lotey[[client]][l])
      norma_g <- sqrt(sum(g^2))  # Norma L2 del gradiente
      g_recortado <- g / max(1, norma_g / C)  # Recorte del gradiente
      gradientes_cliente[[l]] <- g_recortado
    }
    gradientes_recortados[[client]] <- gradientes_cliente
  }
  
  # 4. Añadir ruido (ruido gaussiano con varianza proporcional a C)
  ruido <- lapply(1:n_clients, function(k) rnorm(length(w_t[k,]), mean = 0, sd = sigma * C))  # Ruido para cada cliente
  
  gradientes_ruidosos <- list()
  for (client in 1:n_clients) {
    gradiente_promedio <- Reduce("+", gradientes_recortados[[client]]) / B
    if (DP==1){
    gradiente_ruidoso <- gradiente_promedio + ruido[[client]]}else{
      gradiente_ruidoso <- gradiente_promedio
    }
    gradientes_ruidosos[[client]] <- gradiente_ruidoso
  }
  
  # 5. Actualización de los parámetros locales
  w_t_half <- w_t - eta * do.call(rbind, gradientes_ruidosos)  # Actualización de los parámetros
  
  # 6. Promedio ponderado de los parámetros (utilizando la matriz W)
  w_t_next <- W %*% w_t_half  # Promedio ponderado de los parámetros
  
  # Actualización de los parámetros del nodo
  w_t <- w_t_next
  
  # 7. Calcular el error promedio global con respecto al conjunto global
  errores_clientes <- numeric(n_clients)
  for (client in 1:n_clients) {
    # Para cada cliente, calculamos el error (MSE) utilizando el conjunto de datos global
    errores_clientes[client] <- mean((Y - X%*%w_t[1,])^2)  # Calculamos el error de cada cliente con respecto al conjunto global
  }
  
  # El error promedio global es el promedio de los errores de todos los clientes
  error_promedio_global[t] <- mean(errores_clientes)
}

# Graficar el error promedio global a lo largo del tiempo
plot(1:T, error_promedio_global, type = "l",  
     xlab = "N�mero de Iteraci�n", ylab = "Error Promedio", main = "Evoluci�n del Error Promedio")




if (DP==1){
  plot(error_promedio_global, type="l", xlab  =paste0("N�mero de Iteraci�n"), ylab="Error Promedio" ,
       main = paste0(expression("Evoluci�n del Error Promedio DP")))
  legend("topright", # Posición de la leyenda
         legend=c(paste("Epsilon =", epsilon), 
                  # paste("# de iteraciones =", IT),
                  paste("umbral de recorte=", C),
                  paste("tama�o de lote=", B),
                  #paste("sigma=", sigma),
                  paste("tasa de aprendizaje", eta)
         ), # Texto de la leyenda
         #col="black", # Color de la línea
         #lty=1, # Tipo de línea
         bty="n") # Eliminar el marco de la leyenda
} else{
  plot(error_promedio_global, type="l", xlab  =paste0("N�mero de Iteraci�n"), ylab="Error Promedio" ,
       main = paste0(expression("Evoluci�n del Error Promedio")))
  legend("topright", # Posición de la leyenda
         legend=c(#paste("Epsilon =", epsilon), 
           # paste("# de iteraciones =", IT),
           #paste("umbral de recorte=", C),
           paste("tama�o de lote=", B),
           paste("tasa de aprendizaje", eta)
           #paste("sigma=", sigma)
         ), # Texto de la leyenda
         #col="black", # Color de la línea
         #lty=1, # Tipo de línea
         bty="n") # Eliminar el marco de la leyenda
  
  
}











######
#####
###



#Graficando diferentes valores de epsilon
Valepsilon=c(0.4,0.9,1.5,2,10,100)
CostosEpsilon=list()
for (LL in 1:length(Valepsilon) ) {
  # Parámetros de entrada
  eta <- 0.01  # Tasa de aprendizaje
  C <- 1  # Umbral de recorte
  epsilon=Valepsilon[[LL]]
  sigma <- 1/epsilon  # Escala de ruido
  B <- 6  # Tamaño del lote
  T <- 500  # Número de iteraciones
  DP=1
  
  # Parámetros
  n_clients <- length(NamesBancos) #Cantidad de Clientes
  n_features <- ncol(X) #Cantidad de caracteristicas
  
  # Inicialización de los parámetros de los 7 clientes (cada uno con un vector de 10 parámetros)
  w_t <- matrix(rep(0, n_clients * n_features), nrow = n_clients)  # Inicializando 7 clientes, cada uno con 10 parámetros (aquí solo ejemplos)
  
  

  
  
  
  # Para almacenar el error promedio global a lo largo del tiempo
  error_promedio_global <- numeric(T)
  
  # Algoritmo DP-D-SGD
  for (t in 1:T) {
    # 1. Muestrear al azar un lote de tamaño B de cada base de datos local
    lote=list()
    lotey=list()
    
    for (client in 1:n_clients) {
      index=sample(1:nrow(X_list[[client]]),B)
      lote[[client]]=X_list[[client]][index,]
      lotey[[client]]=Y_list[[client]][index,]
      
    }
    
    # 2. Construir el conjunto de datos global
    D_global <- X # Concatenamos todos los datos locales de los 7 clientes
    
    # 3. Calcular los gradientes y aplicar recorte
    gradientes_recortados <- list()
    for (client in 1:n_clients) {
      gradientes_cliente <- list()
      for (l in 1:B) {
        g=2*lote[[client]][l,]*(sum(lote[[client]][l,]*w_t[client,])-lotey[[client]][l])
        norma_g <- sqrt(sum(g^2))  # Norma L2 del gradiente
        g_recortado <- g / max(1, norma_g / C)  # Recorte del gradiente
        gradientes_cliente[[l]] <- g_recortado
      }
      gradientes_recortados[[client]] <- gradientes_cliente
    }
    
    # 4. Añadir ruido (ruido gaussiano con varianza proporcional a C)
    ruido <- lapply(1:n_clients, function(k) rnorm(length(w_t[k,]), mean = 0, sd = sigma * C))  # Ruido para cada cliente
    
    gradientes_ruidosos <- list()
    for (client in 1:n_clients) {
      gradiente_promedio <- Reduce("+", gradientes_recortados[[client]]) / B
      if (DP==1){
        gradiente_ruidoso <- gradiente_promedio + ruido[[client]]}else{
          gradiente_ruidoso <- gradiente_promedio
        }
      gradientes_ruidosos[[client]] <- gradiente_ruidoso
    }
    
    # 5. Actualización de los parámetros locales
    w_t_half <- w_t - eta * do.call(rbind, gradientes_ruidosos)  # Actualización de los parámetros
    
    # 6. Promedio ponderado de los parámetros (utilizando la matriz W)
    w_t_next <- W %*% w_t_half  # Promedio ponderado de los parámetros
    
    # Actualización de los parámetros del nodo
    w_t <- w_t_next
    
    # 7. Calcular el error promedio global con respecto al conjunto global
    errores_clientes <- numeric(n_clients)
    for (client in 1:n_clients) {
      # Para cada cliente, calculamos el error (MSE) utilizando el conjunto de datos global
      errores_clientes[client] <- mean((Y - X%*%w_t[1,])^2)  # Calculamos el error de cada cliente con respecto al conjunto global
    }
    
    # El error promedio global es el promedio de los errores de todos los clientes
    error_promedio_global[t] <- mean(errores_clientes)
  }
  CostosEpsilon[[LL]]=error_promedio_global
}



# Configura el gráfico
plot(CostosEpsilon[[1]], type = "l",lwd=2, col = 1, ylim = range(unlist(CostosEpsilon)),
     xlab = "Iteraci�n", ylab = "Costos", main = "Costos variando Epsilon")

# Añade los demás vectores
for (i in 2:length(CostosEpsilon)) {
  lines(CostosEpsilon[[i]], type = "l", col = i,lwd=2)
}

# Agrega la leyenda
legend("topright", title = "Valor de epsilon", legend = Valepsilon, col = 1:length(CostosEpsilon),
       pch = NA, lty = 1, lwd=2,cex = 0.7)











#######
######
#####

#Validacion cruzada
alpha_val <- c( 0.01,0.05,0.1, 0.15, 0.18, 0.2,0.22,  0.25,0.27,  0.3, 0.33)

#desde aca para valid Cruzada
NITCrossVal=5
ErorCrossValidation=matrix(rep(0,length(alpha_val)*NITCrossVal), ncol = NITCrossVal, nrow = length(alpha_val))

######
#####

for (cross in 1:NITCrossVal) {
  
  contadormiu=1
  
  for (alphaa in alpha_val) {
    alpha = alphaa
    
    # Construir la matriz W
    W <- matrix(c(
      1 - 3 * alpha,  alpha,         alpha,         0,             0,             alpha,         0,
      alpha,         1 - 2 * alpha,  alpha,         0,             0,             0,             0,
      alpha,         alpha,         1 - 3 * alpha,  alpha,         0,             0,             0,
      0,             0,             alpha,         1 - 3 * alpha,  alpha,         0,             alpha,
      0,             0,             0,             alpha,         1 - alpha,      0,             0,
      alpha,         0,             0,             0,             0,             1 - 2 * alpha,  alpha,
      0,             0,             0,             alpha,         0,             alpha,         1 - 2 * alpha
    ), byrow = TRUE, nrow = 7)

    w_t <- matrix(rep(0, n_clients * n_features), nrow = n_clients)  # Inicializando 7 clientes, cada uno con 10 parámetros (aquí solo ejemplos)
    
    
    # Para almacenar el error promedio global a lo largo del tiempo
    error_promedio_global <- numeric(T)
    
    # Algoritmo DP-D-SGD
    for (t in 1:T) {
      # 1. Muestrear al azar un lote de tamaño B de cada base de datos local
      lote=list()
      lotey=list()
      
      for (client in 1:n_clients) {
        index=sample(1:nrow(X_list[[client]]),B)
        lote[[client]]=X_list[[client]][index,]
        lotey[[client]]=Y_list[[client]][index,]
        
      }
      
      # 2. Construir el conjunto de datos global
      D_global <- X # Concatenamos todos los datos locales de los 7 clientes
      
      # 3. Calcular los gradientes y aplicar recorte
      gradientes_recortados <- list()
      for (client in 1:n_clients) {
        gradientes_cliente <- list()
        for (l in 1:B) {
          g=2*lote[[client]][l,]*(sum(lote[[client]][l,]*w_t[client,])-lotey[[client]][l])
          norma_g <- sqrt(sum(g^2))  # Norma L2 del gradiente
          g_recortado <- g / max(1, norma_g / C)  # Recorte del gradiente
          gradientes_cliente[[l]] <- g_recortado
        }
        gradientes_recortados[[client]] <- gradientes_cliente
      }
      
      # 4. Añadir ruido (ruido gaussiano con varianza proporcional a C)
      ruido <- lapply(1:n_clients, function(k) rnorm(length(w_t[k,]), mean = 0, sd = sigma * C))  # Ruido para cada cliente
      
      gradientes_ruidosos <- list()
      for (client in 1:n_clients) {
        gradiente_promedio <- Reduce("+", gradientes_recortados[[client]]) / B
        if (DP==1){
          gradiente_ruidoso <- gradiente_promedio + ruido[[client]]}else{
            gradiente_ruidoso <- gradiente_promedio
          }
        gradientes_ruidosos[[client]] <- gradiente_ruidoso
      }
      
      # 5. Actualización de los parámetros locales
      w_t_half <- w_t - eta * do.call(rbind, gradientes_ruidosos)  # Actualización de los parámetros
      
      # 6. Promedio ponderado de los parámetros (utilizando la matriz W)
      w_t_next <- W %*% w_t_half  # Promedio ponderado de los parámetros
      
      # Actualización de los parámetros del nodo
      w_t <- w_t_next
      
      # 7. Calcular el error promedio global con respecto al conjunto global
      errores_clientes <- numeric(n_clients)
      for (client in 1:n_clients) {
        # Para cada cliente, calculamos el error (MSE) utilizando el conjunto de datos global
        errores_clientes[client] <- mean((Y - X%*%w_t[1,])^2)  # Calculamos el error de cada cliente con respecto al conjunto global
      }
      
      # El error promedio global es el promedio de los errores de todos los clientes
      error_promedio_global[t] <- mean(errores_clientes)
    }
    
    #Error
    #plot(Error, type="l", xlab  =paste0("Iteración"), 
    #    main = paste0("Error ", "( con mu=", mu, ")" ))
    
    
    #print(paste0("Error final:", Error[length(Error)], "con mu=",mu ))
    
    #Paso Cross Validation
    ErorCrossValidation[contadormiu,cross]=error_promedio_global[length(error_promedio_global)]
    contadormiu=contadormiu+1
    
  }
  
}




rowMeans(ErorCrossValidation)

Validiacion_cruzada=data.frame( Tasa_De_Difusion=alpha_val, Error_Promedio_Obtenido=rowMeans(ErorCrossValidation)
)


print(Validiacion_cruzada)



library(png)

# Crear una imagen PNG donde dibujaremos la tabla
png("tabla_completaGaf.png", width = 800, height = 600)

# Iniciar el gr�fico para poder usar 'text'
plot.new()

# Definir las coordenadas y m�rgenes para dibujar el texto
par(mar = c(5, 5, 2, 2))  # Ajustamos m�rgenes

# Dibujar los encabezados de la tabla
text(0.1, 0.9, "Tasa de difusi�n", cex = 1.0)
text(0.6, 0.9, "Error_Promedio_Obtenido", cex = 1.0)

# Dibujar los datos del dataframe en las filas (ajustamos la posici�n)
for (i in 1:nrow(Validiacion_cruzada)) {
  # Dibujar las celdas de la primera columna (Nombre)
  text(0.1, 0.8 - (i * 0.06), Validiacion_cruzada$Tasa_De_Difusion[i], cex = 1)
  # Dibujar las celdas de la segunda columna (Edad)
  text(0.6, 0.8 - (i * 0.06), Validiacion_cruzada$Error_Promedio_Obtenido[i], cex = 1)
}

# Finalizar el dispositivo gr�fico y guardar la imagen
dev.off()






###########
###
###
###
###
###
###
###
###
###

#deuda_publica_2024_01 <- read.csv("C:/Users/5/Downloads/deuda_publica_2024_01.csv")

deuda_publica_2024_01 <- read.csv("~/TESIS Ms Cs Mt/Differential Privacy Codigos y bases de datos/DeudaPCDMX/deuda_publica_2024_01.csv")


#Seleccion de variables

DATA=deuda_publica_2024_01[,c(7,11:22)]

DATANUM=DATA[,c (1:4, 6:9)]

#Limpieza de datos vacios

DATANUM <- DATANUM[rowSums(DATANUM == "**") == 0 & rowSums(DATANUM == "") == 0 & complete.cases(DATANUM), ]

DATANUM1=DATANUM[,1]

# Estandarización (Z-score)
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



# Paquetes necesarios
library(MASS)
library(dplyr)

# Función para recortar gradientes
clip_gradient <- function(grad, C) {
  norm_grad <- sqrt(sum(grad^2))
  if (norm_grad > C) {
    return(C * grad / norm_grad)
  } else {
    return(grad)
  }
}

# Función para añadir ruido gaussiano
add_noise <- function(grad, sigma, C, B) {
  noise <- rnorm(length(grad), mean = 0, sd = sigma * C / (B*epsilon))
  return(grad + noise)
}

# Función de costo: Error Cuadrático Medio (MSE)
mse_cost <- function(y_true, y_pred) {
  return(mean((y_true - y_pred)^2))
}








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






# Parámetros de entrada
eta <- 0.01  # Tasa de aprendizaje
C <- 1  # Umbral de recorte
epsilon=1
sigma <- 1/epsilon  # Escala de ruido
B <- 6  # Tamaño del lote
T <- 500  # Número de iteraciones
DP=1

# Parámetros
n_clients <- length(NamesBancos) #Cantidad de Clientes
n_features <- ncol(X) #Cantidad de caracteristicas

# Inicialización de los parámetros de los 7 clientes (cada uno con un vector de 10 parámetros)
w_t <- matrix(rep(0, n_clients * n_features), nrow = n_clients)  # Inicializando 7 clientes, cada uno con 10 parámetros (aquí solo ejemplos)


# Matriz de pesos (W) que relaciona a los 7 clientes
W <- matrix(c(
  0,         1/3,       1/3,       0,         0,         1/3,       0,
  1/3,       1/3,       1/3,       0,         0,         0,         0,
  1/3,       1/3,       0,         1/3,       0,         0,         0,
  0,         0,         1/3,       0,         1/3,       0,         1/3,
  0,         0,         0,         1/3,       2/3,       0,         0,
  1/3,       0,         0,         0,         0,         1/3,       1/3,
  0,         0,         0,         1/3,       0,         1/3,       1/3
), byrow = TRUE, nrow = 7)





# Para almacenar el error promedio global a lo largo del tiempo
error_promedio_global <- numeric(T)

# Algoritmo DP-D-SGD
for (t in 1:T) {
  # 1. Muestrear al azar un lote de tamaño B de cada base de datos local
  lote=list()
  lotey=list()
  
  for (client in 1:n_clients) {
    index=sample(1:nrow(X_list[[client]]),B)
    lote[[client]]=X_list[[client]][index,]
    lotey[[client]]=Y_list[[client]][index,]
    
  }
  
  # 2. Construir el conjunto de datos global
  D_global <- X # Concatenamos todos los datos locales de los 7 clientes
  
  # 3. Calcular los gradientes y aplicar recorte
  gradientes_recortados <- list()
  for (client in 1:n_clients) {
    gradientes_cliente <- list()
    for (l in 1:B) {
      g=2*lote[[client]][l,]*(sum(lote[[client]][l,]*w_t[client,])-lotey[[client]][l])
      norma_g <- sqrt(sum(g^2))  # Norma L2 del gradiente
      g_recortado <- g / max(1, norma_g / C)  # Recorte del gradiente
      gradientes_cliente[[l]] <- g_recortado
    }
    gradientes_recortados[[client]] <- gradientes_cliente
  }
  
  # 4. Añadir ruido (ruido gaussiano con varianza proporcional a C)
  ruido <- lapply(1:n_clients, function(k) rnorm(length(w_t[k,]), mean = 0, sd = sigma * C))  # Ruido para cada cliente
  
  gradientes_ruidosos <- list()
  for (client in 1:n_clients) {
    gradiente_promedio <- Reduce("+", gradientes_recortados[[client]]) / B
    if (DP==1){
      gradiente_ruidoso <- gradiente_promedio + ruido[[client]]}else{
        gradiente_ruidoso <- gradiente_promedio
      }
    gradientes_ruidosos[[client]] <- gradiente_ruidoso
  }
  
  # 5. Actualización de los parámetros locales
  w_t_half <- w_t - eta * do.call(rbind, gradientes_ruidosos)  # Actualización de los parámetros
  
  # 6. Promedio ponderado de los parámetros (utilizando la matriz W)
  w_t_next <- W %*% w_t_half  # Promedio ponderado de los parámetros
  
  # Actualización de los parámetros del nodo
  w_t <- w_t_next
  
  # 7. Calcular el error promedio global con respecto al conjunto global
  errores_clientes <- numeric(n_clients)
  for (client in 1:n_clients) {
    # Para cada cliente, calculamos el error (MSE) utilizando el conjunto de datos global
    errores_clientes[client] <- mean((Y - X%*%w_t[1,])^2)  # Calculamos el error de cada cliente con respecto al conjunto global
  }
  
  # El error promedio global es el promedio de los errores de todos los clientes
  error_promedio_global[t] <- mean(errores_clientes)
}

# Graficar el error promedio global a lo largo del tiempo
plot(1:T, error_promedio_global, type = "l",  
     xlab = "N�mero de Iteraci�n", ylab = "Error Promedio", main = "Evoluci�n del Error Promedio")




if (DP==1){
  plot(error_promedio_global, type="l", xlab  =paste0("N�mero de Iteraci�n"), ylab="Error Promedio" ,
       main = paste0(expression("Evoluci�n del Error Promedio DP")))
  legend("topright", # Posición de la leyenda
         legend=c(paste("Epsilon =", epsilon), 
                  # paste("# de iteraciones =", IT),
                  paste("umbral de recorte=", C),
                  paste("tama�o de lote=", B),
                  #paste("sigma=", sigma),
                  paste("tasa de aprendizaje", eta)
         ), # Texto de la leyenda
         #col="black", # Color de la línea
         #lty=1, # Tipo de línea
         bty="n") # Eliminar el marco de la leyenda
} else{
  plot(error_promedio_global, type="l", xlab  =paste0("N�mero de Iteraci�n"), ylab="Error Promedio" ,
       main = paste0(expression("Evoluci�n del Error Promedio")))
  legend("topright", # Posición de la leyenda
         legend=c(#paste("Epsilon =", epsilon), 
           # paste("# de iteraciones =", IT),
           #paste("umbral de recorte=", C),
           paste("tama�o de lote=", B),
           paste("tasa de aprendizaje", eta)
           #paste("sigma=", sigma)
         ), # Texto de la leyenda
         #col="black", # Color de la línea
         #lty=1, # Tipo de línea
         bty="n") # Eliminar el marco de la leyenda
  
  
}











######
#####
###



#Graficando diferentes valores de epsilon
Valepsilon=c(0.4,0.9,1.5,2,10,100)
CostosEpsilon=list()
for (LL in 1:length(Valepsilon) ) {
  # Parámetros de entrada
  eta <- 0.01  # Tasa de aprendizaje
  C <- 1  # Umbral de recorte
  epsilon=Valepsilon[[LL]]
  sigma <- 1/epsilon  # Escala de ruido
  B <- 6  # Tamaño del lote
  T <- 500  # Número de iteraciones
  DP=1
  
  # Parámetros
  n_clients <- length(NamesBancos) #Cantidad de Clientes
  n_features <- ncol(X) #Cantidad de caracteristicas
  
  # Inicialización de los parámetros de los 7 clientes (cada uno con un vector de 10 parámetros)
  w_t <- matrix(rep(0, n_clients * n_features), nrow = n_clients)  # Inicializando 7 clientes, cada uno con 10 parámetros (aquí solo ejemplos)
  
  
  
  
  
  
  # Para almacenar el error promedio global a lo largo del tiempo
  error_promedio_global <- numeric(T)
  
  # Algoritmo DP-D-SGD
  for (t in 1:T) {
    # 1. Muestrear al azar un lote de tamaño B de cada base de datos local
    lote=list()
    lotey=list()
    
    for (client in 1:n_clients) {
      index=sample(1:nrow(X_list[[client]]),B)
      lote[[client]]=X_list[[client]][index,]
      lotey[[client]]=Y_list[[client]][index,]
      
    }
    
    # 2. Construir el conjunto de datos global
    D_global <- X # Concatenamos todos los datos locales de los 7 clientes
    
    # 3. Calcular los gradientes y aplicar recorte
    gradientes_recortados <- list()
    for (client in 1:n_clients) {
      gradientes_cliente <- list()
      for (l in 1:B) {
        g=2*lote[[client]][l,]*(sum(lote[[client]][l,]*w_t[client,])-lotey[[client]][l])
        norma_g <- sqrt(sum(g^2))  # Norma L2 del gradiente
        g_recortado <- g / max(1, norma_g / C)  # Recorte del gradiente
        gradientes_cliente[[l]] <- g_recortado
      }
      gradientes_recortados[[client]] <- gradientes_cliente
    }
    
    # 4. Añadir ruido (ruido gaussiano con varianza proporcional a C)
    ruido <- lapply(1:n_clients, function(k) rnorm(length(w_t[k,]), mean = 0, sd = sigma * C))  # Ruido para cada cliente
    
    gradientes_ruidosos <- list()
    for (client in 1:n_clients) {
      gradiente_promedio <- Reduce("+", gradientes_recortados[[client]]) / B
      if (DP==1){
        gradiente_ruidoso <- gradiente_promedio + ruido[[client]]}else{
          gradiente_ruidoso <- gradiente_promedio
        }
      gradientes_ruidosos[[client]] <- gradiente_ruidoso
    }
    
    # 5. Actualización de los parámetros locales
    w_t_half <- w_t - eta * do.call(rbind, gradientes_ruidosos)  # Actualización de los parámetros
    
    # 6. Promedio ponderado de los parámetros (utilizando la matriz W)
    w_t_next <- W %*% w_t_half  # Promedio ponderado de los parámetros
    
    # Actualización de los parámetros del nodo
    w_t <- w_t_next
    
    # 7. Calcular el error promedio global con respecto al conjunto global
    errores_clientes <- numeric(n_clients)
    for (client in 1:n_clients) {
      # Para cada cliente, calculamos el error (MSE) utilizando el conjunto de datos global
      errores_clientes[client] <- mean((Y - X%*%w_t[1,])^2)  # Calculamos el error de cada cliente con respecto al conjunto global
    }
    
    # El error promedio global es el promedio de los errores de todos los clientes
    error_promedio_global[t] <- mean(errores_clientes)
  }
  CostosEpsilon[[LL]]=error_promedio_global
}



# Configura el gráfico
plot(CostosEpsilon[[1]], type = "l",lwd=2, col = 1, ylim = range(unlist(CostosEpsilon)),
     xlab = "Iteraci�n", ylab = "Costos", main = "Costos variando Epsilon")

# Añade los demás vectores
for (i in 2:length(CostosEpsilon)) {
  lines(CostosEpsilon[[i]], type = "l", col = i,lwd=2)
}

# Agrega la leyenda
legend("topright", title = "Valor de epsilon", legend = Valepsilon, col = 1:length(CostosEpsilon),
       pch = NA, lty = 1, lwd=2,cex = 0.7)








