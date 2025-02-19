# Paquetes necesarios
library(MASS)

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

# Función ClientUpdate
ClientUpdate <- function(X, y, w, learning_rate, epochs, batch_size, C, sigma) {
  n <- nrow(X)
  for (epoch in 1:epochs) {
    # Barajar los datos
    indices <- sample(1:n)
    X <- X[indices, ]
    y <- y[indices]
    
    for (i in seq(1, n, by = batch_size)) {
      end_idx <- min(i + batch_size - 1, n)
      X_batch <- X[i:end_idx, ]
      y_batch <- y[i:end_idx]
      
      # Predicción
      y_pred <- X_batch %*% w
      
      # Gradiente con respecto a MSE
      grad <- -2 * t(X_batch) %*% (y_batch - y_pred) / batch_size
      
      # Recortar gradiente
      grad <- clip_gradient(grad, C)
      
      # Añadir ruido gaussiano
      grad <- add_noise(grad, sigma, C, batch_size)
      
      # Actualizar los parámetros
      w <- w - learning_rate * grad
    }
  }
  return(w)
}

# Función FedAvg
FedAvg <- function(X_list, y_list, initial_w, learning_rate, epochs, batch_size, C, sigma, rounds) {
  K <- length(X_list)
  w_global <- initial_w
  
  for (round in 1:rounds) {
    w_local_list <- list()
    
    for (k in 1:K) {
      w_local <- ClientUpdate(X_list[[k]], y_list[[k]], w_global, learning_rate, epochs, batch_size, C, sigma)
      w_local_list[[k]] <- w_local
    }
    
    # Promediar los pesos locales
    w_global <- Reduce("+", w_local_list) / K
  }
  
  return(w_global)
}













# Datos simulados para cada cliente
n_clients <- 5
set.seed(42)


IT <- 100
#Usando base de datos mtcars

library(dplyr)

# Preparar el conjunto de datos `mtcars`
data(mtcars)
data(mtcars)
mtcars=mtcars[1:30,]
X <- as.matrix(mtcars[,-1])  # Variables independientes
y <- mtcars[,1]  # Variable dependiente

# Dividir el conjunto de datos en clientes (aquí, simplemente dividimos en 5 partes)
set.seed(42)
split_indices <- split(sample(1:nrow(X)), 1:5)
X_list <- lapply(split_indices, function(idx) X[idx, ])
y_list <- lapply(split_indices, function(idx) y[idx])

# Parámetros
n_features <- ncol(X)
initial_w <- rep(0, n_features)
w_global <- initial_w


learning_rate <- 0.01
epochs <- 10

#2,3,6,10,15

batch_size <- 6
C <- 1  # Límite de recorte
sigma <- 0.1  # Desviación estándar del ruido
rounds <- 1  # Número de rondas de agregación


epsilon=1000











set.seed(NULL)

IT <- 1000
w_global <- initial_w


Costos=rep(0, length(IT))
  
# Ejecutar FedAvg por IT iteraciones
for (i in 1:IT) {
  w_global <- FedAvg(X_list, y_list, w_global, learning_rate, epochs, batch_size, C, sigma, rounds)
  
  # Calcular costo global (promedio del costo de todos los clientes)
  Data=as.matrix(mtcars[,2:11])
  rownames(Data) <- NULL
  colnames(Data) <- NULL

  costo_global <- sum((as.vector(mtcars[,1])-Data%*% as.vector(w_global) )^2) / nrow(mtcars)
  Costos[i] <- costo_global
  # Imprimir resultados
  #cat("Iteración:", i, "\n")
  #cat("Pesos globales:", w_global, "\n")
  #cat("Costo global (MSE):", costo_global, "\n\n")
}



plot(Costos, type="l", xlab  =paste0("Iteración"), 
     main = expression("Error ("*epsilon* "=1000)"))







