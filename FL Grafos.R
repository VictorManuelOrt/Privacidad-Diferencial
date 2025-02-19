# Cargamos la base de datos mtcars
data(mtcars)

# Preprocesamiento: convertimos la columna mpg en el objetivo (y) y el resto de las columnas como características (X)
mtcars_data <- mtcars
X <- as.matrix(mtcars_data[, -1])  # Todas las columnas excepto mpg
y <- mtcars_data$mpg  # Columna mpg como objetivo

# Normalizamos las características
X <- scale(X)

# Combinamos características y etiquetas en un solo dataframe
dataset <- data.frame(X, y)

# Dividimos el dataset en 5 partes para simular 5 clientes
set.seed(123)
clients_data <- split(dataset, sample(1:5, nrow(dataset), replace = TRUE))

# Parámetros iniciales para cada cliente (theta de tamaño igual al número de características)
theta_init <- list(
  runif(ncol(X)),  # Cliente 1
  runif(ncol(X)),  # Cliente 2
  runif(ncol(X)),  # Cliente 3
  runif(ncol(X)),  # Cliente 4
  runif(ncol(X))   # Cliente 5
)

# Gradiente de la función de pérdida (usando pérdida cuadrática media)
gradient_loss <- function(theta, S) {
  X <- as.matrix(S[, -ncol(S)])  # Características
  y <- S[, ncol(S)]  # Etiquetas
  # Gradiente del MSE
  grad <- t(X) %*% (X %*% theta - y) / nrow(S)
  return(grad)
}

# Función para calcular el error cuadrático medio (MSE)
mse <- function(theta, data) {
  X <- as.matrix(data[, -ncol(data)])
  y <- data[, ncol(data)]
  predictions <- X %*% theta
  error <- mean((predictions - y)^2)
  return(error)
}

# Algoritmo D-SGD para un cliente
D_SGD_node <- function(theta_init, learning_rate, W, m, K, Di, neighbors, i) {
  theta_i <- theta_init
  
  for (k in 1:K) {
    # Selección del mini-batch de tamaño m del conjunto de datos Di
    S_k_i <- Di[sample(1:nrow(Di), m, replace = TRUE), ]
    
    # Actualización local de SGD
    grad <- gradient_loss(theta_i, S_k_i)
    theta_i_half <- theta_i - learning_rate * grad
    
    # Promediado con los vecinos según la matriz W
    theta_i_new <- theta_i_half * W[i, i]
    for (j in neighbors) {
      theta_i_new <- theta_i_new + W[i, j] * neighbors[[j]]$theta_half
    }
    theta_i <- theta_i_new
  }
  
  return(theta_i)
}

# Parámetros
learning_rate <- 0.01
m <- 5  # Tamaño del mini-batch
K <- 100 # Número de pasos

# Matriz de pesos de mezcla W proporcionada
W <- matrix(c(0.25, 0.25, 0.25, 0, 0.25,
              0.25, 0.50, 0.25, 0, 0,
              0.25, 0.25, 0.25, 0.25, 0,
              0, 0, 0.25, 0.417, 0.333,
              0.25, 0, 0, 0.333, 0.417), 
            nrow = 5, byrow = TRUE)

# Inicialización de theta para cada cliente
theta_clients <- list()
for (i in 1:5) {
  theta_clients[[i]] <- theta_init[[i]]
}



# Iteración del algoritmo D-SGD
for (k in 1:K) {
  theta_half_clients <- vector("list", 5)
  
  # Actualización local de cada cliente (paso 4)
  for (i in 1:5) {
    # Selección del mini-batch de tamaño m del conjunto de datos Di
    S_k_i <- clients_data[[i]][sample(1:nrow(clients_data[[i]]), m, replace = TRUE), ]
    
    # Cálculo del gradiente para la actualización local
    grad <- gradient_loss(theta_clients[[i]], S_k_i)
    
    # Actualización del modelo local
    theta_half_clients[[i]] <- theta_clients[[i]] - learning_rate * grad
  }
  
  # Actualización global (promediado entre vecinos) después de las actualizaciones locales
  for (i in 1:5) {
    neighbors <- which(W[i, ] != 0)
    
    # Inicialización del nuevo modelo para el cliente i
    theta_i_new <- theta_half_clients[[i]] * W[i, i]
    
    # Agregar contribuciones de los vecinos
    for (j in neighbors) {
      if (i != j) {
        theta_i_new <- theta_i_new + W[i, j] * theta_half_clients[[j]]
      }
    }
    
    # Actualización del modelo del cliente i
    theta_clients[[i]] <- theta_i_new
  }
}

# Resultados finales (modelos de cada cliente)
print(theta_clients)

# Cálculo del error global (MSE) de cada cliente con su propio modelo
errors_global <- sapply(1:5, function(i) {
  mse(theta_clients[[i]], dataset)
})

# Imprimir los errores globales
print(errors_global)
