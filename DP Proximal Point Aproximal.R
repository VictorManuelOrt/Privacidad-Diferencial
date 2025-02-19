####
####
####
# Algoritmo por Punto Proximal (PPA)




# Función de error cuadrático medio
mse <- function(theta, X, y) {
  n <- length(y)
  error <- y - X %*% theta
  return(sum(error^2) / n)
}

# Proximal Point Algorithm
ppa <- function(X, y, lambda = 0.1, max_iter = 100) {
  # Inicialización
  n <- nrow(X)
  p <- ncol(X)
  theta <- matrix(0, nrow = p, ncol = 1)  # Inicializa theta en 0
  Acz<- matrix(0, nrow = p, ncol = max_iter)
  Error<- matrix(0, nrow = 1, ncol = max_iter)
  
  for (k in 1:max_iter) {
    # Resuelve el subproblema en cada iteración
    # Minimizamos mse + término de proximidad
    XtX <- t(X) %*% X
    XtY <- t(X) %*% y
    term <- XtX + (n * lambda / 2) * diag(p)
    rhs <- XtY + (n * lambda / 2) * theta
    
    # Actualiza theta usando la fórmula derivada
    theta_new <- solve(term) %*% rhs
    
    
    # Actualiza theta
    theta <- theta_new
    Acz[,k]=theta_new
    Error[,k]=mse(theta,X,y)
  }
  
  return(list(theta = theta, iterations = k, Actualizaciones= Acz, HistError=Error))
}





# Ejemplo de uso
set.seed(NULL)
n <- 100
p <- 1


#Se agreg 1 para representar el sesgo 
X <- cbind(1, matrix(rnorm(n * p), n, p))

#Trataremos de aproximar los siguientes valores
theta_true <- matrix(c(2, -3), nrow = p+1)


#Generamos ruido
y <- X %*% theta_true + rnorm(n)

plot(X[,2],y)



# Aplicamos el PPA
resultado <- ppa(X, y)
print(paste("Theta estimado:", toString(resultado$theta)))
print(paste("Número de iteraciones:", resultado$iterations))
print( resultado$HistError)
print( resultado$Actualizaciones)

