library(VGAM)
library(ggplot2)
library(stats)
library(PerformanceAnalytics)
library(readxl)

install.packages("cluster")
library(cluster)
k <- 3

# Ejecutar Fuzzy C-Means
result <- fanny(Dataset, k)

# Visualizar resultados
plot(Dataset, col=rgb(result$membership), pch=20, main="Fuzzy C-Means Clustering",cex=2)




DATA <- read.csv("diabetes.csv")

PPlot=DATA[1:200,c(2,6,9)]
PPlot[,3]=PPlot[,3]+1
#plot(PPlot[,1],PPlot[,2],col=PPlot[,3],pch=16)

#Escojemos los datos
Dataset=DATA[1:200,c(2,6)]


#preparacion de los datos
for (j in 1:dim(Dataset)[2]) {
  Dataset[,j]=Dataset[,j]/(max(Dataset[,j])-min(Dataset[,j]))
  
}



#Generar c valores iniciales para los centroides de los cluster

c=2 #cantidad de clusters
privacy_parameter <- 2000 #parametro de privacidad
m=2 #coeficiente de fuzzificacion
t=4 #numero de iteraciones
d=dim(Dataset)[2]




#Matriz con los centroides
V=matrix(runif(c*dim(Dataset)[2]), c, dim(Dataset)[2])

N=dim(Dataset)[1]
epsilon=privacy_parameter/(t*c*(2+dim(Dataset)[2]))


U=matrix(rep(0,dim(Dataset)[1]*c), dim(Dataset)[1],c)


for (k in 1:t) {
  
  
  for (i in 1:dim(Dataset)[1]) {
    for (j in 1:c) {
      S=0
      for (l in 1:c) {
        S=S+(sqrt(sum((Dataset[i,]-V[j,])^2))
             +rlaplace(1,0,(3*sqrt(d)/epsilon))
             /sqrt(sum((Dataset[i,]-V[l,])^2))
             +rlaplace(1,0,(3*sqrt(d)/epsilon)))^(1/(m-1))
      }
      U[i,j]=1/S #obs x_i\neq v_j
    }
  }
  #Paso de normalizacion de las filas de U
  for (i in 1:dim(Dataset)[1]) {
    U[i,]=U[i,]-min(U)+1
   U[i,]=U[i,]/sum(U[i,]) 
  }
  #Calculo de los nuevos centroides
  for (i in 1:c) {
    for (j in 1:dim(Dataset)[2]) {
      V[i,j]=sum(((U[,i])^m)*Dataset[,j])/sum(((U[,i])^m))+rlaplace(1,0,(3/epsilon))
   
  #   if (V[i,j]>1){
   #     V[i,j]=1
    #  }  else {
     #   if (V[i,j]<0){
      #    V[i,j]=0
        }
      }
      
    #}
  #}
}






Color=data.frame(U,rep(0,dim(Dataset)[1]))

plot(Dataset[,1],Dataset[,2],col=rgb(Color),pch=16)



plot(Dataset, col=rgb(result$membership), pch=20, main="Fuzzy C-Means Clustering",cex=2)
plot(PPlot[,1],PPlot[,2],col=PPlot[,3],pch=16)




















