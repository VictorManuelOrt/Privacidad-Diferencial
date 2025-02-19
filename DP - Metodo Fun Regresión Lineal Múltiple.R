library(stats)

library(VGAM)
library(ggplot2)
library(PerformanceAnalytics)
library(readxl)

privacy_parameter <- 1

#TARIFASTURBOSINA_AGOSTO2023 <- read_excel("C:/Users/5/Downloads/TARIFASTURBOSINA_AGOSTO2023.xlsx")
#TARIFASTURBOSINA_AGOSTO2023=read_excel("Turbosina/TARIFASTURBOSINA_AGOSTO2023.xlsx")


#dat_cor11=data.frame(TARIFASTURBOSINA_AGOSTO2023[,10],TARIFASTURBOSINA_AGOSTO2023$COM_ALM_EXP_AUTOTANQUE...20)

#chart.Correlation(dat_cor11)



diabetes=read.csv("diabetes.csv")
Dataset=read.csv("diabetes.csv")

DATA=data.frame(rep(1,dim(diabetes)[1]), diabetes[,c(2,3,7)])


#recortamos datos
#DATA=DATA[1:500,]

DATA1=DATA
#Preparaci贸n de los datos:

DATA1[,2]=(DATA[,2]-min(DATA[,2]))/((max(DATA[,2])-min(DATA[,2]))*sqrt(dim(DATA[,-1])[2]-1))
DATA1[,3]=(DATA[,3]-min(DATA[,3]))/((max(DATA[,3])-min(DATA[,3]))*sqrt(dim(DATA[,-1])[2]-1))

DATA1[,4]=(DATA[,4]-min(DATA[,4]))/(max(DATA[,4])-min(DATA[,4]))


DATA2=DATA1
DATA1=DATA2[1:500,]

LineR=lm(formula = DATA1$DiabetesPedigreeFunction ~ DATA1$Glucose+DATA1$BloodPressure, data=DATA1[,2:4])

LineR

#DATA[,1]=DATA[,1]/sqrt(dim(DATA[,-1])[2])

#DATA[,2]=(DATA[,2]-min(DATA[,2]))/((max(DATA[,2])-min(DATA[,2]))*sqrt(dim(DATA[,-1])[2]))
#DATA[,3]=(DATA[,3]-min(DATA[,3]))/((max(DATA[,3])-min(DATA[,3]))*sqrt(dim(DATA[,-1])[2]))

#DATA[,4]=(DATA[,4]-min(DATA[,4]))/(max(DATA[,4])-min(DATA[,4]))

#LineR=lm(formula = DATA$DiabetesPedigreeFunction ~ DATA$Glucose+DATA$BloodPressure, data=DATA[,2:4])






#Coeficientes:


Phi1=rep(0,dim(DATA[-1])[2])
#Phi2=rep(0,dim(DATA)[2]^2)
Phi2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])

for (j in 1:dim(DATA[,-1])[2]) {
  Phi1[j]=-2*sum(DATA[,j]*DATA[,dim(DATA)[2]])
}

for (j in 1:dim(DATA[-1])[2]) {
  for (l in 1:dim(DATA[-1])[2]) {
    Phi2[j,l]=sum(DATA[,j]*DATA[,l])
  }
  
}


#Polinomio: recibe como entrada un set de datos tamano n x (d+1)
# y un vector tamano d
f_obj=function(DATA,w){
  lambdaw1=rep(0,dim(DATA[-1])[2])
  #lambdaw1=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[,-1])[2]) {
    lambdaw1[j]=(-2*sum(DATA[,j]*DATA[,dim(DATA)[2]]))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=(sum(DATA[,j]*DATA[,l]))*w[j]*w[l]
    }
  }
  return(sum(lambdaw1)+sum(lambdaw2))
}



f_objD=function(w){
  f_obj(DATA1,w)
}



argmini=optim(rep(0,dim(DATA[,-1])[2]),f_objD)
argmini


Delta=2*(dim(DATA[-1])[2]+1)^2

#Parte de DP

#Polinomio con ruidio
Lf_obj=function(DATA,w){
  lambdaw1=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[,-1])[2]) {
    lambdaw1[j]=((-2*sum(DATA[,j]*DATA[,dim(DATA)[2]]))+rlaplace(1,0,Delta/privacy_parameter))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=((sum(DATA[,j]*DATA[,l]))+rlaplace(1,0,Delta/privacy_parameter))*w[j]*w[l]
    }
  }
  return(sum(lambdaw1)+sum(lambdaw2))
}


#funcion obj con ruido
Lf_objD=function(w){
  Lf_obj(DATA1,w)
}


#minimo de la funciopn con ruido
Largmini=optim(rep(0,dim(DATA[,-1])[2]),Lf_objD)



#funcion de costos consideranbdo eleent lioneal

f_objto=function(DATA,w){
  lambdaw0=sum((DATA[,dim(DATA)[2]])^2)
  lambdaw1=rep(0,dim(DATA[-1])[2])
  #lambdaw1=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[,-1])[2]) {
    lambdaw1[j]=(-2*sum(DATA[,j]*DATA[,dim(DATA)[2]]))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=(sum(DATA[,j]*DATA[,l]))*w[j]*w[l]
    }
  }
  return(sum(lambdaw1)+sum(lambdaw2)+lambdaw0)
}





ARGM=data.frame(LineR$coefficients,argmini$par,Largmini$par)
ARGM=t(ARGM)
ARGM=data.frame(ARGM,c(f_objD(LineR$coefficients),f_objD(argmini$par),f_objD(Largmini$par)),c(f_obj(DATA2,LineR$coefficients),f_obj(DATA2,argmini$par),f_obj(DATA2,Largmini$par)))
ARGM=data.frame(ARGM,c(f_objto(DATA2,LineR$coefficients),f_objto(DATA2,argmini$par),f_objto(DATA2,Largmini$par)))

names(ARGM)[4]="Error Entrenamiento"
names(ARGM)[5]="Error total"
names(ARGM)[6]="Error total no neg"

ARGM

n=100
WW=matrix(rep(0,n*(dim(DATA[-1])[2]+3)),100,dim(DATA[-1])[2]+3)
for (i in 1:n) {
  WW[i,c(1,2,3)]=optim(rep(0,dim(DATA[,-1])[2]),Lf_objD)$par
}


for (i in 1:n) {
  WW[i,4]=f_objD(WW[i,c(1,2,3)])
}

for (i in 1:n) {
  WW[i,5]=f_obj(DATA2,WW[i,c(1,2,3)])
}




for (i in 1:n) {
  WW[i,6]=f_objto(DATA2,WW[i,c(1,2,3)])
}


summary(WW)




######
#####
####
###
##
#

#Ejemplo caso particular
privacy_parameter <- 800
#cor(Dataset)


DATA=read.csv("Bases de Datos/FuelConsumption.csv")



DATA=data.frame(rep(1,dim(DATA)[1]),DATA$FUEL.CONSUMPTION ,DATA$COEMISSIONS)




DATA1=DATA
#Preparaci贸n de los datos:
DATA1[,1]=DATA[,1]/sqrt(dim(DATA[,-1])[2]-1)
DATA1[,2]=(DATA[,2]-min(DATA[,2]))/((max(DATA[,2])-min(DATA[,2]))*sqrt(dim(DATA[,-1])[2]-1))

DATA1[,3]=(DATA[,3]-min(DATA[,3]))/(max(DATA[,3])-min(DATA[,3]))


DATA2=DATA1

DATA1=DATA2[1:500,]




#Coeficientes:


Phi1=rep(0,dim(DATA[-1])[2])
#Phi2=rep(0,dim(DATA)[2]^2)
Phi2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])

for (j in 1:dim(DATA[,-1])[2]) {
  Phi1[j]=-2*sum(DATA[,j]*DATA[,dim(DATA)[2]])
}

for (j in 1:dim(DATA[-1])[2]) {
  for (l in 1:dim(DATA[-1])[2]) {
    Phi2[j,l]=sum(DATA[,j]*DATA[,l])
  }
  
}


#Polinomio: recibe como entrada un set de datos tamano n x (d+1)
# y un vector tamano d
f_obj=function(DATA,w){
  lambdaw1=rep(0,dim(DATA[-1])[2])
  #lambdaw1=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[,-1])[2]) {
    lambdaw1[j]=(-2*sum(DATA[,j]*DATA[,dim(DATA)[2]]))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=(sum(DATA[,j]*DATA[,l]))*w[j]*w[l]
    }
  }
  return(sum(lambdaw1)+sum(lambdaw2))
}



f_objD=function(w){
  f_obj(DATA1,w)
}



argmini=optim(rep(0,dim(DATA[,-1])[2]),f_objD)
argmini


Delta=2*(dim(DATA[-1])[2]+1)^2

#Parte de DP

#Polinomio con ruidio
Lf_obj=function(DATA,w){
  lambdaw1=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[,-1])[2]) {
    lambdaw1[j]=((-2*sum(DATA[,j]*DATA[,dim(DATA)[2]]))+rlaplace(1,0,Delta/privacy_parameter))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=((sum(DATA[,j]*DATA[,l]))+rlaplace(1,0,Delta/privacy_parameter))*w[j]*w[l]
    }
  }
  return(sum(lambdaw1)+sum(lambdaw2))
}


#funcion obj con ruido
Lf_objD=function(w){
  Lf_obj(DATA1,w)
}


#minimo de la funciopn con ruido
Largmini=optim(rep(0,dim(DATA[,-1])[2]),Lf_objD)



ARGM=data.frame(argmini$par,Largmini$par)
ARGM=t(ARGM)
ARGM=data.frame(ARGM,c(f_objD(argmini$par),f_objD(Largmini$par)),c(f_obj(DATA2,argmini$par),f_obj(DATA2,Largmini$par)))
ARGM=data.frame(ARGM,c(f_objto(DATA2,argmini$par),f_objto(DATA2,Largmini$par)))

names(ARGM)[3]="Error Entrenamiento"
names(ARGM)[4]="Error total"
names(ARGM)[5]="Error total no neg"

ARGM

X=seq(0,1,length=100)
plot(DATA1[,c(2,3)], main="Regresi贸n Lineal", xlab="Consumo de combustible "
     , ylab="Emisiones de CO2")
legend(x = "topleft",         # Posici贸n
       legend = c("Regresi贸n Lineal", "Regresi贸n Lineal con DP"), # Textos de la leyenda
       lty = c(1, 1),          # Tipo de l铆neas
       col = c("green", "blue"),          # Colores de las l铆neas
       lwd = 2)     
lines(X,argmini$par[1]+argmini$par[2]*X,col="green", lwd=3)
lines(X,Largmini$par[1]+Largmini$par[2]*X,col="blue", lwd=3)


b <- argmini$par[1]  # Intercepto
m <- argmini$par[2]  # Pendiente
# Asumiendo que DATA1 es tu dataframe y las columnas 2 y 3 son x y y
xx <- DATA1[, 2]  # La variable independiente (x)
yy <- DATA1[, 3]  # La variable dependiente (y)
# Calcular las predicciones de y usando los coeficientes b y m
yy_pred <- b + m * xx
# Calcular el error cuadr醫ico medio (MSE)
mse <- sum((yy - yy_pred)^2)

# Imprimir el MSE
mse


Lb <- Largmini$par[1]  # Intercepto
Lm <- Largmini$par[2]  # Pendiente
# Asumiendo que DATA1 es tu dataframe y las columnas 2 y 3 son x y y
Lxx <- DATA1[, 2]  # La variable independiente (x)
Lyy <- DATA1[, 3]  # La variable dependiente (y)
# Calcular las predicciones de y usando los coeficientes b y m
Lyy_pred <- Lb + Lm * Lxx
# Calcular el error cuadr醫ico medio (MSE)
Lmse <- sum((Lyy - Lyy_pred)^2)

# Imprimir el MSE
Lmse




X=seq(0,1,length=100)

plot(DATA1[,c(2,3)], main="Regresi髇 Lineal", xlab="Consumo de combustible "
     , ylab="Emisiones de CO2")
legend(x = "topleft",         # Posici贸n
       legend = c(paste0("Regresi髇 Lineal, error=", round(mse,3), "      ") , paste0("Regresi髇 Lineal DP, error=", round(Lmse,3))), # Textos de la leyenda
       lty = c(1, 1),          # Tipo de l铆neas
       col = c("green", "blue"),          # Colores de las l铆neas
       lwd = 2)     
lines(X,argmini$par[1]+argmini$par[2]*X,col="green", lwd=3)
lines(X,Largmini$par[1]+Largmini$par[2]*X,col="blue", lwd=3)













