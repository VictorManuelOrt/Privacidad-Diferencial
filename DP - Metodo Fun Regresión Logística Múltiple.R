library(stats)

library(VGAM)
library(ggplot2)
library(PerformanceAnalytics)
library(readxl)

privacy_parameter <- 50
#TARIFASTURBOSINA_AGOSTO2023 <- read_excel("C:/Users/5/Downloads/TARIFASTURBOSINA_AGOSTO2023.xlsx")
#TARIFASTURBOSINA_AGOSTO2023=read_excel("Turbosina/TARIFASTURBOSINA_AGOSTO2023.xlsx")


#dat_cor11=data.frame(TARIFASTURBOSINA_AGOSTO2023[,10],TARIFASTURBOSINA_AGOSTO2023$COM_ALM_EXP_AUTOTANQUE...20)

#chart.Correlation(dat_cor11)



Dataset=read.csv("diabetes.csv")
diabetes=read.csv("diabetes.csv")


DATA=data.frame(rep(1,dim(diabetes)[1]),Dataset$Glucose ,Dataset$BMI,Dataset$Outcome)


#recortamos datos
#DATA=DATA[1:500,]

DATA1=DATA
#Preparación de los datos:
DATA1[,1]=DATA[,1]/sqrt(dim(DATA[,-1])[2]-1)
DATA1[,2]=(DATA[,2]-min(DATA[,2]))/((max(DATA[,2])-min(DATA[,2]))*sqrt(dim(DATA[,-1])[2]-1))
DATA1[,3]=(DATA[,3]-min(DATA[,3]))/((max(DATA[,3])-min(DATA[,3]))*sqrt(dim(DATA[,-1])[2]-1))

DATA1[,4]=(DATA[,4]-min(DATA[,4]))/(max(DATA[,4])-min(DATA[,4]))


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
    lambdaw1[j]=((1/2)*sum(DATA[,j])-sum(DATA[,j]*DATA[,dim(DATA)[2]]))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=(1/8)*(sum(DATA[,j]*DATA[,l]))*w[j]*w[l]
    }
  }
  return(sum(lambdaw1)+sum(lambdaw2))
}



f_objD=function(w){
  f_obj(DATA1,w)
}



argmini=optim(rep(0,dim(DATA[,-1])[2]),f_objD)
argmini


Delta=((dim(DATA[-1])[2])^2)/4+3*dim(DATA[-1])[2]

#Parte de DP

#Polinomio con ruidio



Lf_obj=function(DATA,w){
  lambdaw1=rep(0,dim(DATA[-1])[2])
  #lambdaw1=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[,-1])[2]) {
    lambdaw1[j]=((1/2)*sum(DATA[,j])-sum(DATA[,j]*DATA[,dim(DATA)[2]])+rlaplace(1,0,Delta/privacy_parameter))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=((1/8)*(sum(DATA[,j]*DATA[,l]))+rlaplace(1,0,Delta/privacy_parameter))*w[j]*w[l]
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
Largmini




#funcion de costo exp. (maxima veroslimilitud)

f_objto=function(DATA,w){
  S=0
  for (i in 1:dim(DATA)[1]) {
    S=S+log(1+exp(sum(DATA[i,1:(dim(DATA)[2]-1)]*w)))-DATA[i,dim(DATA)[2]]*sum(DATA[i,1:(dim(DATA)[2]-1)]*w)
  }
  return(S)
}


f_objtoD=function(w){
  f_objto(DATA1,w)
}

argminito=optim(rep(0,dim(DATA[,-1])[2]),f_objtoD)






ARGM=data.frame(argminito$par,argmini$par,Largmini$par)
ARGM=t(ARGM)
ARGM=data.frame(ARGM,c(f_objD(argminito$par),f_objD(argmini$par),f_objD(Largmini$par)),c(f_obj(DATA2,argminito$par),f_obj(DATA2,argmini$par),f_obj(DATA2,Largmini$par)))
ARGM=data.frame(ARGM,c(f_objto(DATA2,argminito$par),f_objto(DATA2,argmini$par),f_objto(DATA2,Largmini$par)))

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


#f_objto=function(DATA,w){
 # S=0
  #for (i in 1:dim(DATA)[1]) {
   # S=S+log(1+exp(sum(DATA[i,1:(dim(DATA)[2]-1)]*w)))-DATA[i,dim(DATA)[2]]*sum(DATA[i,1:(dim(DATA)[2]-1)]*w)
#  }
 # return(S)
#}




#f_objtoD=function(w){
 # f_objto(DATA1,w)
#}



#argminito=optim(rep(0,dim(DATA[,-1])[2]),f_objtoD)






summary(WW)




















#Ejemplo caso particular
DATA=data.frame(rep(1,dim(diabetes)[1]),Dataset$Glucose ,Dataset$Outcome)

DATA1=DATA
#Preparación de los datos:
DATA1[,1]=DATA[,1]/sqrt(dim(DATA[,-1])[2]-1)
DATA1[,2]=(DATA[,2]-min(DATA[,2]))/((max(DATA[,2])-min(DATA[,2]))*sqrt(dim(DATA[,-1])[2]-1))

DATA1[,3]=(DATA[,3]-min(DATA[,3]))/(max(DATA[,3])-min(DATA[,3]))


DATA2=DATA1

DATA1=DATA2[1:500,]









#LogR=glm(formula = DATA1$Dataset.Outcome ~ DATA1$Dataset.Glucose+DATA1$Dataset.BMI, data=DATA1[,2:4],family = "binomial")

#LogR

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
    lambdaw1[j]=((1/2)*sum(DATA[,j])-sum(DATA[,j]*DATA[,dim(DATA)[2]]))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=(1/8)*(sum(DATA[,j]*DATA[,l]))*w[j]*w[l]
    }
  }
  return(sum(lambdaw1)+sum(lambdaw2))
}



f_objD=function(w){
  f_obj(DATA1,w)
}



argmini=optim(rep(0,dim(DATA[,-1])[2]),f_objD)
argmini


Delta=((dim(DATA[-1])[2])^2)/4+3*dim(DATA[-1])[2]

#Parte de DP

#Polinomio con ruidio



Lf_obj=function(DATA,w){
  lambdaw1=rep(0,dim(DATA[-1])[2])
  #lambdaw1=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[,-1])[2]) {
    lambdaw1[j]=((1/2)*sum(DATA[,j])-sum(DATA[,j]*DATA[,dim(DATA)[2]])+rlaplace(1,0,Delta/privacy_parameter))*w[j]
  }
  
  
  lambdaw2=matrix(rep(0,dim(DATA[-1])[2]^2),dim(DATA[-1])[2],dim(DATA[-1])[2])
  for (j in 1:dim(DATA[-1])[2]) {
    for (l in 1:dim(DATA[-1])[2]) {
      lambdaw2[j,l]=((1/8)*(sum(DATA[,j]*DATA[,l]))+rlaplace(1,0,Delta/privacy_parameter))*w[j]*w[l]
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
Largmini


#argminito=optim(rep(0,dim(DATA[,-1])[2]),f_objtoD)



ARGM=data.frame(argmini$par,Largmini$par)
ARGM=t(ARGM)
ARGM=data.frame(ARGM,c(f_objD(argmini$par),f_objD(Largmini$par)),c(f_obj(DATA2,argmini$par),f_obj(DATA2,Largmini$par)))
ARGM=data.frame(ARGM,c(f_objto(DATA2,argmini$par),f_objto(DATA2,Largmini$par)))

names(ARGM)[3]="Error Entrenamiento"
names(ARGM)[4]="Error total"
names(ARGM)[5]="Error total no neg"

ARGM

X=seq(0,1,length=100)
plot(DATA1$Dataset.Glucose,DATA1$Dataset.Outcome, main="Regresión Logística",
     xlab="Nivel de Glucosa", ylab="Probabilidad de diabetes")
lines(X,(exp(argmini$par[1]+argmini$par[2]*X))/(1+exp(argmini$par[1]+argmini$par[2]*X)),col="green", lwd=3)
#lines(X,(exp(argminito$par[1]+argminito$par[2]*X))/(1+exp(argminito$par[1]+argminito$par[2]*X)),col="red", lwd=3)
lines(X,(exp(Largmini$par[1]+Largmini$par[2]*X))/(1+exp(Largmini$par[1]+Largmini$par[2]*X)),col="blue", lwd=3)
legend(x = 0,y=0.95,         # Posición
       legend = c("Regresión Logística", "Regresión Logística con DP"), # Textos de la leyenda
       lty = c(1, 1),          # Tipo de líneas
       col = c("green", "blue"),          # Colores de las líneas
       lwd = 2)     













