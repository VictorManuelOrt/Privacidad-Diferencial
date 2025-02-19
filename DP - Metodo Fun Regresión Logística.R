library(VGAM)
library(ggplot2)
library(stats)
library(PerformanceAnalytics)
library(readxl)

privacy_parameter <- 0.1



Dataset <- read.csv("C:/Users/5/OneDrive - Benemérita Universidad Autónoma de Puebla/Documents/TESIS Ms Cs Mt/diabetes.csv", header=TRUE)
Dataset <- read.csv("diabetes.csv")

#Etiquetas de los muertos por diab?tes

D1=which(as.numeric(substr(Dataset[,11],2,3))<=14 )
D2=which(as.numeric(substr(Dataset[,11],2,3))>=10 )
D3=which(substr(Dataset[,11],1,2)=="E1")

d=intersect(D1,D2)
D=intersect(d,D3)


Ind=which(Dataset$edad>=4000)
IND=which(Dataset$edad<=4120)
II=intersect(Ind,IND)
DATA=Dataset[II,]

Causa=DATA$lista_mex
#I219
Y=rep(0,length(Causa))
for (i in 1:length(Causa) ){
  Y[i]= (Causa[i]=="49A"|Causa[i]=="49B"|Causa[i]=="49C"|Causa[i]=="49D"|Causa[i]=="49E"|Causa[i]=="49F"|Causa[i]=="49I")
}
sum(Y)

Data=data.frame(Dataset$BMI,Dataset$Outcome)


DAA1=Data
DAA1=Data[1:100000,]
plot(DAA1)
#Funcion de costos (L2) de cada t_i
#ti es la d+1 tupla, siendo la ultima entrada y, w es un vector d dim
#fc=function(ti,w,d){
#  costo=(ti[d+1]-ti[1:d]%*%w)^2
#  return(costo)
#}



fc=function(ti,w){
  costo=log(1+exp(as.numeric(ti[1])*w))-as.numeric(ti[1])*w*as.numeric(ti[2])
  return(costo)
}




#Suma de costos, D es el conjunto de datos con filas siendo las ti tuplas
fcD=function(Data,w){
  Suma=0
  for (i in 1:dim(Data)[1]){
  Suma=Suma+fc(as.vector(Data[i,]),as.vector(w))
  }
  return(Suma)
}

#Suma de costos dejando fija a la D
Fw=function(w){
  return(fcD(DAA1,w))
}


O1=optimize(Fw,lower = -10,upper = 10)
O1



#Coeficientes:
Phi0=function(Data){
  length(Data)*log(2)
}



Phi1=function(Data){
  sum((1/2)*Data[,1]-Data[,1]*Data[,2])
}

Phi2=function(Data){
  sum((1/8)*(Data[,1])^2)
  
}

#funcion de costos solo como funcion de w con D fijo

Cost=function(Data,w){
  Phi2(Data)*w^2+Phi1(Data)*w+Phi0(Data)
}


FwP=function(w){
  return(Cost(DAA1,w))
}

OP=optimize(FwP,lower = -1,upper = 1.5)
OP


#Sensibilidad 2(d+1)^2
Delta=1/4+3

R1=rlaplace(1,0,Delta/privacy_parameter)
R2=rlaplace(1,0,Delta/privacy_parameter)
R3=rlaplace(1,0,Delta/privacy_parameter)


#Funcion de costco con ruiidos
CostRand=function(Data,w){
  (Phi2(Data)+R1)*w^2+(Phi1(Data)+R2)*w+Phi0(Data)+R3
}

#Funcion de costco con ruiidos dejndo constante a D
FwPRand=function(w){
  return(CostRand(DAA1,w))
}

ORand=optimize(FwPRand,lower = -10,upper = 10.5)
ORand





plot(DAA1)
lines(DAA1[,1], DAA1[,1]*ORand$minimum, col="red")

plot(DAA1[,1], DAA1[,1]*ORand$minimum, col="red",type="l")



xseq=seq(min(DAA1),max(DAA1),len=10)
EscReal=as.numeric(O1[1])*xseq
EscAlt=as.numeric(ORand[1])*xseq


plot(DAA1)
lines(xseq,EscReal, col="red")
lines(xseq,EscAlt, col="blue")

#Calculo de L2
Fw(as.numeric(O1[1]))
Fw(as.numeric(ORand[1]))



#Generamos varias simulaciones para w

plot(DAA1)
lines(xseq,EscReal, col="red", lwd=4)
for (i in 1:100) {
  R1=rlaplace(1,0,8/privacy_parameter)
  R2=rlaplace(1,0,8/privacy_parameter)
  R3=rlaplace(1,0,8/privacy_parameter)
  
  
  #Funcion de costco con ruiidos
  CostRand=function(Data,w){
    (Phi2(Data)+R1)*w^2-2*(Phi1(Data)+R2)*w+Phi0(Data)+R3
  }
  
  #Funcion de costco con ruiidos dejndo constante a D
  FwPRand=function(w){
    return(CostRand(DAA1,w))
  }
  
  ORand=optimize(FwPRand,lower = -1,upper = 1.5)
  EscAlt=as.numeric(ORand[1])*xseq
  lines(xseq,EscAlt, col="blue")
  
}
lines(xseq,EscReal, col="red", lwd=4)


