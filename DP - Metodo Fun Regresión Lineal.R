library(VGAM)
library(ggplot2)
library(stats)
library(PerformanceAnalytics)
library(readxl)

privacy_parameter <- 0.1

#TARIFASTURBOSINA_AGOSTO2023 <- read_excel("C:/Users/5/Downloads/TARIFASTURBOSINA_AGOSTO2023.xlsx")
TARIFASTURBOSINA_AGOSTO2023=read_excel("Turbosina/TARIFASTURBOSINA_AGOSTO2023.xlsx")


dat_cor11=data.frame(TARIFASTURBOSINA_AGOSTO2023[,10],TARIFASTURBOSINA_AGOSTO2023$COM_ALM_EXP_AUTOTANQUE...20)

chart.Correlation(dat_cor11)

#DAA=cbind(TARIFASTURBOSINA_AGOSTO2023[,10],TARIFASTURBOSINA_AGOSTO2023$COM_ALM_EXP_AUTOTANQUE...20)

#DAA=cbind(as.vector(TARIFASTURBOSINA_AGOSTO2023[,10]),as.vector(TARIFASTURBOSINA_AGOSTO2023$COM_ALM_EXP_AUTOTANQUE...20))

#DAA1=DAA/15
DAA1=dat_cor11/15
#Funcion de costos (L2) de cada t_i
#ti es la d+1 tupla, siendo la ultima entrada y, w es un vector d dim
#fc=function(ti,w,d){
#  costo=(ti[d+1]-ti[1:d]%*%w)^2
#  return(costo)
#}

fc=function(ti,w,d){
  costo=(as.numeric(ti[d+1])-sum(as.numeric(ti[1:d])*w))^2
  return(costo)
}



#Suma de costos, D es el conjunto de datos con filas siendo las ti tuplas
fcD=function(Data,w){
  Suma=0
  for (i in 1:dim(Data)[1]){
  Suma=Suma+fc(as.vector(Data[i,]),as.vector(w),dim(Data)[2]-1)
  }
  return(Suma)
}

#Suma de costos dejando fija a la D
Fw=function(w){
  return(fcD(DAA1,w))
}


O1=optimize(Fw,lower = -1,upper = 1.5)




#Coeficientes:
Phi0=function(Data){
  sum((Data[,length(Data)])^2)
  
}



Phi1=function(Data){
  sum((Data[,1])*Data[,2])
  
}

Phi2=function(Data){
  sum((Data[,1])^2)
  
}

#funcion de costos solo como funcion de w con D fijo

Cost=function(Data,w){
  Phi2(Data)*w^2-2*Phi1(Data)*w+Phi0(Data)
}


FwP=function(w){
  return(Cost(DAA1,w))
}

OP=optimize(FwP,lower = -1,upper = 1.5)

xseq=seq(min(DAA1),max(DAA1),len=10)
EscReal=as.numeric(O1[1])*xseq


plot(DAA1)
lines(xseq,EscReal, col="red")



#Sensibilidad 2(d+1)^2
Delta=2*(1+1)^2

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
ORand



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


