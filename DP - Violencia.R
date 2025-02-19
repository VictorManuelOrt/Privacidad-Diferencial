library(VGAM)


Dataset <- read.csv("conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)

curve(dlaplace(x, location = 33, scale = 7), from = 0, to = 66, 
      main="Densidad de la Distribuci贸n de Laplace", ylab=" ", xlab = " ",
      col=3,lwd=3, sub=paste(" Par谩metro de localizaci贸n=33", 
                             "\n", "Par谩metro de escala=7"))
#Etiquetas de los muertos por violencia en casa


alpha=which( Dataset[,11]=="Y070"|
             Dataset[,11]=="X850"|
            Dataset[,11]=="X860"|
            Dataset[,11]=="X870"|
            Dataset[,11]=="X880"|
            Dataset[,11]=="X900"|
            Dataset[,11]=="X910"|
            Dataset[,11]=="X920"|
            Dataset[,11]=="X930"|
            Dataset[,11]=="X940"|
            Dataset[,11]=="X950"|
            Dataset[,11]=="X960"|
            Dataset[,11]=="X970"|
            Dataset[,11]=="X990"|
            Dataset[,11]=="Y000"|
            Dataset[,11]=="Y010"|
            Dataset[,11]=="Y040"|
            Dataset[,11]=="Y080"|
            Dataset[,11]=="Y090"
)

Alpha=Dataset[alpha,]


A1=which(as.numeric(Alpha[,14])<=4010)
A2=which(as.numeric(Alpha[,14])<=4020 & as.numeric(Alpha[,14])>4010)
A3=which(as.numeric(Alpha[,14])<=4030 & as.numeric(Alpha[,14])>4020)
A4=which(as.numeric(Alpha[,14])<=4040 & as.numeric(Alpha[,14])>4030)
A5=which(as.numeric(Alpha[,14])<=4050 & as.numeric(Alpha[,14])>4040)
A6=which(as.numeric(Alpha[,14])<=4060 & as.numeric(Alpha[,14])>4050)
A7=which(as.numeric(Alpha[,14])<=4070 & as.numeric(Alpha[,14])>4060)
A8=which(as.numeric(Alpha[,14])<=4080 & as.numeric(Alpha[,14])>4070)
A9=which(as.numeric(Alpha[,14])<=4090 & as.numeric(Alpha[,14])>4080)
A10=which(as.numeric(Alpha[,14])<=4100 & as.numeric(Alpha[,14])>4090)
A11=which(as.numeric(Alpha[,14])<=4110 & as.numeric(Alpha[,14])>4100)
#A12=which(as.numeric(Alpha[,14])<=4120 & as.numeric(Alpha[,14])>4110)
#A13=which(as.numeric(Alpha[,14])<=4130 & as.numeric(Alpha[,14])>4120)

C1=length(A1)
C2=length(A2)
C3=length(A3)
C4=length(A4)
C5=length(A5)
C6=length(A6)
C7=length(A7)
C8=length(A8)
C9=length(A9)
C10=length(A10)
C11=length(A11)
#C12=length(A12)
#C13=length(A13)

C=c(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11)#,C12,C13)

Hist=data.frame(c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","100-110"), C)

colnames(Hist)[1]="Intervalo" 


#Grafica con los datos reales
barplot(Hist$C, names.arg=Hist$Intervalo,col = 3, cex.names=.8,
        xlab="Rango de edad", ylab = "Cantidad",
        main="V韈timas de violencia familiar por edades")


barplot(Hist$C+rlaplace(11,0,1/.05), names.arg=Hist$Intervalo,col = 2,
        cex.names=.8,
        xlab="Rango de edad", ylab = "Cantidad",
        main="V韈timas de violencia familiar por edades con DP")







#Hombres
H1=length(which(as.numeric(Alpha[A1,13])==1))
H2=length(which(as.numeric(Alpha[A2,13])==1))
H3=length(which(as.numeric(Alpha[A3,13])==1))
H4=length(which(as.numeric(Alpha[A4,13])==1))
H5=length(which(as.numeric(Alpha[A5,13])==1))
H6=length(which(as.numeric(Alpha[A6,13])==1))
H7=length(which(as.numeric(Alpha[A7,13])==1))
H8=length(which(as.numeric(Alpha[A8,13])==1))
H9=length(which(as.numeric(Alpha[A9,13])==1))
H10=length(which(as.numeric(Alpha[A10,13])==1))
H11=length(which(as.numeric(Alpha[A11,13])==1))
H12=length(which(as.numeric(Alpha[A12,13])==1))
H13=length(which(as.numeric(Alpha[A13,13])==1))

H=c(H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13)



#Mujeres
M1=length(which(as.numeric(Alpha[A1,13])==2))
M2=length(which(as.numeric(Alpha[A2,13])==2))
M3=length(which(as.numeric(Alpha[A3,13])==2))
M4=length(which(as.numeric(Alpha[A4,13])==2))
M5=length(which(as.numeric(Alpha[A5,13])==2))
M6=length(which(as.numeric(Alpha[A6,13])==2))
M7=length(which(as.numeric(Alpha[A7,13])==2))
M8=length(which(as.numeric(Alpha[A8,13])==2))
M9=length(which(as.numeric(Alpha[A9,13])==2))
M10=length(which(as.numeric(Alpha[A10,13])==2))
M11=length(which(as.numeric(Alpha[A11,13])==2))
M12=length(which(as.numeric(Alpha[A12,13])==2))
M13=length(which(as.numeric(Alpha[A13,13])==2))

M=c(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13)




D1=which(as.numeric(substr(Dataset[,11],2,3))<=14 )
D2=which(as.numeric(substr(Dataset[,11],2,3))>=10 )
D3=which(substr(Dataset[,11],1,2)=="E1")

d=intersect(D1,D2)
D=intersect(d,D3)



#Etiquetas de los muertos por diab?tes
M=which(as.numeric(Dataset[,14])<=4017)

#Interseccion de los anteriores
I=intersect(D,M)



A=Dataset[D,]

B=Dataset[M,]

#Registros de personas muertas por Diabetes menores de edad
C=Dataset[I,]


#Cantidad de resgistros en C
#Dato real
c=length(C[,1])


x=seq(50, 130,length=10000)


plot(x, dlaplace(x,c,(1/1)), type="l", col="red",  xlim=c(80,130), ylim=c(0,0.5),
     main="", ylab=" ", lwd=1)
lines(x, dlaplace(x,c+1,(1/1)), type="l", col="blue")
abline(v=c, col="red")
abline(v=c+1, col="blue")

plot(x, dlaplace(x,c,(1/0.1)), type="l", col="red",  xlim=c(80,130), ylim=c(0,0.5),
     main="", ylab=" ")
lines(x, dlaplace(x,c+1,(1/0.1)), type="l", col="blue")
abline(v=c, col="red")
abline(v=c+1, col="blue")

plot(x,  dlaplace(x,c,(1/0.01)), type="l", col="red",  xlim=c(0,170), ylim=c(0,0.5), main="Distribucion con 	epsilon=0.01$")
lines(x, dlaplace(x,c+1,(1/0.01)), type="l", col="blue")
abline(v=c, col="red")
abline(v=c+1, col="blue")



c+rlaplace(1,0,1/1)




curve(dlaplace(x, location = 33, scale = 7), from = 0, to = 66, 
      main="Densidad de la Distribuci贸n de Laplace", ylab=" ", xlab = " ",
      col=3,lwd=3, sub=paste(" Par谩metro de localizaci贸n=33", 
                             "\n", "Par谩metro de escala=7"))
