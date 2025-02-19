library(VGAM)


Dataset <- read.csv("C:/Users/5/OneDrive - Benemérita Universidad Autónoma de Puebla/Desktop/Maestría/Differential Privacy Codigos y bases de datos/conjunto_de_datos_defunciones_registradas_2021_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2021.csv", header=FALSE)


#Etiquetas de los muertos por diab?tes

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


x=seq(0, 170,length=1000)


plot(x, dlaplace(x,c,(1/1)), type="l", col="red",  xlim=c(0,170), ylim=c(0,0.5), main="Distribucion con 	epsilon=1$")
lines(x, dlaplace(x,c+1,(1/1)), type="l", col="blue")
abline(v=c, col="red")
abline(v=c+1, col="blue")

plot(x, dlaplace(x,c,(1/0.1)), type="l", col="red",  xlim=c(0,170), ylim=c(0,0.5), main="Distribucion con 	epsilon=0.1$")
lines(x, dlaplace(x,c+1,(1/0.1)), type="l", col="blue")
abline(v=c, col="red")
abline(v=c+1, col="blue")

plot(x,  dlaplace(x,c,(1/0.01)), type="l", col="red",  xlim=c(0,170), ylim=c(0,0.5), main="Distribucion con 	epsilon=0.01$")
lines(x, dlaplace(x,c+1,(1/0.01)), type="l", col="blue")
abline(v=c, col="red")
abline(v=c+1, col="blue")



c+rlaplace(1,0,1/1)
