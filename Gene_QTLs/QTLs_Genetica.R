# Mediciones de traits poligenicos: QTLS 
# Tarea grupal Genética

# Notas: Estado_Nat se refiere al estado de nacimiento
#        Estado_Dura se refiere al estado donde ha vivido mas la persona
#        Region 1 solo son Norte, Central y Sur
#        Region 2 lo mismo pero con Central dividido en Occidente, Oriente, Central Norte y Central Sur
#        Altura esta medida en metros y Calzado en cm

rm(list=ls())
#setwd("~/Downloads")
#setwd("~/Documents/")
####################################
#    Internalizacion de datos      #
####################################
D = read.csv("MedicionesQTLs_Geneticacsv", header = FALSE)
D = as.data.frame(D)
D = D[1:22]
D[,23] = c("Norte",  "Central","Central","Central","Central","Central","Central","Central","Central","Central",
           "Central","Central","Sur",    "Central","Norte",  "Central","Central","Central","Central","Central",
           "Central","Central","Sur",    "Central","Central","Norte",  "Sur",    "Central","Central","Central","Central")
############  1              2             3            4               5               6            7                8           9             10
D[,24] = c("Norte",     "CentralSur",  "CentralSur","CentralSur",    "CentralSur", "CentralNor",  "CentralNor",  "Occidente", "Occidente",  "Oriente",
           "Occidente","CentralSur",  "Sur",       "Occidente",    "Norte",      "Occidente",  "CentralNor",  "CentralSur", "CentralSur",  "CentralSur",
           "CentralSur","CentralNor",  "Sur",       "CentralSur",    "CentralSur",  "CentralNor",   "Sur",      "CentralSur",   "Oriente",   "CentralNor",    "CentralSur")

colnames(D) = c("Nombre","Sexo","Edad","Estado_Nat","Estado_Dura","Altura","PigmFr1","PigmFr2","PigmFr3",
                "PigmFr4","PigmFr5","PigmFrAvg","PigmBr1","PigmBr2","PigmBr3","PigmBr4","PigmBr5","PigmBrAvg",
                "Calzado","PS_Sis","PS_Dia","Dia_med","Region1","Region2")
D

####################################
#       Análisis de datos          #
####################################

##############  Histogramas por sexo ################################################# 
num=seq(1,length(D[,1]))
M=num[D$Sexo=="M"]
H=num[D$Sexo=="H"]

hist((D$PigmFrAvg[H]),xlim=c(35,60),probability=T,col=rgb(0.1, 0.9, 0.1, 0.3),main='Pigmentación en frente por sexo ("ambiental")'
     ,xlab="Nivel de pigmentación",ylab="Densidad",breaks=8)
hist(D$PigmFrAvg[M],freq=F,col=rgb(0.9, 0.1, 0.1, 0.3),add=T,breaks=8)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)

#Graficar el nivel de melanina de acuerdo al sexo.
#Como en el brazo estamos considerando el nivel "genético" (por no estar tan expuesto al sol), no sé incluirá las entradas
#de extranjeros.
D_mex=D[-2,]
num=seq(1,length(D_mex[,1]))
M_mex=num[D_mex$Sexo=="M"]
H_mex=num[D_mex$Sexo=="H"]
#Se agruparán las mediciones por niveles
#freq_PigBrAvg=matrix(data=0,nrow=2,ncol=5)
#for i in 1:length(D[:1]){
#  if(Pig)
#}
hist(D_mex$PigmBrAvg[M_mex],freq=F,xlim=c(34,50),col=rgb(0.9, 0.1, 0.1, 0.3),main='Pigmentación en brazos por sexo ("genético")'
     ,xlab="Nivel de pigmentación",ylab="Densidad",breaks=8)
hist(D_mex$PigmBrAvg[H_mex],freq=F,xlim=c(34,50),col=rgb(0.1, 0.9, 0.1, 0.3),add=T,breaks=8)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)

#Altura
hist(D_mex$Altura[M_mex],freq=F,xlim=c(1.5,1.85),col=rgb(0.9, 0.1, 0.1, 0.3),main='Altura'
     ,xlab="Altura en m",ylab="Densidad",breaks=5)
hist(D_mex$Altura[H_mex],freq=F,xlim=c(1.5,1.85),col=rgb(0.1, 0.9, 0.1, 0.3),add=T,breaks=5)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)
median(D$Altura) # Mediana de altura para ambos sexos
mean(D$Altura) # Promedio de altura para ambos sexos
mean(D_mex$Altura[M_mex])
mean(D_mex$Altura[H_mex])
# contar campos con "M" en la columna de sexo
i=1
count=0
for (field in D$Sexo) {
  if(field == "M"){
      count = count +1
  }
  i = i + 1
}
count # 13

#Calzado
hist(D_mex$Calzado[M_mex],freq=F,xlim=c(23,29),col=rgb(0.9, 0.1, 0.1, 0.3),main='Longitud de calzado'
     ,xlab="Calzado en cm",ylab="Densidad",breaks=6)
hist(D_mex$Calzado[H_mex],freq=F,xlim=c(23,29),col=rgb(0.1, 0.9, 0.1, 0.3),add=T,breaks=6)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)

#Presion sistolica
hist(D_mex$PS_Sis[M_mex],freq=F,xlim=c(90,140),col=rgb(0.9, 0.1, 0.1, 0.3),main='Presion sistolica'
     ,xlab="Presion sistolica (mm Hg)",ylab="Densidad",breaks=8)
hist(D_mex$PS_Sis[H_mex],freq=F,xlim=c(90,140),col=rgb(0.1, 0.9, 0.1, 0.3),add=T,breaks=8)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)

#Presion diastolica
hist(D_mex$PS_Dia[M_mex],freq=F,xlim=c(40,100),col=rgb(0.9, 0.1, 0.1, 0.3),main='Presion diastolica'
     ,xlab="Presion diastolica  (mm Hg)",ylab="Densidad",breaks=6)
hist(D_mex$PS_Dia[H_mex],freq=F,xlim=c(40,100),col=rgb(0.1, 0.9, 0.1, 0.3),add=T,breaks=8)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=10)

#Bronceado
hist(D_mex$PigmFrAvg[M_mex]-D_mex$PigmBrAvg[M_mex],freq=F,xlim=c(-1,20),col=rgb(0.9, 0.1, 0.1, 0.3),main='Pigmentación en brazos por sexo ("genético")'
     ,xlab="Nivel de pigmentación",ylab="Densidad",breaks=8)
hist(D_mex$PigmFrAvg[H_mex]-D_mex$PigmBrAvg[H_mex],freq=F,xlim=c(-1,20),col=rgb(0.1, 0.9, 0.1, 0.3),add=T,breaks=8)
legend("topright", c("Mujeres", "Hombres"), col=c(rgb(0.9, 0.1, 0.1, 0.3),rgb(0.1, 0.9, 0.1, 0.3)), lwd=8)

##############  PLOTS DE REGIONES DEL PAIS #################################################
region = rep(1,31)
region[D$Region1 == "Central"] = 2
region[D$Region1 == "Sur"] = 3
# Pigmentación Avg de la frente
plot(region,D$PigmFrAvg, xlab = "Región del país", ylab = "Pigmentación (M value)",
       main='Pigmentación promedio de la frente por regiones del país', col=region)
legend(x=2.5, y=45 ,c("Norte","Central","Sur"), col = c(1,2,3), lwd=4)

# Pigm Avg del brazo
plot(region,D$PigmBrAvg, xlab = "Región del país", ylab = "Pigmentación (M value)",
     main='Pigmentación promedio del brazo por regiones del país', col=region)
legend(x=2.5, y=45 ,c("Norte","Central","Sur"), col = c(1,2,3), lwd=4)

# Altura
plot(region,D$Altura, xlab = "Región del país", ylab = "Altura (metros)",
     main='Altura por regiones del país', col=region)
legend(x=2.5, y=1.8 ,c("Norte","Central","Sur"), col = c(1,2,3), lwd=4)

# Calzado
plot(region,D$Calzado, xlab = "Región del país", ylab = "Largo del pie (cm)",
     main='Calzado por regiones del país', col=region)
legend(x=2.5, y=29 ,c("Norte","Central","Sur"), col = c(1,2,3), lwd=4)

# Presion sistólica
plot(region,D$PS_Sis, xlab = "Región del país", ylab = "Presion sistólica (mm Hg)",
     main='Presion sistólica por regiones del país', col=region)
legend(x=1, y=135 ,c("Norte","Central","Sur"), col = c(1,2,3), lwd=4)

# Presion diastólica
plot(region,D$PS_Sis, xlab = "Región del país", ylab = "Presion diastólica (mm Hg)",
     main='Presion diastólica por regiones del país', col=region)
legend(x=1, y=135 ,c("Norte","Central","Sur"), col = c(1,2,3), lwd=4)
