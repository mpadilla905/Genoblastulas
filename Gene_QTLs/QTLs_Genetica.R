# Mediciones de traits poligenicos: QTLS 
# Tarea grupal Genetica

# Notas: Estado_Nat se refiere al estado de nacimiento
#        Estado_Dura se refiere al estado donde ha vivido mas la persona
#        Region 1 solo sn Norte, Central y Sur
#        Region 2 lo mismo pero con Central dividido en Occidente, Oriente, Central Norte y Central Sur
#        Altura esta medida en metros y Calzado en cm

rm(list=ls())
setwd("~/Downloads")
setwd("~/Documents/")
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

####################################
#    ...     #
####################################
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
D
