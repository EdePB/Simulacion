Tiempos<-data.frame()
Tnp<-numeric()

Tp<-numeric()
for(corrida in 1:5) {
for(k in seq(100000,200000,50000)){


    source('~/GitHub/SimulacionComputacional/P8/codigobase.R', encoding = 'UTF-8')
  
  Tnp <- cbind("original", tiempo, k, corrida)

  source('~/GitHub/SimulacionComputacional/P8/prueba2paralelo.R', encoding = 'UTF-8')
  
  Tp <- cbind("paralelo",tiempo, k, corrida)
  
  Tiempos<-  rbind(Tiempos,Tnp, Tp)
  }  
  
}
save.image(file="datosR1_1.RData")
colnames (Tiempos)= c("Tipo","Tiempo","k", "corrida")
Tiempos$Tipo=as.factor(Tiempos$Tipo)
Tiempos$Tiempo=as.numeric(levels(Tiempos$Tiempo))[Tiempos$Tiempo]
Tiempos[Tiempos$Tiempo>30,2]<-Tiempos[Tiempos$Tiempo>30,2]/60
Tiempos$k=as.numeric(levels(Tiempos$k))[Tiempos$k]
Tiempos$k<-Tiempos$k/1000
Tiempos$k=as.factor(Tiempos$k)
library(ggplot2)
png("p8R1_plot1.png")
ggplot(data=Tiempos, aes(x=k, y=Tiempo, fill=Tipo)) + 
  geom_boxplot()+
  scale_y_continuous(name="Tiempo (min)") +
  scale_x_discrete(name="Tama\u{00f1}o de c\u{00fa}mulos")+
ggtitle("Tiempos comparados variando k")
graphics.off()