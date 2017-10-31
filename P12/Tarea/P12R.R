suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Tiempos<-data.frame()
Tnp<-numeric()

Tp<-numeric()

for(pr in seq(600, 1200, 200)){
  for(corrida in 1:5) {
 
 
   source('~/GitHub/SimulacionComputacional/P12/originalP12.R')
  
  Tnp <- cbind("original", tiempo, pr,porcentaje,  corrida)
  
  
  
  source('~/GitHub/SimulacionComputacional/P12/paraleloP12.R')
  
  Tp <- cbind("paralelo",tiempo, pr, porcentaje, corrida)
  
  
  Tiempos<-  rbind(Tiempos,Tnp, Tp)
    }  
  
}
stopImplicitCluster()
save.image(file="datosR12_2.RData")
colnames (Tiempos)= c("Tipo","Tiempo","Prueba", "porcentaje", "corrida")
Tiempos$Tipo=as.factor(Tiempos$Tipo)
Tiempos$Tiempo=as.numeric(levels(Tiempos$Tiempo))[Tiempos$Tiempo]
#Tiempos[Tiempos$Tiempo>10,2]<-Tiempos[Tiempos$Tiempo>10,2]/60

Tiempos$Prueba=as.factor(Tiempos$Prueba)

library(ggplot2)
#png("p11R1_plot2.png")
ggplot(data=Tiempos, aes(x=Prueba, y=Tiempo, color=Tipo)) + 
guides(color=guide_legend(title = NULL))+
  geom_boxplot()+ 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Tiempos comparados con diferente tamaño de prueba")
#graphics.off()

png("p12R1_por.png")
Tiempos$porcentaje=as.numeric(levels(Tiempos$porcentaje))[Tiempos$porcentaje]
ggplot(data=Tiempos, aes(x=Prueba, y=porcentaje, color=Tipo))+
  guides(color=guide_legend(title = NULL))+
  geom_boxplot()+ 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Porcentaje de error")
graphics.off()