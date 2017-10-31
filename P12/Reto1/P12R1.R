suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Tiempos<-data.frame()

Tp<-numeric()

npr<-seq(0.09,0.99, 0.1)
gpr<-seq(0.09,0.99, 0.1)
bpr<-seq(0.09,0.99, 0.1)

for(negro in npr){
  for(gris in gpr) {
    for(blanco in bpr){
    

    
    source('~/GitHub/SimulacionComputacional/P12/codigoP12R1.R', encoding = 'UTF-8')
    
    Tp <- cbind(blanco, negro, gris, porcentaje)
    
    print(Tp)
    
    Tiempos<-  rbind(Tiempos,Tp)
  }  
  
  }
}
stopImplicitCluster()
save.image(file="datosR12_R1.RData")
colnames (Tiempos)= c("Tipo","Tiempo","Prueba", "porcentaje", "corrida")
Tiempos$Tipo=as.factor(Tiempos$Tipo)
Tiempos$Tiempo=as.numeric(levels(Tiempos$Tiempo))[Tiempos$Tiempo]
#Tiempos[Tiempos$Tiempo>10,2]<-Tiempos[Tiempos$Tiempo>10,2]/60

Tiempos$Prueba=as.factor(Tiempos$Prueba)
 
library(ggplot2)
png("p12R1_BN1.png")
ggplot(data=Tiempos, aes(x=negro, y=blanco)) +
  scale_fill_gradient(low="white", high="blue")+
  theme(legend.title = element_text(colour="black", size=15))+
  theme(legend.text = element_text(colour="black", size = 15))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=18)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=18)) +
  geom_raster(aes(fill=porcentaje))
  graphics.off()
  
  png("p12R1_NG1.png")
  ggplot(data=Tiempos, aes(x=negro, y=gris)) + 
    scale_fill_gradient(low="white", high="blue")+
    theme(legend.title = element_text(colour="black", size=15))+
    theme(legend.text = element_text(colour="black", size = 15))+
    theme(axis.title.x = element_text(face="bold", colour="black", size=18)) +
    theme(axis.title.y = element_text(face="bold", colour="black", size=18)) +
    geom_raster(aes(fill=porcentaje))
  graphics.off()
  
  png("p12R1_BG1.png")
  ggplot(data=Tiempos, aes(x=blanco, y=gris)) + 
    scale_fill_gradient(low="white", high="blue")+
    theme(legend.title = element_text(colour="black", size=15))+
    theme(legend.text = element_text(colour="black", size = 15))+
    theme(axis.title.x = element_text(face="bold", colour="black", size=18)) +
    theme(axis.title.y = element_text(face="bold", colour="black", size=18)) +
    geom_raster(aes(fill=porcentaje))
  graphics.off()
  
  Tiempos$gris=as.numeric(Tiempos$gris)
  ggplot()+
      geom_point(data=Tiempos, aes(x=blanco, y=negro, color=porcentaje, size=gris))+
    #scale_fill_gradient(low="white", high="blue")+
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Relación de porcentajes")
