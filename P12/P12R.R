Tiempos<-data.frame()
Tnp<-numeric()

Tp<-numeric()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for(corrida in 1:10) {
for(pr in seq(2000, 5000, 1000)){
 


  source('~/GitHub/SimulacionComputacional/P12/originalP12.R', encoding = 'UTF-8')
  
  Tnp <- cbind("original", tiempo, pr,porcentaje,  corrida)
  #hacer violinplot
  #png(paste("p11s_",corrida, k,n,".png"), width=600, height=300)
  #plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
  #points(1:tmax, mejores, pch=15)
  #abline(h=optimo, col="green", lwd=3)
  #graphics.off()
  
  source('~/GitHub/SimulacionComputacional/P12/paraleloP12.R', encoding = 'UTF-8')
  
  
  Tp <- cbind("paralelo",tiempo, pr, porcentaje, corrida)
  #hacer violinplot
  #png(paste("p11p_",corrida,init,".png"), width=600, height=300)
  #plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
  #points(1:tmax, mejores, pch=15)
  #abline(h=optimo, col="green", lwd=3)
  #graphics.off()
  
  Tiempos<-  rbind(Tiempos,Tnp, Tp)
    }  
  
}
stopImplicitCluster()
#save.image(file="datosR11_1.RData")
colnames (Tiempos)= c("Tipo","Tiempo","Prueba", "porcentaje", "corrida")
Tiempos$Tipo=as.factor(Tiempos$Tipo)
Tiempos$Tiempo=as.numeric(levels(Tiempos$Tiempo))[Tiempos$Tiempo]
#Tiempos[Tiempos$Tiempo>10,2]<-Tiempos[Tiempos$Tiempo>10,2]/60

Tiempos$Prueba=as.factor(Tiempos$Prueba)

library(ggplot2)
#png("p11R1_plot1.png")
ggplot(data=Tiempos, aes(x=Prueba, y=Tiempo, color=Tipo)) + 
guides(color=guide_legend(title = NULL))+
  geom_boxplot()+ 
  ggtitle("Tiempos comparados con diferente tamaño de prueba")
#graphics.off()