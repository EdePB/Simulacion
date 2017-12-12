Tiempos<-data.frame()
Tnp<-numeric()

Tp<-numeric()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for(corrida in 1:5) {
  for(n in seq(100, 200, 50)){


    source('~/GitHub/SimulacionComputacional/P11/originalP11.R', encoding = 'UTF-8')
  
  Tnp <- cbind("original", tiempo, k, n, corrida)
  #hacer violinplot
  #png(paste("p11s_",corrida, k,n,".png"), width=600, height=300)
  #plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
  #points(1:tmax, mejores, pch=15)
  #abline(h=optimo, col="green", lwd=3)
  #graphics.off()
  
  source('~/GitHub/SimulacionComputacional/P11/paraleloP11.R', encoding = 'UTF-8')

  
  Tp <- cbind("paralelo",tiempo, k, n, corrida)
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
save.image(file="datosR11_1.RData")
colnames (Tiempos)= c("Tipo","Tiempo","Objetivos", "Soluciones", "corrida")
Tiempos$Tipo=as.factor(Tiempos$Tipo)
Tiempos$Tiempo=as.numeric(levels(Tiempos$Tiempo))[Tiempos$Tiempo]
#Tiempos[Tiempos$Tiempo>10,2]<-Tiempos[Tiempos$Tiempo>10,2]/60

Tiempos$Objetivos=as.factor(Tiempos$Objetivos)

Tiempos$Soluciones=as.factor(Tiempos$Soluciones)
library(ggplot2)
png("p11R1_plot1.png")
ggplot(data=Tiempos, aes(x=Objetivos, y=Tiempo, color=Tipo)) + 
guides(color=guide_legend(title = NULL))+
  geom_boxplot()+ facet_grid(Soluciones~.)
  ggtitle("Tiempos comparados variando soluciones y k")
graphics.off()