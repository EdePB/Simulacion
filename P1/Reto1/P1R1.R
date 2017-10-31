timei<-Sys.time()
repetir <- 100
duracion <- seq(200,600,200)

library(parallel)

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "repetir")
datos <-  data.frame()
for(replica in 1:5){
for (dimension in 1:8) {
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir,
                         function(r) {
                           pos <- rep(0, dimension)
                           origen=rep(0,dimension)
                           
                           contador=0
                           for (t in 1:duracion) {
                             cambiar <- sample(1:dimension, 1)
                             cambio <- 1
                             if (runif(1) < 0.5) {
                               cambio <- -1
                             }
                             pos[cambiar] <- pos[cambiar] + cambio
                           }
                             if (all(pos==origen)) {
                               contador=contador +1 
                             }
                           
                           return(contador)
                         })
  res<-cbind(replica, duracion, repetir,dimension, sum(resultado))
  datos <- rbind(datos, res)
  #resultado<-cbind(resultado,res)
}
  
  }
stopCluster(cluster)
colnames(datos)<-c("Replica", "Pasos","Repeticiones","Dimension","Regresos")
datos$Repeticiones<-as.factor(datos$Repeticiones)
datos$Dimension<-as.factor(datos$Dimension)
datos$Pasos<-as.factor(datos$Pasos)

library(ggplot2)
ggplot(data=datos, aes(x = Dimension, y= Regresos, color=Pasos)) +
  geom_boxplot(position = position_dodge(1))+
   ylab("Veces en origen")+
ggsave("P1R1_plot.png")
timef<-Sys.time()
tiempo<-(timef-timei)
print(tiempo)