Tiempos<-data.frame()
Tnp<-numeric()
for(i in 1:5) {
 
  source('~/GitHub/SimulacionComputacional/P8/codigobase.R', encoding = 'UTF-8')
  
  Tnp <- rbind(Tnp, tiempo)
}  
Tp<-numeric()
for(i in 1:5) {
  
  source('~/GitHub/SimulacionComputacional/P8/prueba2paralelo.R', encoding = 'UTF-8')
  
  Tp <- rbind(Tp,  tiempo)
}  
Tiempos<-  cbind(Tnp, Tp)
colnames(Tiempos)=c("No paralelizado","Paralelizado")
png("p8R10.png")
boxplot(Tiempos, ylab="Tiempo(s)", main="Tiempos comparados", col=(c("purple","red")))
graphics.off()