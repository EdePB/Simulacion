Tiempos<-data.frame()
Tnp<-numeric()
for(i in 1:10) {
 
  source('~/GitHub/SimulacionComputacional/P6/NoParalelo.R', encoding = 'UTF-8')
  
  Tnp <- rbind(Tnp, tiempo)
}  
Tp<-numeric()
for(i in 1:10) {
  
  source('~/GitHub/SimulacionComputacional/P6/Paralelo.R', encoding = 'UTF-8')
  
  Tp <- rbind(Tp,  tiempo)
}  
Tiempos<-  cbind(Tnp, Tp)
colnames(Tiempos)=c("No paralelizado","Paralelizado")
png("p6R.png")
boxplot(Tiempos, ylab="Tiempo(s)", main="Tiempos comparados", col=(c("purple","red")))
graphics.off()