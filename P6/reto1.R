tcontagios<-data.frame()
Env<-numeric()
for(i in 1:10) {
  
  source('~/GitHub/SimulacionComputacional/P6/Paralelo.R', encoding = 'UTF-8')
  print(max(epidemia))
  Env <- rbind(Env, max(epidemia))
}  
Ev<-numeric()
for(i in 1:10) {
  
  
  source('~/GitHub/SimulacionComputacional/P6/Vacuna.R', encoding = 'UTF-8')
  print(max(epidemia))
  Ev <- rbind(Ev,  max(epidemia))
}  
tcontagios<-  cbind(Env, Ev)
colnames(tcontagios)=c("Sin vacuna","Con Vacuna")
png("R1p6.png")
boxplot(tcontagios, ylab="Máximo de infectados" , col=(c("yellow","blue")))
graphics.off()