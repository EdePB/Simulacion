tcontagios<-c()
Env<-data.frame()
for(pi in c(0.02,0.04,0.6,0.8)) {
    source('~/GitHub/SimulacionComputacional/P6/Paralelo.R', encoding = 'UTF-8')
  
  Env= cbind( pi,epidemia)
}  
Ev<-data.frame()

for(pv in c(0.02, 0.04,0.6, 0.8)) {
  pv=p/100
  
  source('~/GitHub/SimulacionComputacional/P6/Vacuna.R', encoding = 'UTF-8')
  
  Ev= cbind( pv, epidemia)
}  
tcontagios<-  rbind(Env, Ev)

png("boxplotPI.png",width = 800, height = 800,pointsize = 20)
boxplot(data=tcontagios, epidemia~pi, xlab="pi", ylab="epidemia",col=c("Blue","Red","green","yellow","purple"))
graphics.off()