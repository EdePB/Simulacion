load("C:/Users/ede_p/OneDrive/Documentos/GitHub/SimulacionComputacional/P8/datosR1_1.RData")
colnames(Tiempos)<-c("Tipo","Tiempo","k","corrida")
Tiempos$Tiempo<-as.numeric(levels(Tiempos$Tiempo))[Tiempos$Tiempo]
Tiempos[Tiempos$Tiempo>30,4]<-Tiempos[Tiempos$Tiempo>30,4]/60

for (k in c(100,150,200)){
  tstparalelo<-Tiempos[Tiempos$k==k& Tiempos$Tipo=="paralelo",]
  tstoriginal<-Tiempos[Tiempos$k==k & Tiempos$Tipo=="original",]
  
  datospara<-tstparalelo$Tiempo
  datosorig<-tstoriginal$Tiempo
  
  pval<-t.test(datosorig,datospara)
  print(pval)
}