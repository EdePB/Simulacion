suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
Datos<-data.frame()
Tp<-numeric()

for(corrida in 1:50) {
for(k in seq(2, 16, 2)){
  for(n in c(200)){

  
  source('~/GitHub/SimulacionComputacional/P11/paraleloP11.R', encoding = 'UTF-8')

  
  Tp <- cbind(dim(frente)[1], k, n)
  
  
Datos<-  rbind(Datos, Tp)
    }  
  }
}
stopImplicitCluster()

colnames (Datos)= c("Dominantes","Objetivos", "Soluciones")
Datos$Objetivos=as.factor(Datos$Objetivos)

library(ggplot2)
#png("p11T_violinplot3.png",width = 700, height = 350)
ggplot(data=Datos, aes(x=Objetivos, y=(Dominantes/n)*100)) + 
  geom_violin(scale="width",fill="darkolivegreen1", color="black")+
  geom_boxplot(width=0.2, fill="dodgerblue3", color="darkblue")+ 
  xlab("Funciones objetivo") +
  ylab("Porcentaje de soluciones dominantes")+
  ggtitle("Cantidad de soluciones dominantes")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p11T_violinplot4.png",width = 10, height = 5)