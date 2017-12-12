
fd<-function(dis){
  d=0.7926*(dis^-2.6561)
  return(d)
}
fc<-function(conc){
  d=0.9304*conc-0.1038
  return(d)
}
fv<-function(volt){
  d=0.2029*volt+0.7544
  return(d)
}
datos<-data.frame()
resultados<-data.frame()

soluc<-runif(200)

for(i in 1:200){
  dist<-soluc[i]
  datos=cbind(fd(dist), fv(dist), dist)
  resultados=rbind(resultados,datos)
  
}
png("p11_inicial.png")
plot(resultados[,1], resultados[,2], xlab="distancia (maximizar)", ylab="voltaje (minimizar)", main="Ejemplo bidimensional")
graphics.off()

minim <- (resultados > 0.5)
#lo que buscamos es menor voltaje y diametro con mayor concentración, las soluciones seran las concentraciones
#distancia y voltaje en base a la concentracion
poli <- function(coef, variab, expo) {
  f <- data.frame(coef=integer(), variab=integer(), expo=integer())
 
    cf <-0.7926
    deg <-2.6561 
    f <-  rbind(f, cf,var , deg)
  
  names(f) <- c("coef", "variable", "degree")
  return(f)
}

vc <- 4
md <- 3
tc<- 1
k <- 2 # cuantas funciones objetivo

funciones<-data.frame()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
obj<- foreach(i = 1:k, .combine=) %dopar% poli(md,vc, tc)

stopImplicitCluster()
