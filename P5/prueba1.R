f <- function(x) { return(1 / (exp(x) + exp(-x)))}
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
muestra <- generador(50000) # sacamos una muestra

desde <- 3 #limites de la integral
hasta <- 7
pedazo <- 500
resultados<-data.frame()
wa<-0.04883411112604931084064237220194267497311653701062409984

parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for(cuantos in seq(200,1000,200)){
  for(corridas in 1:20){
    iTime<-Sys.time()
    montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
    integral <- sum(montecarlo) / (cuantos * pedazo)
    estimado=((pi / 2) * integral)
    print(estimado)
    print(cuantos)
    dif=abs(wa-estimado)
    fTime<-Sys.time()
    tiempo<-(fTime-iTime)
    stopImplicitCluster()
    resultados=rbind(resultados,c(cuantos,dif, tiempo))
  }
}
names(resultados)=c("muestras","error","tiempo")
resultados$muestras=as.factor(resultados$muestras)
png("res.png",width=2000, height = 1200, pointsize = 20)
split.screen(c(2,1))
screen(1)
boxplot(data=resultados, tiempo~muestras, xlab="No.muestra", ylab="Tiempo(s)", main="Tiempo de simulación", ylin=c(0,1), col=(c("blue","green","gray", "yellow","purple")))
screen(2)
boxplot(data=resultados, error~muestras, xlab="No.muestra", ylab="Diferencia", main="Error en aproximación", col=(c("blue","green","gray", "yellow","purple")))
dev.off()