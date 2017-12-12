f <- function(x) { return(1 / (exp(x) + exp(-x)))}
  suppressMessages(library(distr))
  resultados<-data.frame()
      g <- function(x) { return((2 / pi) * f(x)) }
    generador  <- r(AbscontDistribution(d = g)) # creamos un generador
    muestra <- generador(50000) # sacamos una muestra
    desde <- 3 #limites de la integral
    hasta <- 7
    pedazo <- 500
    #cuantos <- 5
    for(cuantos in seq(5,10,5)){
      for(corridas in 1:5){
        iTime<-Sys.time()
      parte <- function() {
        valores <- generador(pedazo)
        return(sum(valores >= desde & valores <= hasta))
      }
      suppressMessages(library(doParallel))
      registerDoParallel(makeCluster(detectCores() - 1))
      montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
      
      integral <- sum(montecarlo) / (cuantos * pedazo)
      estimado=((pi / 2) * integral)
      wa<-0.04883411112604931084064237220194267497311653701062409984
      print(estimado)
      print(cuantos)
      dif=abs(wa-estimado)
      fTime<-Sys.time()
      tiempo<-(fTime-iTime)
      stopImplicitCluster()
      resultados=rbind(resultados,c(cuantos,dif, tiempo))
    }
    }
  names(resultados)=c("corridas","error","tiempo")
  resultados$corridas=as.factor(resultados$corridas)
  png("res.png",width=1200, height = 1800, pointsize = 20)
  split.screen(c(1,2))
  screen(1)
 boxplot(data=resultados, tiempo~corridas, xlab="No.corridas", ylab="Tiempo")
  screen(2)
  boxplot(data=resultados, error~corridas, xlab="No.corridas", ylab="Muestra")
 #graphics.off()