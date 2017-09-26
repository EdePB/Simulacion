f <- function(x) {
  return(cos(14.5*x - 0.3) + x * (x + 0.2) + 1.01)
}

low <- -3
high <- -low

tmax <- 100

x <- runif(1, low, high)
best <- x
step <- .1
datos=data.frame()
resultados= data.frame()
for (tiempo in 1:tmax) {
  delta <- runif(1, 0, step)
  xpr<- x + delta
  fxpr<- f(xpr)
  fx <- f(x)
  if (fxpr > fx) {
    x <- xpr
  } 
  if (f(x) < f(best)) { # minimizamos
    best <- x
    
  }
  datos=cbind(tiempo,fx,fxpr,delta, best)
  resultados=rbind(resultados,datos)
}
names(resultados)=c("Tiempo","fx","fxpr","delta","posx")
#paso$Xi=as.factor(paso$Xi)
library(ggplot2)

g1<-ggplot(resultados, aes(x=Tiempo, y=fx, color="red")) +
  geom_line() +
  geom_point(size=2)+
  xlab("Tiempo") + ylab("f(x)")
