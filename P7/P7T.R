f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -2
high <- 2
step <- 0.25
replicas <- 100
best<-c()
t=35
WA<-0.0666822
po<-function (){
      datos=data.frame()
      resultados=data.frame()
      currx <- runif(1, low, high)
      curry <- runif(1, low, high)
      best <- c(currx, curry)
    for (tiempo in 1:t) {
        delta <- runif(1, 0, step)
        left <- currx - delta
        right <- currx + delta
        down <- curry - delta
        up <- curry + delta
        
        if (f(left,curry) > f(right,curry)) {
          bestx <- c(left,curry)
        } else {
          bestx <- c(right,curry)
        }
        if (f(currx, up) > f(currx, down)) {
          besty <-c(currx,up)
        } else {
          besty <- c(currx,down)
        }
        if(f (bestx[1],bestx[2])> f(besty[1], besty[2])){
          currx<-bestx[1]
          curry<-bestx[2]
        }else{
          currx<-besty[1]
          curry<-besty[2]
        }
        if (f(currx, curry) > f(best[1],best[2])) {
          best <- c(currx,curry)
        }
        datos=cbind(i,tiempo, currx, curry,f(best[1], best[2])) 
        resultados=rbind(resultados,datos)
      }
      return(resultados)
  }


suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
y <- foreach(i = 1:10, .combine=rbind) %dopar% po()

stopImplicitCluster()

names(y)=c("corrida","tiempo","posx","posy", "Fevaluada")
y$corrida=as.factor(y$corrida)
#png("p7_2de.png",width=700, height = 700)
library(lattice) # lo mismo aplica con este paquete
png("p7_T.png", width=600, height=500)

xyplot(data=y,Fevaluada~tiempo, xlab="Pasos",ylab="f(x,y)",groups=corrida,
       panel=function(x,y,subscripts,groups) {
         panel.xyplot(x,y)
                    panel.stripplot(x,y,groups = groups,subscripts=subscripts, type="o", pch=16)
          panel.abline(h=WA,col="green", lwd=2)
          }
)
graphics.off()