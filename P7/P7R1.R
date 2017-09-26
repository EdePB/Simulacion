f <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}
#png("p7_2d.png", width=700, height=700)
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, f)
colnames(z)=x
rownames(z)=y
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
d <- melt(z)
names(d) <- c("x", "y", "z")
#library(lattice) # lo mismo aplica con este paquete
#png("p7_flat_2.png", width=500, height=500)
#g1<-levelplot(z ~ x * y, data = d)
#graphics.off()
library(ggplot2)
g1<-ggplot(d, aes(x, y)) +
  geom_raster(aes(fill =z))+
scale_fill_gradient(low="red", high="white")
low <- -2
high <- 2
step <- 0.25
best<-c()
t=50
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
names(y)=c("corrida","tiempo","posx","posy", "f(x)")
y$corrida=as.factor(y$corrida)
for(tiempo in 1:t){
  g<-y[y$tiempo==tiempo,]
  g1+geom_point(data=g, aes(g$posx,g$posy, color=corrida))
  #ggsave(paste("p7R1",tiempo,".png"))
 }
