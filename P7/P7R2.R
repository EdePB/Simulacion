f <- function(x) {
  return(cos(14.5*x - 0.3) + x * (x + 0.2) + 1.01)
}
#x <- seq(-3, 3, 0.05)
#png("p7_1d.png", width=500, height=400)
#plot(x, f(x), type="l")
#graphics.off()


low <- -3
high <- 3
step <- 1

t=100
#temp=10
#j=0.99

resultados=data.frame()
datos=data.frame()
mejor=data.frame()
paso=data.frame()

for(j in c(0.99,0.95, 0.91)){
for(temp in seq(10,100,10)){
  reg=cbind(temp)
for (tiempo in 1:t) {
  x <- runif(1, low, high)
  best=x
  delta <- runif(1, 0, step)
  xpr<- x + delta
  d=f(xpr)-f(x)
  p=exp(-d/temp)
  
  datos=cbind(tiempo,x,xpr,temp)
  if (d > 0) {
    x=xpr
  } else {
      if (runif(1) < p) {
       x=xpr
      temp=temp*j
    }  
    
  }
  
  datos=cbind(datos,d,temp,p,x,f(x)) 
  resultados=rbind(resultados,datos)
}  
  mejor=cbind(j,reg,temp,max(resultados[9]))
 paso=rbind(paso,mejor)
   }
}
names(paso)=c("Xi","Tempi","Temperatura","Vmaximo")
paso$Xi=as.factor(paso$Xi)
library(ggplot2)

g1<-ggplot(paso, aes(x=Tempi, y=Vmaximo,color=Xi)) +
  geom_line() +
  geom_point(size=2)+
  xlab("Temperatura inicial") + ylab("Valor máximo")
#ggsave("p7R2.png")