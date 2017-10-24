pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

vc <- 4
md <- 3
tc <- 5
k <- 2 # cuantas funciones objetivo
obj <- list()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

#######################paralelizar objetivo
#for (i in 1:k)
#p1<-function (i){
#  return (poli(md,vc, tc))
#}
obj<- foreach(i = 1:k, .combine=) %dopar% poli(md,vc, tc)

#######################
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200 # cuantas soluciones aleatorias

#############################paralelizar valores
#for (i in 1:n) { # evaluamos las soluciones
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)


p2<-function(i){
  valores<-double()
  for (j in 1:k) { # para todos los objetivos
    ev<-eval(obj[[j]], sol[i,], tc)
    valores<-cbind(valores, ev)
    
  }
  return(valores)
}
val<-foreach(i = 1:n, .combine=rbind) %dopar% p2(i)

cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")

no.dom <- logical()
dom <- integer()
###################paralelizar dominantes
#for (i in 1:n) {
p3<-function(i){ 
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  #return( cuantos)
  return( cuantos == 0) # nadie le domina
}
no.dom<-foreach(i = 1:n, .combine=c)%dopar% p3(i)
stopImplicitCluster()
#################
frente <- subset(val, no.dom) # solamente las no dominadas
dimfrente<-dim(frente)[1]
val<-as.data.frame(val)
frente<-as.data.frame(frente)

library(ggplot2) 

png("p11_frente.png")
ggplot()+
  geom_point(data = val, aes(x=val[,1], y=val[,2]),size=2)+
  xlab(xl) +
  ylab(yl) +
    geom_point(data = frente, aes(x=frente[,1],y=frente[,2]), color="blue", size=4)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Frente")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        plot.title = element_text(size=18,face="bold"))
graphics.off()
  
  if(dimfrente>2){
    
    ordenf<-frente[order(frente[,1]),]
  
  dis<-c()
  for(i in 1:dimfrente-1){
    d<- sqrt((ordenf[i,1]-ordenf[i+1,1])**2+(ordenf[i,2]-ordenf[i+1,2])**2)
    dis<-c(dis,d)
  }
  umbral<-mean(dis)
  nuevo<-rep(FALSE,dimfrente)
  for(i in 1:dimfrente){
    if (ordenf[i,]==head(ordenf,n=1)||ordenf[i,]==tail(ordenf,n=1)){
      nuevo[i]=TRUE
      }else{
        j<-max(which(nuevo))
        d<-sqrt((ordenf[i,1]-ordenf[j,1])**2+(ordenf[i,2]-ordenf[j,2])**2)
        if(d>=umbral){
          nuevo[i]=TRUE
        }else{nuevo[i]=FALSE
          }
      }
  }
  mejor<-subset(ordenf,nuevo)
  png("p11_frente_mod.png")
    ggplot()+
    geom_point(data = val, aes(x=val[,1], y=val[,2]),size=2)+
    xlab(xl) +
    ylab(yl) +
    geom_point(data = frente, aes(x=frente[,1],y=frente[,2]), color="blue", size=4)+
    geom_point(data = mejor, aes(x=mejor[,1],y=mejor[,2]), color="red", size=4)+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=16),
            plot.title = element_text(size=18,face="bold"))+
    ggtitle("Diversificación de frente")
  graphics.off()
    png("p11_nuevofrente.png")
    ggplot()+
      geom_point(data = val, aes(x=val[,1], y=val[,2]),size=2)+
      xlab(xl) +
      ylab(yl) +
      geom_point(data = mejor, aes(x=mejor[,1],y=mejor[,2]), color="red", size=4)+
      theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Nuevo frente")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            plot.title = element_text(size=14,face="bold"))
      
    ggsave("p11_nuevofrente.png",width = 10, height = 5)
  }
