timei<-Sys.time()

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
#domin.by <- function(target, challenger, total) {
#  if (sum(challenger < target) > 0) {
#    return(FALSE) # hay empeora
#  } # si no hay empeora, vemos si hay mejora
#  return(sum(challenger > target) > 0)
#}

datos<-read.csv(file="exp.csv", head=TRUE, sep = ",")

reg<-data.frame()

exponentes<-3

columna<-dim(datos)[2]
for (i in seq(2,columna,1)){
  
  lineal<-lm(datos[,i] ~datos[,1], datos)
  exponencial<-lm(log(datos[,i])~datos[,1],datos)
  polinomial<-lm(datos[,i] ~ poly(datos[,1],exponentes),datos)
  
  lin<-cbind(colnames(datos)[i],"Lineal",summary(lineal)$r.squared,summary(lineal)$coefficients[2,1],
             0,0,summary(lineal)$coefficients[1,1])
  
  exp<-cbind(colnames(datos)[i],"Exponencial",summary(exponencial)$r.squared,summary(exponencial)$coefficients[2,1],
             0,0,summary(exponencial)$coefficients[1,1])
  
  poli<-cbind(colnames(datos)[i],"Polinomial",summary(polinomial)$r.squared,summary(polinomial)$coefficients[2,1],
              summary(polinomial)$coefficients[3,1],summary(polinomial)$coefficients[4,1],summary(exponencial)$coefficients[1,1])
  
  reg<-rbind(reg,lin,exp,poli)
}
colnames(reg)<-c("Caracteristica","Regresion","Rcuadrada","C.lineal",
                        "C.cuadratico","C.cubico","Interseccion")

reg$Rcuadrada<-as.numeric(levels(reg$Rcuadrada))[reg$Rcuadrada]
reg$C.lineal<-as.numeric(levels(reg$C.lineal))[reg$C.lineal]
reg$C.cuadratico<-as.numeric(levels(reg$C.cuadratico))[reg$C.cuadratico]
reg$C.cubico<-as.numeric(levels(reg$C.cubico))[reg$C.cubico]
reg$Interseccion<-as.numeric(levels(reg$Interseccion))[reg$Interseccion]

n<-200
Vi<-runif(n,800,4500) 

funciones<-function(j){
#for(j in 2:3){
 res<-c()
  nombre<-colnames(datos)[j]
  temporal<-reg[reg$Caracteristica==nombre,]
  f<-temporal[which.max(temporal$Rcuadrada),]
  
  for(k in 1:n){
    V<-Vi[k]
    
    if (f$Regresion=="Polinomial"){y=(f$C.lineal*V) +(f$C.cuadratico*(V**2))
    + (f$C.cubico*(V**3))+ f$Interseccion
    print("p")
    print(y)} 
    
    if(f$Regresion=="Lineal"){y=(f$C.lineal*V) + f$Interseccion
    print("l")} 
    
    if(f$Regresion=="Exponencial"){y=f$C.lineal*log(V) + f$Interseccion
    print("e")}
    
    res<-c(res,y)
  }
  
  return(res)
  
}

Soluciones<-foreach(j=2:3,.combine = cbind)%dopar%funciones(j)
stopImplicitCluster()


png("p_inicial.png")
plot(Soluciones[,1], Soluciones[,2], xlab="diametro (minimizar)", ylab="voltaje (minimizar)", main="Ejemplo bidimensional")
graphics.off()

#hay que ponerle signo a las evaluaciones
sign<-c(-1, -1)

mejordiam <- which.max(sign[1] * val[,1])#elige el maximom sign por evaluacion
mejorvolt <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")#como elige cual maximiza y cual minimiza
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
png("p11_init.png")
plot(val[,1], val[,2], xlab="distancia (min)", ylab="voltaje (min)", main="Relación de parámetros")
graphics.off()
png("p11_mejores.png")
plot(val[,1], val[,2], xlab=paste("distancia (mejor con cuadro azul)"),
     ylab=paste("voltaje (mejor con bolita naranja)"),
     main="Relación de parámetros")
points(val[mejordiam, 1], val[mejordiam, 2], col="blue", pch=15, cex=1.5)
points(val[mejorvolt, 1], val[mejorvolt, 2], col="orange", pch=16, cex=1.5)
graphics.off()
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
no.dom<-rbind(no.dom,foreach(i = 1:n, .combine=rbind)%dopar% p3(i))


frente <- subset(val, no.dom) # solamente las no dominadas
png("p11_frente.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Frente de Pareto")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()


#library(ggplot2) # recordar instalar si hace falta
#data <- data.frame(pos=rep(0, n), dom=dominadores)
#png("p11_violin.png")
#gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
#gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
#  xlab("") +
#  ylab("Frecuencia") +
#  ggtitle("Cantidad de soluciones dominantes")
#graphics.off()
timef<-Sys.time()
tiempo<-(timef-timei)
print(tiempo)