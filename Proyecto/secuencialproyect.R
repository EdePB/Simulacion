timei<-Sys.time()

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

datos<-read.csv(file="exp.csv", head=TRUE, sep = ",")

reg<-data.frame()

exponentes<-3

columna<-dim(datos)[2]
for (i in seq(2,columna,1)){
  
  lineal<-lm(datos[,i] ~datos[,1], datos)
  exponencial<-lm(log(datos[,i])~datos[,1],datos)
  polinomial<-lm(datos[,i] ~ poly(datos[,1],exponentes,raw = TRUE),datos)
  
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

#funciones<-function(j){
Soluciones<-data.frame()
for(j in 2:3){
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
  Soluciones<-cbind(Soluciones,res)
#  return(res)
  
}


#############################
Soluciones$viscosidad<-Vi

png("p_inicial.png")
plot(Soluciones[,1], Soluciones[,2], xlab="diametro (minimizar)", ylab="voltaje (minimizar)", main="Relación de parámetros")
graphics.off()
sign<-c(-1, -1)

mejordiam <- which.max(sign[1] * Soluciones[,1])#elige el maximom sign por evaluacion
mejorvolt <- which.max(sign[2] * Soluciones[,2])


png("p11_init.png")
plot(Soluciones[,1], Soluciones[,2], xlab="diámetro (min)", ylab="voltaje (min)", main="Relación de parámetros")
graphics.off()
png("p_mejores.png")
plot(Soluciones[,1], Soluciones[,2], xlab="distancia (mejor con cuadro azul)",
     ylab="voltaje (mejor con bolita naranja)",
     main="Relación de parámetros")
points(Soluciones[mejordiam, 1], Soluciones[mejordiam, 2], col="blue", pch=15, cex=1.5)
points(Soluciones[mejorvolt, 1], Soluciones[mejorvolt, 2], col="orange", pch=16, cex=1.5)
graphics.off()
no.dom <- logical()
dom <- integer()


###################
for (i in 1:n) {
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * Soluciones[i,], sign * Soluciones[j,], 2))
  }
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, cuantos == 0) # nadie le domina
}

#################
frente <- subset(Soluciones, no.dom) # solamente las no dominadas


timef<-Sys.time()
tiempo<-(timef-timei)
print(tiempo)