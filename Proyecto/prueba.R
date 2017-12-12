
datos<-read.csv(file="exp.csv", head=TRUE, sep = ",")

reg<-data.frame()

exponentes<-3

columna<-dim(datos)[2]
for (i in seq(2,columna,1)){
  
  lineal<-lm(datos[,i] ~datos[,1], datos)
  exponencial<-lm(log(datos[,i])~datos[,1],datos)
  polinomial<-lm(datos[,i] ~ poly(datos[,1],exponentes),datos)
  
  lin<-cbind(colnames(datos)[i],"Lineal",summary(linear)$r.squared,summary(lineal)$coefficients[2,1],
             0,0,summary(lineal)$coefficients[1,1])
  
  exp<-cbind(colnames(datos)[i],"Exponencial",summary(exponencial)$r.squared,summary(exponencial)$coefficients[2,1],
             0,0,summary(exponencial)$coefficients[1,1])
  
  poli<-cbind(colnames(datos)[i],"Polinomial",summary(polinomial)$r.squared,summary(polinomial)$coefficients[2,1],
              summary(polinomial)$coefficients[3,1],summary(polinomial)$coefficients[4,1],summary(exponencial)$coefficients[1,1])
  
  reg<-rbind(reg,lin,exp,poli)
}
colnames(reg)<-c("Caracteristica","Regresion","Rcuadrada","C.lineal",
                        "C.cuadratico","C.cubico","Interseccion")

a1<-summary(fit1)$coefficients[2,1]
b1<-summary(fit1)$coefficients[1,1]
fd<-function(vis){
  diam=a1*vis+b1
  return(diam)
}


diamejor<-if(regresion$Caracteristica==Diametro){
  max(regresion$Rcuadrada)}
if(regresion$Caracteristica==Distancia){
  cbind(regresion)<- max(regresion$Rcuadrada)
  }
volmejor<-if(regresion$Caracteristica==Diametro){
  max(regresion$Rcuadrada)}

for (j in seq(2,columna,1)){
  J<-colnames(reg)[j]
  
  temporal<-reg[reg$Caracteristicas==J,]
  f<-temporal[which.max(temporal$Rcuadrada), ]
  
  if(f$Regresion=="Lineal"){
    a2<-summary(fit2)$coefficients[2,1]
    b2<-summary(fit2)$coefficients[1,1]
    volt=a2*vis+b2
    
      
  }
  
  


datos<-data.frame()
resultados<-data.frame()

n<-runif(200)
#determinar si maximiza o minimiza
for(i in 1:200){
  vs<-soluc[i]
  datos=cbind(fd(vs),-fv(vs), vs)
  resultados=rbind(resultados,datos)
  
}
png("p11_inicial.png")
plot(resultados[,1], resultados[,2], xlab="diametro (minimizar)", ylab="voltaje (minimizar)", main="Ejemplo bidimensional")
graphics.off()