caminata<-function(dim,dur){
pos <-rep(0,dim)
posOrigen<- rep(0, dim)
orig <- 0
datos<- data.frame()

for (t in 1:dur) { 
cambiar<-sample(1:dim,1)
cambio<-1  
if (runif(1) < 0.5) {
cambio<- -1
   }
        pos[cambiar] <- pos[cambiar] + cambio
       
    if (all (pos==posOrigen)){
	 orig <- orig + 1
}
 }     
return(orig)
}
for(dim in 1:10){
png("p1er.png")
bloxpot(data.matrix(datos), xlab="dimension", ylab="posicionOrigen", mean="analisis")
}
graphics.off()