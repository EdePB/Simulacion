library(parallel)
dim <- 10
num <-  dim^2
corridas=9
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
  
for(expe in 1:corridas){
  for(prb in 1:9){
     p=prb/10
    actual <- matrix(sample(c(0,1),num,prob=c(1-p,p), replace=TRUE), nrow=dim, ncol=dim)
    suppressMessages(library("sna"))
    png("p2_t0.png")
  
    plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
    graphics.off()
    print(p)
  }
    



      for (iteracion in 1:10){
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster, 1:num, paso)
      print(iteracion-1)
        if (sum(siguiente) == 0) { # todos murieron
        print("Ya no queda nadie vivo.")
        break;
   }

    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    salida = paste("p2_t", iteracion,"probabilidad",p, ".png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  graphics.off()
    
    }

bloxpot(datos, diaglab=FALSE, xlab="probabilidad", ylab="corridas",main="Analisis" )
graphics.off()

  }
}
stopCluster(cluster)