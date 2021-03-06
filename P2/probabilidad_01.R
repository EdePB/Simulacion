library(parallel)
dim <- 10
num <-  dim^2
for(prb in 1){
  p=prb/10
  actual <- matrix(sample(c(0,1),num,prob=c(p,p), replace=TRUE), nrow=dim, ncol=dim)
  suppressMessages(library("sna"))
  png("p2_t0.png")
  plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  graphics.off()
  print(p)

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for (iteracion in 1:9) {
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  if (sum(siguiente) == 0) { # todos murieron
    print("Ya no queda nadie vivo.")
    break;
  print(iteracion-1)
    }
  }
  
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  salida = paste("p2_t", iteracion, ".png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  graphics.off()
}
stopCluster(cluster)