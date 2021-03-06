primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
suppressMessages(library(doParallel))
desde <- 1000
hasta <-  3000
original <- desde:hasta
invertido <- hasta:desde
replicas <- 100
datos= data.frame()
for(core in 1:3){
  registerDoParallel(makeCluster(detectCores() - core))
  ncl=4-core
  print(ncl)
   
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
    for (r in 1:replicas) {
      ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
      it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
      at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
      }


   stopImplicitCluster()
   datos=rbind(ot,it,at)
   nombre = paste("p3r", ncl, ".png", sep="")
   NNucleo = paste("Nucleos:", ncl)
   png(nombre)
   boxplot(data.matrix(datos),use.cols=FALSE, xlab="Orden", ylab="Tiempo", main=NNucleo)
   graphics.off()
   }
summary(ot)
summary(it)
summary(at)