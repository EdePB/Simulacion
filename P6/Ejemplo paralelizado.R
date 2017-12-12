rm(list=ls())
gc()


library(data.table)

archivos<-list.files()

# Version NO PARALELA
time1 <- Sys.time()
out <- for(i in 1:10 (source('~/GitHub/SimulacionComputacional/P6/codigobase.R', encoding = 'UTF-8'))){
  fread(source('~/GitHub/SimulacionComputacional/P6/codigobase.R', encoding = 'UTF-8')[i],header=T,dec=",",sep=";")
}
time2 <- Sys.time()
print(paste("Tiempo de lectura =", time2 - time1))

# VERSION PARALELA

library(foreach)
time1 <- Sys.time()
out <- foreach(i = 1:10(source('~/GitHub/SimulacionComputacional/P6/Prueba1.R', encoding = 'UTF-8')),
               .packages = 'data.table') %dopar%
  fread(source('~/GitHub/SimulacionComputacional/P6/Prueba1.R', encoding = 'UTF-8')[i],header=T,dec=",",sep=";")
time2 <- Sys.time()
print(paste("Tiempo de lectura =", time2 - time1))

library( multicore )
n.iter <- 1000
foo <- function(i) mean( rnorm( 100000 ) )  

system.time( resultados <- replicate( n.iter, foo() ) )
# 16.47 segundos en mi sistema (elapsed)

system.time( resultados <-
               mclapply( 1:n.iter, foo, mc.set.seed = TRUE, mc.cores = 6 ) )
# 4.34 segundos