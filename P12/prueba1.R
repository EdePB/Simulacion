binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002

r <- 5
c <- 3
dim <- r * c


tranqui <- 0.99
tasa <- 0.15

tope <- 9
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

##########################################

for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}

##########################################
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
#cuenta<-data.frame()
#for (t in 1:300) { # prueba
f2<-function(t){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  #contadores[d+1, r+1]<-contadores[d+1, r+1] + 1
  return(c(d,r))
}
cuenta<-foreach(t=1:300, .combine=rbind) %dopar% f2(t)
colnames(cuenta)<-c("d","r")
rownames(cuenta)<- c(1:300)

#contadores[d+1, r+1] <- contadores[d+1, r+1] + 1

stopImplicitCluster()
print(contadores)