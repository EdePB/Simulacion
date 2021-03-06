#timei<-Sys.time()
l <- 1.5
n <- 100#estaba en 50
pi <- 0.05
pr <- 0.02
#pv<-0.03
v <- l / 30
agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
for (i in 1:n) {
  e <- "S"
  if (runif(1) < pv) {
    e <- "R"
  }
  if (runif(1) < pi&& e!="R") {
    e <- "I"
  }
  agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                       dx = runif(1, -v, v), dy = runif(1, -v, v),
                                       estado = e))
  levels(agentes$estado) <- c("S", "I", "R")
}
epidemia <- integer()
r <- 0.1
tmax <- 100 #estaba en 100
digitos <- floor(log(tmax, 10)) + 1

f1<-function(j){
  contagios <- rep(FALSE, n)
  for (i in 1:n) { # posibles contagios
    #a1 <- agentes[i, ]
    if (agentes[i,5] == "I") { # desde los infectados
      #for (j in 1:n) {
      if (!contagios[j]) { # aun sin contagio
        #a2 <- agentes[j, ]
        if (agentes[j,5] == "S") { # hacia los susceptibles
          dx <- agentes[i,1] - agentes[j,1]
          dy <- agentes[i,2] - agentes[j,2]
          d <- sqrt(dx^2 + dy^2)
          if (d < r) { # umbral
            p <- (r - d) / r
            if (runif(1) < p) {
              contagios[j] <- TRUE
            }
          }
        }
      }
    }
  }
  return(contagios[j])
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for (tiempo in 1:tmax) {#se puede paralelizar los for
  infectados <- dim(agentes[agentes$estado == "I",])[1]
  epidemia <- c(epidemia, infectados)
  if (infectados == 0) {
    break
  }
  para1=foreach(j=1:n, .combine = c)%dopar% f1(j) #se paraleliza contagios en este punto
  stopImplicitCluster()
  for (i in 1:n) { # movimientos y actualizaciones
    #f2<-function(i){
    a <- agentes[i, ]
    if (para1[i]) {
      a$estado <- "I"
    } else if (a$estado == "I") { # ya estaba infectado
      if (runif(1) < pr) {
        a$estado <- "R" # recupera
      }
    }
    a$x <- a$x + a$dx
    a$y <- a$y + a$dy
    if (a$x > l) {
      a$x <- a$x - l
    }
    if (a$y > l) {
      a$y <- a$y - l
    }
    if (a$x < 0) {
      a$x <- a$x + l
    }
    if (a$y < 0) {
      a$y <- a$y + l
    }
    agentes[i, ] <- a # para1[i,]<-a
  }  
  #return(agentes[i, ])
  
  #para2=foreach(i=1:n, .combine = c)%dopar% f2(i) #se paraleliza cambios de estado y movimientos
  
 # aS <-  agentes[agentes$estado == "S",]#para2[para2$estsado== "S",]
  #aI <- agentes[agentes$estado == "I",]#para2[para2$estado=="I",]
  #aR <- agentes[agentes$estado == "R",]#para2[para2$estado=="R",]
  #tl <- paste(tiempo, "", sep="")
  #while (nchar(tl) < digitos) {
  #  tl <- paste("0", tl, sep="")
}

#salida <- paste("p6_t", tl, ".png", sep="")
#tiempo <- paste("Paso", tiempo)
#png(salida)
#plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
#if (dim(aS)[1] > 0) {
 #points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
#}
#if (dim(aI)[1] > 0) {
 #points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
#}
#if (dim(aR)[1] > 0) {
 #points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
#}
# graphics.off()
#}
#png("p6v.png", width=600, height=300)
#plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentahe de infectados")
#graphics.off()
#timef<-Sys.time()
#tiempo<-(timef-timei)
#print(tiempo)