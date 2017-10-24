timei<-Sys.time()
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

vc <- 4
md <- 3
tc <- 5
#k <- 2 # cuantas funciones objetivo
obj <- list()

#######################
for (i in 1:k){
  
  obj[[i]] <- poli(md,vc, tc)
  }

#######################
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
#n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
#############################
for (i in 1:n) { # evaluamos las soluciones
  for (j in 1:k) { # para todos los objetivos
    val[i, j] <- eval(obj[[j]], sol[i,], tc)
  }
}
mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
no.dom <- logical()
dominadores <- integer()
###################
for (i in 1:n) {
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, cuantos == 0) # nadie le domina
}

#################
frente <- subset(val, no.dom) # solamente las no dominadas

library(ggplot2) # recordar instalar si hace falta
data <- data.frame(pos=rep(0, n), dom=dominadores)
#png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
  xlab("") +
  ylab("Frecuencia") +
  ggtitle("Cantidad de soluciones dominantes")
#graphics.off()
timef<-Sys.time()
tiempo<-(timef-timei)
print(tiempo)