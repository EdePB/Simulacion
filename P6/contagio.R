contagios <- rep(FALSE, n)
if (a1$estado == "I") { # desde los infectados
  for (j in 1:n) {
    if (!contagios[j]) { # aun sin contagio
      a2 <- agentes[j, ]
      if (a2$estado == "S") { # hacia los susceptibles
        dx <- a1$x - a2$x
        dy <- a1$y - a2$y
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