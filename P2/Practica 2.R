  for(prb in 1:9){
  p=prb/10
  actual <- matrix(sample(c(0,1),num,prob=c(p,p), replace=TRUE), nrow=dim, ncol=dim)
  suppressMessages(library("sna"))
  png("p2_t0.png")
  plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
  graphics.off()
  print(p)
  }