> prueba = matrix(c(0, 1, 0, 0), nrows=2)
Error in matrix(c(0, 1, 0, 0), nrows = 2) : unused argument (nrows = 2)
> 
  > prueba = matrix(c(0, 1, 0, 0), nrow=2)
> prueba
[,1] [,2]
[1,]    0    0
[2,]    1    0
> image(prueba)
Error in plot.new() : figure margins too large
> par(mar=c(0, 0, 0, 0))
> image(prueba)
> prueba = matrix(c(0, 1, 0, 0, 0, 1, 1, 1), nrow=2)
> prueba
[,1] [,2] [,3] [,4]
[1,]    0    0    0    1
[2,]    1    0    1    1
> image(prueba)
> rotate <- function(x) t(apply(x, 2, rev))
> image(rotate(prueba))
> image(prueba)
