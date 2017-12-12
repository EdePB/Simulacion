g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}
png("p7_2d.png", width=700, height=700)
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, g)
persp(x, y, z, shade=0.2, col='orange', theta=40, phi=30)
graphics.off()
png("p7_flat_1.png", width=500, height=500)
image(z)
graphics.off()
dimnames(z) <- list(x, y)
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
d <- melt(z)
names(d) <- c("x", "y", "z")
library(lattice) # lo mismo aplica con este paquete
png("p7_flat_2.png", width=500, height=500)
levelplot(z ~ x * y, data = d)
graphics.off()