###Matrix Scatterplots

#load dataset
data(iris)

# Plot #1: Basic scatterplot matrix of the four measurements
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris)

# panel.smooth function is built in.
# panel.cor puts correlation in upper panels, size proportional to correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Plot #2: same as above, but add loess smoother in lower and correlation in upper
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Iris Scatterplot Matrix")


library(ggplot2)
pairs(iris[,1:4], col=iris$Species)

plotmatrix(iris[,1:4], colour="gray20") +
  geom_smooth(method="lm")


# Scatterplot Matrices from the glus Package 

library(gclus)
dta <- mtcars[c(1,3,5,6)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 


# Scatterplot Matrices from the lattice Package 
library(lattice)
splom(mtcars[c(1,3,5,6)], groups=cyl, data=mtcars,
      panel=panel.superpose, 
      key=list(title="Three Cylinder Options",
               columns=3,
               points=list(pch=super.sym$pch[1:3],
                           col=super.sym$col[1:3]),
               text=list(c("4 Cylinder","6 Cylinder","8 Cylinder"))))