library(gstat)
library(sp)
data("meuse")
library(ggplot2)
library(viridis)
View(meuse)

ggplot(meuse,aes(y=log(copper),x=sqrt(dist)))+geom_point()

ggplot(meuse,aes(y=y,x=x,col=copper))+geom_point()
scale_color_viridis()