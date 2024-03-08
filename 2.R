library(sp)
library(gstat)


data(meuse)
class(meuse)

coordinates(meuse) = ~x+y


#spplot(meuse, "zinc",  colorkey = TRUE, main = "zinc concentrations (ppm)")
data(meuse.grid)
#summary(meuse.grid)
#str(meuse.grid)
class(meuse.grid)

coordinates(meuse.grid) = ~x+y
class(meuse.grid)
gridded(meuse.grid) = TRUE
class(meuse.grid)

image(meuse.grid["dist"])
title("distance to river (red = 0)")

library(gstat)
zinc.idw = idw(zinc~1, meuse, meuse.grid)


spplot(zinc.idw["var1.pred"], main = "zinc inverse distance weighted interpolations")

vgm1 = variogram(log(zinc)~1, meuse)
plot(variogram(log(zinc)~1, meuse, cloud=TRUE))

plot(vgm1)

vgm1.fit = fit.variogram(vgm1, model = vgm(1, "Sph", 900, 1))
vgm1.fit


plot(vgm1, vgm1.fit)

vgm1.kriged = krige(log(zinc)~1, meuse, meuse.grid, model = vgm1.fit)
View(log(zinc)~1)

#view(vgm1.kriged)
vgm5<- variogram(log(zinc)~1, meuse)
vgm5.fit <- fit.variogram(vgm5, model=vgm(psill=1, model="Sph", range=900, nugget=1))

plot(vgm5, vgm5.fit)

lzn.okriging <- krige(log(zinc)~1, meuse, meuse.grid, model = vgm5.fit )

spplot(lzn.okriging["var1.pred"], main = "ordinary kriging predictions")






