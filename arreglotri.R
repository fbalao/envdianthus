nuevopunto<-data.frame(decimalLongitude=38.34267778, decimalLatitude=-8.69638889)
coordinates(nuevopunto)<- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(nuevopunto) <- crs.geo 

<<<<<<< HEAD
prueba<-extract(variables$tri,nuevopunto)
=======
envbrot<-extract(bio,nuevopunto)


install.packages("spatialEco")
library(spatialEco)
library(raster)
data(elev)
( tri.ext <- tri(elev) )
( tri.app <- tri(elev, exact = FALSE) )
plot(tri.ext)
>>>>>>> 928c48b970c054fc4d4a76d8332f8baca25d3e69
