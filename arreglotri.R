nuevopunto<-data.frame(decimalLongitude=38.34267778, decimalLatitude=-8.69638889)
coordinates(nuevopunto)<- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(nuevopunto) <- crs.geo 

prueba<-extract(variables$tri,nuevopunto)
