nuevopunto<-data.frame(decimalLongitude=36.4443, decimalLatitude=-6.0867)
coordinates(nuevopunto)<- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(brot_cleaned) <- crs.geo 

envbrot<-extract(bio,nuevopunto)