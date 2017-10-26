library (dismo)

dbroteri <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/Poblaciones_Nicho.csv", sep = ";", fileEncoding = "latin1", colClasses = c("factor", "factor", "numeric", "numeric"))
coordinates (dbroteri) <- ~long + lat
crs.geo <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string (dbroteri) <- crs.geo

dbroteri_back <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/background_pops.csv", sep = ";")
coordinates (dbroteri_back) <- ~long + lat
proj4string (dbroteri_back) <- crs.geo

vars.stack <- stack("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/stack_zoon.grd")

maxent (x=vars.stack,p=dbroteri,a=dbroteri_back,path="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/maxent")
