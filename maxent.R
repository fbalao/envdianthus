library (dismo)
library (ENMeval)

dbroteri <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/Poblaciones_Nicho.csv", sep = ";", fileEncoding = "latin1", colClasses = c("factor", "factor", "numeric", "numeric"))
coordinates (dbroteri) <- ~long + lat
crs.geo <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string (dbroteri) <- crs.geo

dbroteri_back <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/background_pops.csv", sep = ";")
coordinates (dbroteri_back) <- ~long + lat
proj4string (dbroteri_back) <- crs.geo

vars.stack <- stack ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/stack_zoon.grd")

dbroteridata <- as.data.frame(dbroteri)
dbroteridata <- dbroteridata [,c(4,3)]
dbroteri_backdata <- as.data.frame (dbroteri_back)

dbroteri_di <- dbroteridata [c(1:6),]
dbroteri_te <- dbroteridata [c(7:17),]
dbroteri_he <- dbroteridata [c(18:22),]
dbroteri_do <- dbroteridata [c(23:29),]

coordinates (dbroteri_di) <- ~long + lat
proj4string (dbroteri_di) <- crs.geo
coordinates (dbroteri_te) <- ~long + lat
proj4string (dbroteri_te) <- crs.geo
coordinates (dbroteri_he) <- ~long + lat
proj4string (dbroteri_he) <- crs.geo
coordinates (dbroteri_do) <- ~long + lat
proj4string (dbroteri_do) <- crs.geo

mod_maxent <- maxent (x=vars.stack,p=dbroteri,a=dbroteri_back,path="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/maxent", responsecurves = T)
mod_predict <- predict (mod_maxent, vars.stack, c(-10,1,36,40.5))
plot (mod_predict)

mod_maxent_di <- maxent (x=vars.stack,p=dbroteri_di,a=dbroteri_back,path="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/maxent", responsecurves = T)
mod_predict_di <- predict (mod_maxent_di, vars.stack, c(-10,1,36,40.5))
plot (mod_predict_di)

mod_maxent_te <- maxent (x=vars.stack,p=dbroteri_te,a=dbroteri_back,path="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/maxent", responsecurves = T)
mod_predict_te <- predict (mod_maxent_te, vars.stack, c(-10,1,36,40.5))
plot (mod_predict_te)

mod_maxent_he <- maxent (x=vars.stack,p=dbroteri_he,a=dbroteri_back,path="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/maxent", responsecurves = T)
mod_predict_he <- predict (mod_maxent_he, vars.stack, c(-10,1,36,40.5))
plot (mod_predict_he)

mod_maxent_do <- maxent (x=vars.stack,p=dbroteri_do,a=dbroteri_back,path="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/maxent", responsecurves = T)
mod_predict_do <- predict (mod_maxent_do, vars.stack, c(-10,1,36,40.5))
plot (mod_predict_do)

dbroteri_di <- as.data.frame (dbroteri_di)
dbroteri_te <- as.data.frame (dbroteri_te)
dbroteri_he <- as.data.frame (dbroteri_he)
dbroteri_do <- as.data.frame (dbroteri_do)

evaluation <- ENMevaluate (dbroteridata, vars.stack, dbroteri_backdata, rasterPreds = T)
evaluation_di <- ENMevaluate (dbroteri_di, vars.stack, dbroteri_backdata, method = "jackknife", rasterPreds = T)
evaluation_te <- ENMevaluate (dbroteri_te, vars.stack, dbroteri_backdata, method = "jackknife", rasterPreds = T)
evaluation_he <- ENMevaluate (dbroteri_he, vars.stack, dbroteri_backdata, method = "jackknife", rasterPreds = T)
evaluation_do <- ENMevaluate (dbroteri_do, vars.stack, dbroteri_backdata, method = "jackknife", rasterPreds = T)
