library(gtools)
library(rgdal)
library(zoon)
library (spThin)
devtools::install_github("zoonproject/zoon")


dbroteri <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/Poblaciones_Nicho.csv", sep = ";", fileEncoding = "latin1", colClasses = c("factor", "factor", "numeric", "numeric"))
coordinates (dbroteri) <- ~long + lat
crs.geo <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string (dbroteri) <- crs.geo

e <- extent (-10,3,35,44)
chelsafiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/chelsa", pattern = ".tif", full.names = TRUE))
chelsa <- stack (chelsafiles)
che.c <- crop (chelsa,e)

enviremfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/envirem", pattern = ".bil", full.names = TRUE))
envirem <- stack (enviremfiles)
env.c <- crop (envirem, e)

alt15files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/altitud_15", pattern = ".bil", full.names = TRUE))
alt16files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/altitud_16", pattern = ".bil", full.names = TRUE))
alt15 <- stack (alt15files)
alt16 <- stack (alt16files)
alt.m <- merge (alt15, alt16, ext=e)

soilgridsfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", pattern = ".tif", full.names = TRUE))
soilgrids <- stack (soilgridsfiles)
soil.c <- crop (soilgrids, e)

crs (env.c) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs (alt.m) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
variables0 <- CombineRasters (c(che.c, env.c, alt.m))
variables <- CombineRasters (c(variables0, soil.c))
# names (variables) <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19","annualPET","aridityIndexThornthwaite","climaticMoistureIndex","continentality","embergerQ","growingDegDays0","growingDegDays5","maxTempColdest","minTempWarmest","monthCountByTemp10","PETColdestQuarter","PETDriestQuarter","PETseasonality","PETWarmestQuarter","PETWettestQuarter","thermicityIndex","topoWet","tri","elevation")
