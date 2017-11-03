library (gtools)
library (rgdal)
library (rJava)
library (zoon)
# library (spatialEco)

#==================POPULATIONS=====================#

# dbrot <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/dbroteri.csv", sep = ";")
# 
# e <- extent (-10,3,35,44)
# chefiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_pops/chelsa", pattern = ".tif", full.names = TRUE))
# chelsa <- stack (chefiles)
# che.c <- crop (chelsa,e)
# 
# envfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_pops/envirem", pattern = ".bil", full.names = TRUE))
# envirem <- stack (envfiles)
# env.c <- crop (envirem,e)
# 
# soilfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_pops/soil", pattern = ".tif", full.names = TRUE))
# soilgrids <- stack (soilfiles)
# soil.c <- crop (soilgrids,e)
# 
# alt15files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/altitud_15", pattern = ".bil", full.names = TRUE))
# alt16files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/altitud_16", pattern = ".bil", full.names = TRUE))
# alt15 <- stack (alt15files)
# alt16 <- stack (alt16files)
# alt.m <- merge (alt15, alt16, ext=e)
# tri.ext <- tri(alt.m)
# 
# projection (che.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# projection (env.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# projection (soil.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# projection (tri.ext) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# combras <- CombineRasters(c(che.c, env.c, soil.c, tri.ext))
# vars.stack <- stack (combras [[1]], combras [[2]], combras [[3]], combras[[4]])

LoadModule('PredictNewRasterMap')
LoadModule('ROCcurve')
LoadModule('PerformanceMeasures')

vars.stack <- stack("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/stack_zoon.grd")


work <- workflow (occurrence = LocalOccurrenceData (filename="dbroteri.csv"),
                  covariate  = LocalRaster ("stack_zoon"),
                  process    = Chain (Background (n = 1000, bias = 50), Crossvalidate),
work <- workflow (occurrence = LocalOccurrenceData (filename="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/dbroteri.csv"),
                  covariate  = LocalRaster (vars.stack),
                  process    = Crossvalidate,
                  model      = MaxEnt,
                  output     = Chain (PrintMap, PredictNewRasterMap, ROCcurve, PerformanceMeasures))

#==================POPULATIONS=====================#


#==================GBIF=====================#

chefiles2 <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_gbif/chelsa", pattern = ".tif", full.names = TRUE))
chelsa2 <- stack (chefiles2)
che.c2 <- crop (chelsa2,e)

envfiles2 <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_gbif/envirem", pattern = ".bil", full.names = TRUE))
envirem2 <- stack (envfiles2)
env.c2 <- crop (envirem2,e)

soilfiles2 <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_gbif/soil", pattern = ".tif", full.names = TRUE))
soilgrids2 <- stack (soilfiles2)
soil.c2 <- crop (soilgrids2,e)

projection (che.c2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
projection (env.c2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
projection (soil.c2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
combras2 <- CombineRasters(c(che.c2, env.c2, soil.c2))
vars.stack2 <- stack (combras2 [[1]], combras2 [[2]], combras2 [[3]])

work2 <- workflow (occurrence = LocalOccurrenceData (filename="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/dbroterigbif.csv"),
                   covariate  = LocalRaster(vars.stack2),
                   process    = Background (n = 1000, bias = 50),
                   model      = MaxEnt,
                   output     = PrintMap)

#==================GBIF=====================#
