library (gtools)
library (rgdal)
library (rJava)
library (zoon)
library (future)
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

run_wf <- function () {
  work <- workflow (occurrence = LocalOccurrenceData (filename="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/dbroteri.csv"),
                    covariate  = LocalRaster (vars.stack),
                    process    = Crossvalidate,
                    model      = MaxEnt,
                    output     = Chain (PrintMap, PredictNewRasterMap, ROCcurve, PerformanceMeasures))
}

plan(multiprocess)
system.time(run_wf())

#==================POPULATIONS=====================#