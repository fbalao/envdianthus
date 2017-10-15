library(gtools)
library(rgdal)
library(zoon)

#==================POPULATIONS=====================#

e <- extent (-10,3,35,44)
chefiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_pops/chelsa", pattern = ".tif", full.names = TRUE))
chelsa <- stack (chefiles)
che.c <- crop (chelsa,e)

envfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_pops/envirem", pattern = ".bil", full.names = TRUE))
envirem <- stack (envfiles)
env.c <- crop (envirem,e)

soilfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_pops/soil", pattern = ".tif", full.names = TRUE))
soilgrids <- stack (soilfiles)
soil.c <- crop (soilgrids,e)

projection (che.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
projection (env.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
projection (soil.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
combras <- CombineRasters(c(che.c, env.c, soil.c))
vars.stack <- stack (combras [[1]], combras [[2]], combras [[3]])

work <- workflow (occurrence = LocalOccurrenceData (filename="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/dbroteri.csv"),
                  covariate  = LocalRaster(vars.stack),
                  process    = Background (n = 1000, bias = 50),
                  model      = MaxEnt,
                  output     = PrintMap)

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
