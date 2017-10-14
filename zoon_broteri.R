library(gtools)
library(rgdal)
library(zoon)

#==================POPULATIONS=====================#

dbroteri <- read.csv2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/dbroteri.csv")

e <- extent (-10,3,35,44)
varsfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_pops", pattern = c(".bil", ".tif"), full.names = TRUE))
variables <- stack (varsfiles)
vars.c <- crop (variables,e)

soilfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_pops/soil", pattern = ".tif", full.names = TRUE))
soilgrids <- stack (soilfiles)
soil.c <- crop (soilgrids,e)

projection (vars.c) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
combras <- CombineRasters(vars.c, soil.c)
vars.stack <- stack (combras [[1]], combras [[2]])

work <- workflow (occurrence = LocalOccurrenceData (filename="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/dbroteri.csv"),
                  covariate  = LocalRaster(vars.stack),
                  process    = Background (n = 1000, bias = 50),
                  model      = MaxEnt,
                  output     = PrintMap)

#==================POPULATIONS=====================#


#==================GBIF=====================#

varsfiles2 <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_gbif", pattern = c(".bil", ".tif"), full.names = TRUE))
variables2 <- stack (varsfiles2)
vars.c2 <- crop (variables2,e)

soilfiles2 <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/vars_selected_gbif/soil", pattern = ".tif", full.names = TRUE))
soilgrids2 <- stack (soilfiles2)
soil.c2 <- crop (soilgrids2,e)

projection (vars.c2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
combras2 <- CombineRasters(vars.c2, soil.c2)
vars.stack2 <- stack (combras2 [[1]], combras2 [[2]])

work <- workflow (occurrence = LocalOccurrenceData (filename="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/dbroteri.csv"),
                  covariate  = LocalRaster(vars.stack2),
                  process    = Background (n = 1000, bias = 50),
                  model      = MaxEnt,
                  output     = PrintMap)

#==================GBIF=====================#
