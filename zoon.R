library (zoon)
library (future)
# library (spatialEco)

#==================POPULATIONS=====================#

# dbrot <- LocalOccurrenceData (filename="dbroteri.csv")
# 
# e <- extent (-10,3,35,44)
# chefiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_gbif/chelsa", pattern = ".tif", full.names = TRUE))
# chelsa <- stack (chefiles)
# che.c <- crop (chelsa,e)
# 
# envfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_gbif/envirem", pattern = ".bil", full.names = TRUE))
# envirem <- stack (envfiles)
# env.c <- crop (envirem,e)
# 
# soilfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_gbif/soil", pattern = ".tif", full.names = TRUE))
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
# writeRaster(vars.stack,"stack_zoon_gbif.grd", format="raster")

LoadModule('PerformanceMeasures')

vars.stack <- stack("stack_zoon.grd")


work <- function(){ workflow (occurrence = LocalOccurrenceData (filename="dbroteri.csv"),
                              covariate  = LocalRaster(vars.stack),
                              process = Chain(Clean,
                                              Background(n = 1000, bias = 50),
                                              StandardiseCov,
                                              Crossvalidate),
                              model = MaxNet,
                              output = Chain(PrintMap, PerformanceMeasures),
                              forceReproducible = TRUE)}

workres<-work()
save(workres, file = 'workflow_javi.RData')

#==================POPULATIONS=====================#

#==================GBIF=====================#

LoadModule('PerformanceMeasures')

vars.stack.gbif <- stack("stack_zoon_gbif.grd")


work1 <- function(){ workflow (occurrence = LocalOccurrenceData (filename="dbroterigbif.csv"),
                              covariate  = LocalRaster(vars.stack.gbif),
                              process = Chain(Clean,
                                              Background(n = 1000, bias = 50),
                                              StandardiseCov,
                                              Crossvalidate),
                              model = MaxNet,
                              output = Chain(PrintMap, PerformanceMeasures),
                              forceReproducible = TRUE)}

workres1<-work1()
save(workres1, file = 'workflow_javi_gbif.RData')

#==================GBIF=====================#