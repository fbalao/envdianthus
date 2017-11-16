library (zoon)
library (future)
library (ulimit)
# library (spatialEco)

#==================POPULATIONS=====================#

# dbrot <- LocalOccurrenceData (filename="dbroteri.csv")
# 
# e <- extent (-10,4.5,35.5,44)
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
# writeRaster(vars.stack,"stack_zoon_pops.grd", format="raster")

# Chain(PrintMap, PerformanceMeasures),

LoadModule('PerformanceMeasures')

vars.stack <- stack("stack_zoon_pops.grd")
plan (multicore)

work <- function(){ workflow (occurrence = LocalOccurrenceData (filename="dbroteri.csv",
                                                                occurrenceType='presence/absence',
                                                                columns=c(long = 'longitude', lat = 'latitude', value = 'value', type = 'type')),
                              covariate  = LocalRaster(vars.stack),
                              process = Chain(StandardiseCov,
                                              Crossvalidate),
                              model = MaxNet,
                              output = PrintMap (dir = "/home/javlopez", size = c (600,600)),
                              forceReproducible = TRUE)}

workres<-work()
save(workres, file = 'workflow_javi.RData')

#==================POPULATIONS=====================#


#==================GBIF=====================#

LoadModule('PerformanceMeasures')
LoadModule('ROCcurve')

vars.stack.gbif <- stack("stack_zoon_gbif.grd")
plan (multicore)
ulimit::memory_limit(100000)

workgbif <- function(){ workflow (occurrence = LocalOccurrenceData 
                               (filename = "dbroterigbif.csv",
                                occurrenceType='presence',
                                columns=c(long = 'longitude', lat = 'latitude', 
                                          value = 'value', type = 'type', fold = 'fold')),
                              covariate  = LocalRaster(vars.stack.gbif),
                              process = Chain (Background (n = 10000, bias = 100), 
                                               StandardiseCov),
                              model = MaxNet,
                              output = Chain (PrintMap (dir = "/home/javlopez", 
                                                 size = c (600,600)),
                                              PerformanceMeasures, ROCcurve (newwin = FALSE)),
                              forceReproducible = TRUE)}

workresgbif <- workgbif()
save (workresgbif, file = 'workflow_gbif.RData')

#==================GBIF=====================#

LoadModule ('ChangeWorkflow')

ChangeWorkflow (workres4, occurrence = NULL,
                covariate = NULL,
                process = NULL,
                model = NULL,
                output = PrintMap (dir = "/home/javlopez", 
                          size = c (600,600), points = T),
                forceReproducible = NULL)
