# Paquetes ----------------------------------------------------------------

library(raster)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(terra)

# Uso funcion que robe de "https://babichmorrowc.github.io/post/2019-04-12-sdm-threshold/"
# pero la modifico para primero calcular el valor de thrshold que quiero:
# Esta funcion tiene para calcular tres tipos de thrEsholds: MTP, P5 Y P10.
thresh = function(sdm, occs, type = "mtp"){
  occPredVals <- raster::extract(sdm, occs) # Estraigo valores de idoneidad de presencias
  if(type == "mtp"){
    thresh <- min(na.omit(occPredVals))
  } else if(type == "p5"){
    if(length(occPredVals) < 10){
      p5 <- floor(length(occPredVals) * 0.95)
    } else {
      p5 <- ceiling(length(occPredVals) * 0.95)
    }
    thresh <- rev(sort(occPredVals))[p5]
  } else if(type == "p10"){
    if(length(occPredVals) < 10){
      p10 <- floor(length(occPredVals) * 0.9)
    } else {
      p10 <- ceiling(length(occPredVals) * 0.9)
    }
    thresh <- rev(sort(occPredVals))[p10]
  }
  return(thresh)
}
# Funcion para binarizar los mapas de acuerdo al umbral calculado previamente
sdm_threshold <- function(sdm, Umbral, binary = FALSE){
  sdm_thresh <- sdm
  sdm_thresh[sdm_thresh < Umbral] <- 0
  if(binary){
    sdm_thresh[sdm_thresh >= Umbral] <- 1
  }
  return(sdm_thresh)
}

# Capas geograficas -------------------------------------------------------
# 1.1. Países
Sudam = ne_countries(scale = 10, continent = "south america", returnclass = "sf")
Argentina = ne_countries(scale = 10, country = "argentina", returnclass = "sf")

# 1.2. Provincias
ProvSudam = ne_states(iso_a2 = c("AR", "BO", "PY", "BR", "CL", "UY", "PE", "EC", "VE", "CO", "GY", "SR", "FR"),
                      returnclass = "sf")
ProvArg = ne_states(iso_a2 = c("AR"),
                    returnclass = "sf")

## Ecorregiones (Olson et al., 2001)
# Descargado de https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world 
# Cargar archivo en R
Ecoregion = st_read("C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_biologicas/Ecorregiones_Olson/wwf_terr_ecos.shp")
ChacoEcoregion = Ecoregion %>% filter(ECO_NAME == "Dry Chaco" |ECO_NAME == "Humid Chaco") 

# Cargo mapa de bioregiones de sudamerica de Morrone 2014
Bioregions = st_read("C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_biologicas/BioRegions_Morrone2014/Lowenberg_Neto_2014.shp")
Bioregions_geo = st_transform(Bioregions,crs = 4326)
Bioregion_geo = Bioregions %>%  
  filter(Province_1=="Chacoan province" | Province_1=="Monte province" | Province_1=="Pampean province" ) %>%
  st_transform(crs = 4326)

Chaco_Bioregion = Bioregions %>%  
  filter(Province_1=="Chacoan province") %>%
  st_transform(crs = 4326)

ChacoArg_Bioregion = st_intersection(Chaco_Bioregion, Argentina)

# Mapa base
mapa_base = tm_shape(ProvSudam, bbox = Hunting_Pressure) + tm_borders(col = "#252525" , lty = "dotted") + tmap_options(check.and.fix = TRUE)+
  tm_shape(Sudam, bbox = Hunting_Pressure) +  tm_borders(col = "#252525", lwd = 2)

# 1. Amenazas --------------------------------------------------

# 1.1. Indice de caza --------------------------------------------------------
## Opcion 1 -----------------------
# Mapa del numero de mammal sp que sufren alta presion en 2015 de caza de Romero-Muñoz (2020)
# Las descargo desde https://datadryad.org/stash/dataset/doi:10.5061/dryad.wm37pvmjg 
# y coloco en el wd
dir.create("Paper_Hunting")
# Descomprimo archivo
unzip(zipfile = "doi_10.5061_dryad.wm37pvmjg__v2.zip", exdir = "Paper_Hunting")
# Cargo el archivo, OJO que el CRS  NO es WGS84!!!
Hunting_Pressure = raster::raster("Paper_Hunting/Hunting_pressure_2015.tif") 
# Lo convierto a WGS84
wgs84 = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
HP_wgs84 = projectRaster(Hunting_Pressure, crs = wgs84, method = "ngb")
qtm(HP_wgs84)
# Opcion 1: Reclasifico el raster para darle un valor de 1 a todos los lugares
# donde al mas de 10 sp de mamiferos tienen alta presion de caza. Si son menos,
# se considera un aumento proporcional. 1 sp, 0.1 probabilidad de que sea Toly mat
rclHP = matrix(c(2147483645, 2147483648,NA, 
                 0, 1, 0,
                 1, 2, 0.1,
                 2, 3, 0.2,
                 3, 4, 0.3,
                 4, 5, 0.4,
                 5, 6, 0.5,
                 6, 7, 0.6,
                 7, 8, 0.7,
                 8, 9, 0.8,
                 9, 10, 0.9,
                 10, 50, 1 #Como es una de las sps mas cazadas del Chaco, consideramos que de 10 de mamiferos hay muy alta prob que en ellas este el mataco
                 ), ncol = 3, byrow = TRUE)
Caza1 = reclassify(HP_wgs84 ,rclHP, include.lowest = T)
qtm(Caza1)

# Opcion 2: Dividir el raster entre 47, para calcular la probabilidad de que 
# la especie sea Tolypeutes
HP_wgs84_NA <- reclassify(HP_wgs84, cbind(2147483645, 2147483648, NA), right=FALSE)
qtm(HP_wgs84_NA)
Caza2 = HP_wgs84_NA/47 
qtm(Caza2)
# Resampleo para que tenga misma resolucion que otros mapas
Current_Si_raster = raster::raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Futuro/Future_WC2.1_Soil/2_Final_Models/M_0.1_F_lq_Set_1_EC/Tolypeutes_matacus_Actual_median.asc")
Caza1 <- resample(Caza1, Current_Si_raster, method = "ngb" )
crs(Caza1) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
qtm(Current_Si_raster)
writeRaster(Caza1, "Caza1.tif", overwrite = T)
Caza1 = raster("4_Caza1.tif")


# Corto usando la bioregión del Chaco de Argentina
Caza1_crop = crop(Caza1, ChacoArg_Bioregion) 
Caza1_rast = mask(Caza1_crop, ChacoArg_Bioregion)

# Plot
Caza1_Mapa =  tm_shape(Sudam, bbox = ChacoArg_Bioregion) + tm_polygons(col = "lightgrey") + 
  tm_shape(Caza1_rast) + tm_raster(style = "cont",
                                palette =viridisLite::viridis(20),
                                title= "Caza",
                                legend.format =list(text.separator="-"),
                                legend.reverse = T) +
  tm_shape(Sudam) + tm_borders(lwd = 2) +
  tm_shape (ProvSudam) + tm_borders(col = "black") + 
  tm_layout(legend.position = c("right","bottom")) + 
  tmap_options(check.and.fix = TRUE)
Caza1_Mapa
tmap_save(Caza1_Mapa, "Caza1_mapa.pdf")

## Opcion 2 -----------------------

# Calculo el índice de presión de caza para la especie como:
# Caza = Distancia a Punto de acceso de cazadores * Densidad Poblacional Humana

# Datos de distancia a Puntos de acceso de cazadores
# Descargo desde OSM
library(osmdata)
parks = opq(bbox = "leeds uk") %>% 
  add_osm_feature(key = "leisure", value = "park") %>% 
  osmdata_sf()

# Datos de densidad de poblacion humana en raster de 30arcseg
help("download.file")
download.file("https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/0_Mosaicked/ppp_2020_1km_Aggregated.tif",
              destfile = "C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_biologicas/Human_density.tif")
HumanDens = raster("C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_biologicas/Human_density.tif")

# 1.2. Destrucción de habitat ---------------------------------------------
## Opción 1 ---------------------
## Descargo mapa de land cover de Copernicus ("https://zenodo.org/record/3939050/files/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif?download=1")
# Cargo el tif
Land_cover_global = raster::raster("Land_cover/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")
# Lo corto por sudam
Land_cover_chacoCropped = raster::crop(Land_cover_global, ChacoEcoregion)#genero un raster con las mismas dimensiones q la mascara
Land_cover_chaco = raster::mask(Land_cover_chacoCropped, ChacoEcoregion)#convierte lo que esta fuera del shape a NA u otro valor (updatevalue =)
writeRaster(Land_cover_chaco, "Land_cover/LC_Chaco.tif", overwrite = T)
Land_cover_chaco = raster("Land_cover/LC_Chaco.tif")

# Pensar en valores de asignacion a cada cobertura de suelo, que afecten a la especie
# Por ejemplo, Forests = 0, Shrubs, Cropland, Herbaceous vegetation, Herbaceous wetland,
# urban, snow and ice.
rcl = matrix(c(0, NA, #No input data available
               111, 0, #Closed forest, evergreen needle leaf
               112, 0, #Closed forest, deciduous needle leaf
               113, 0, #Closed forest, evergreen, broad leaf
               114, 0, #Closed forest, deciduous broad leaf
               115, 0, #Closed forest, mixed
               116, 0, #Closed forest, unknown
               121, 0, #Open forest, evergreen needle leaf
               122, 0, #Open forest, deciduous needle leaf
               123, 0, #Open forest, evergreen broad leaf
               124, 0, #Open forest, deciduous broad leaf
               125, 0, #Open forest, mixed Open
               126, 0, #Open forest, unknown
               20,  0, #Shrubs
               30, 0, #Herbaceous vegetation
               90, 0, #Herbaceous wetland
               100, 0, #Moss and lichen
               60, 0, #Bare / Sparse vegetation
               40, 0.75, #Cropland
               50, 1, #Built-up or urban
               70, 0, #Snow & ice
               80, 0, #Permanent water bodies
               200, NA ), #Open sea
              ncol = 2, byrow = TRUE)

LC_Chaco_Reclas = reclassify(Land_cover_chaco, rcl)
# Cambiar resolucion para que matchee la de los modelos.
Current_Si_raster = raster::raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Futuro/Future_WC2.1_Soil/2_Final_Models/M_0.1_F_lq_Set_1_EC/Tolypeutes_matacus_Actual_median.asc")
PerdidaHab <- resample(LC_Chaco_Reclas, Current_Si_raster, method="ngb")# Ver si uso metodo bilinear o sin metodo
crs(PerdidaHab) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
writeRaster(PerdidaHab, "PerdidaHab_Chaco.tif", overwrite = T)
PerdidaHab = raster("4_PerdidaHab_Chaco.tif")
plot(PerdidaHab)


## Opción 2 ------------------------------------------------------------------
# Uso mapa de desmontes obtenido de http://monitoreodesmonte.com.ar/
# Colección 8.0
# Enlace de descarga: "http://monitoreodesmonte.com.ar/descargar.php?id=19"
DesmonteChacoArg = st_read("C:/Users/ale_f/OneDrive/Documentos/Capas_GIS/Capas_biologicas/Desmontes_Chaco/Coleccion_8.0_Argentina-1976-2019.shp")
DesmonteChacoArge_geo = st_transform(DesmonteChacoArg, crs = 4326)

help("rasterize")
DesmonteChacoArg_rast = fasterize::fasterize(DesmonteChacoArge_geo, 
                                  PerdidaHab, 
                                  field = NULL,
                                  background = 0)
plot(DesmonteChacoArg_rast)
# Sumo raster de desmontes + raster de perdida de hab obtenido de copernicus
PH_para_reclasificar = PerdidaHab + DesmonteChacoArg_rast
m = c(-1, 0, 0, 0.1, 2.5, 1) 
rcl = matrix(m, ncol = 3, byrow = T)
PH = reclassify(PH_para_reclasificar, rcl)

# Corto usando la bioregión del Chaco de Argentina
PH_crop = crop(PH, ChacoArg_Bioregion) 
PH_rast = mask(PH_crop, ChacoArg_Bioregion)

# Plot
PH_Mapa =  tm_shape(Sudam, bbox = ChacoArg_Bioregion) + tm_polygons(col = "lightgrey") + 
  tm_shape(PH_rast) + tm_raster(style = "fixed",
                                breaks = c(-0,1,2),
                                labels = c("Sin intervención antrópica","Sitios modificados"),
                                palette = viridisLite::viridis(20),
                                title= "Deforestación",
                                legend.format =list(text.separator="-"),
                                legend.reverse = T) +
  tm_shape(Sudam) + tm_borders(lwd = 2) +
  tm_shape (ProvSudam) + tm_borders(col = "black") + 
  tm_layout(legend.position = c("right","bottom")) + 
  tmap_options(check.and.fix = TRUE)
PH_Mapa
tmap_save(PH_Mapa, "4_PH_mapa.pdf")

# 1.3. Cambio climático -------------------------------------------------
## CambioClim = 1 - Prob en el pixel i promediada entre las diferentes escenarios y GCMs
## Indice de proteccion futura = Proteccion en el pixel en N GCMs
# Cargo mapas de idoneidad futurA
Suitab_future_paths = list.files(path = "Future_WC2.1_Soil/2_Final_Model_Stats/Statistics_EC",
                                 pattern = "*med.tif$",full.names = TRUE)
Suitab_future_stack <- raster::stack(Suitab_future_paths)
Suitab_future_stack = dropLayer(Suitab_future_stack, 1)
Suitab_future_mean = overlay(Suitab_future_stack, fun = "mean")
CambioClim = 1 - Suitab_future_mean
crs(CambioClim) <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
writeRaster(CambioClim, "CambioClim.tif", overwrite = T)
CambioClim = raster("CambioClim.tif")
CambioClim_chacoCropped = raster::crop(CambioClim, ChacoEcoregion)#genero un raster con las mismas dimensiones q la mascara
CambioClim_chaco = raster::mask(CambioClim_chacoCropped, ChacoEcoregion)#convierte lo que esta fuera del shape a NA u otro valor (updatevalue =)

# Corto usando la bioregión del Chaco de Argentina
DF_crop = crop(CambioClim_chaco, ChacoArg_Bioregion) 
DF_rast = mask(DF_crop, ChacoArg_Bioregion)

# Plot
DF_Mapa =  tm_shape(Sudam, bbox = ChacoArg_Bioregion) + tm_polygons(col = "lightgrey") + 
  tm_shape(DF_rast) + tm_raster(style = "cont",
                                palette =viridisLite::viridis(20),
                                title= "Desfavorabilidad",
                                legend.format =list(text.separator="-"),
                                legend.reverse = T) +
  tm_shape(Sudam) + tm_borders(lwd = 2) +
  tm_shape (ProvSudam) + tm_borders(col = "black") + 
  tm_layout(legend.position = c("right","bottom")) + 
  tmap_options(check.and.fix = TRUE)
DF_Mapa
tmap_save(DF_Mapa, "4_DF_mapa.pdf")


# 1.4. Indice de amenaza --------------------------------------------------
IA_rast = (DF_rast + PH_rast + Caza1_rast )/3  
# Plot
IA_Mapa =  tm_shape(Sudam, bbox = ChacoArg_Bioregion) + tm_polygons(col = "lightgrey") + 
  tm_shape(IA_rast) + tm_raster(style = "cont",
                                palette =viridisLite::viridis(20),
                                title= "Amenaza",
                                legend.format =list(text.separator="-"),
                                legend.reverse = T) +
  tm_shape(Sudam) + tm_borders(lwd = 2) +
  tm_shape (ProvSudam) + tm_borders(col = "black") + 
  tm_layout(legend.position = c("right","bottom")) + 
  tmap_options(check.and.fix = TRUE)
IA_Mapa
tmap_save(IA_Mapa, "4_IA_mapa.pdf")

# Mapa final
Mapafinal = tmap_arrange(Caza_Map, PerdidaHab_Map, CambioClim_Map, Amenaza_Map, 
                         ncol = ,
                         nrow = 1)
Mapafinal



# 2. Indice de conservación ----------------------------------------------

# 2.1. Importancia Genética --------------------------------------------- 
# Tiene en cuenta la importancia genética y la presencia de haplotipos endémicos 

# Cargo tabla con IG previamente calculados
library(gsheet)
IG_Data = read.csv("Importancia_Genetica.csv", sep = ";")

# Convertir el IG_data a sf y darle un correcto crs
IG_Data_sf = IG_Data %>% st_as_sf(coords = c("Longitud","Latitud")) 
IG_Data_sp = IG_Data_sf %>% as_Spatial()

# Cargar raster de idoneidad actual para generar interpolación
SDM_Actual = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Futuro/Future_WC2.1_Soil/2_Final_Model_Stats/Statistics_EC/Actual_med.tif")
SDM_occs = read.csv("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Futuro/Future_WC2.1_Soil/Sp_joint.csv")
SDM_occ_LL = SDM_occs[,2:3] 

# Calculo el umbral P5
SDM_Actual_umbral5 = thresh(SDM_Actual, SDM_occ_LL, type = "p5") 
# Raster umbralizado mediante P5, NO BINARIO
SDM_Actual_P5 = sdm_threshold(SDM_Actual, SDM_Actual_umbral5, binary = F)
SDM_Actual_binP5 = sdm_threshold(SDM_Actual, SDM_Actual_umbral5, binary = T)

SDM_Actual_binP5_Spatrast = as(SDM_Actual_binP5, "SpatRaster")
SDM_Actual_binP5_vector = as.countour(SDM_Actual_binP5_Spatrast)
plot(SDM_Actual_binP5_vector)

# Interpolar con la distancia Inversa Ponderada (IDW)
# para obtener un raster con la distribución de la IG.

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

gs <- gstat(formula=IG~1, locations= IG_Data_sp, set=list(idp = 2))
IG_idw_rast = interpolate(SDM_Actual,gs)

# Corto usando la bioregión del Chaco de Argentina
IG_idw_rast_crop = crop(IG_idw_rast, ChacoArg_Bioregion) 
IG_idw = mask(IG_idw_rast_crop, ChacoArg_Bioregion)
plot(IG_idw)
# Plot
IG_Mapa =  tm_shape(Sudam, bbox = ChacoArg_Bioregion) + tm_polygons(col = "lightgrey") + 
  tm_shape(IG_idw) + tm_raster(style = "cont",
                                    palette =viridisLite::viridis(20),
                                    title= "Importancia genética",
                                    legend.format =list(text.separator="-"),
                                    legend.reverse = T) +
  tm_shape(Sudam) + tm_borders(lwd = 2) +
  tm_shape (ProvArg) + tm_borders() + 
  tm_layout(legend.position = c("right","bottom")) + 
  tmap_options(check.and.fix = TRUE)
IG_Mapa
tmap_save(IG_Mapa, "IG_mapa.pdf")

# 2.2. Refugio ------------------------------------------------------------

# Opción 1:
# Proporción de 12 modelos donde la especie presenta idoneidad ambiental.
NichAct_CurrDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/1_Current_Final_Model_Stats/Statistics_EC/Actual_med.tif")
NichAct_occs = read.csv("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/1_Current_joint.csv")
NichAct_occs_LL = NichAct_occs[,-1]
NichAct_Umbral_p5 = thresh(NichAct_CurrDist,NichAct_occs_LL, type = "p5")
NichAct_CurrDist_binP5 = sdm_threshold(NichAct_CurrDist, NichAct_Umbral_p5, binary = T)
PLOTNichAct_HTMDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/1_Current_Final_Model_Stats/Statistics_EC/HTM_med.tif")
NichAct_HTMDist_binP5 = sdm_threshold(NichAct_HTMDist, NichAct_Umbral_p5, binary = T)
NichAct_LGMDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/1_Current_Final_Model_Stats/Statistics_EC/LGM_med.tif")
NichAct_LGMDist_binP5 = sdm_threshold(NichAct_LGMDist, NichAct_Umbral_p5, binary = T)
NichAct_LIGDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/1_Current_Final_Model_Stats/Statistics_EC/LIG_med.tif")
NichAct_LIGDist_binP5 = sdm_threshold(NichAct_LIGDist, NichAct_Umbral_p5, binary = T)

NichGla_CurrDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/2_Glacial_Final_Model_Stats/Statistics_EC/Actual_med.tif")
NichGla_occs = read.csv("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/2_Glacial_joint.csv")
NichGla_occs_LL = NichGla_occs[,-1]
NichGla_Umbral_p5 = thresh(NichGla_CurrDist,NichGla_occs_LL, type = "p5")
NichGla_CurrDist_binP5 = sdm_threshold(NichGla_CurrDist, NichGla_Umbral_p5, binary = T)
NichGla_HTMDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/2_Glacial_Final_Model_Stats/Statistics_EC/HTM_med.tif")
NichGla_HTMDist_binP5 = sdm_threshold(NichGla_HTMDist, NichGla_Umbral_p5, binary = T)
NichGla_LGMDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/2_Glacial_Final_Model_Stats/Statistics_EC/LGM_med.tif")
NichGla_LGMDist_binP5 = sdm_threshold(NichGla_LGMDist, NichGla_Umbral_p5, binary = T)
NichGla_LIGDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/2_Glacial_Final_Model_Stats/Statistics_EC/LGM_med.tif")
NichGla_LIGDist_binP5 = sdm_threshold(NichGla_LIGDist, NichGla_Umbral_p5, binary = T)

NichMT_CurrDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/3_MultitempSWD_Final_Model_Stats/Statistics_EC/Actual_med.tif")
NichMT_occs = read.csv("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/3_MultitempSWD_joint.csv")
NichMT_occs_LL = NichMT_occs[,2:3]
NichMT_Umbral_p5 = thresh(NichMT_CurrDist,NichMT_occs_LL, type = "p5")
NichMT_CurrDist_binP5 = sdm_threshold(NichMT_CurrDist, NichMT_Umbral_p5, binary = T)
NichMT_HTMDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/3_MultitempSWD_Final_Model_Stats/Statistics_EC/Actual_med.tif")
NichMT_HTMDist_binP5 = sdm_threshold(NichMT_HTMDist, NichMT_Umbral_p5, binary = T)
NichMT_LGMDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/3_MultitempSWD_Final_Model_Stats/Statistics_EC/Actual_med.tif")
NichMT_LGMDist_binP5 = sdm_threshold(NichMT_LGMDist, NichMT_Umbral_p5, binary = T)
NichMT_LIGDist = raster("C:/Users/ale_f/OneDrive/Documentos/Doctorado/ENM/TolyMat_Past/kuenm/3_MultitempSWD_Final_Model_Stats/Statistics_EC/LIG_med.tif")
NichMT_LIGDist_binP5 = sdm_threshold(NichMT_LIGDist, NichMT_Umbral_p5, binary = T)

Refugio1 = ( NichAct_CurrDist_binP5 + NichAct_HTMDist_binP5 + NichAct_LGMDist_binP5 + NichAct_LIGDist_binP5 + 
               NichGla_CurrDist_binP5 + NichGla_HTMDist_binP5 + NichGla_LGMDist_binP5 + NichGla_LIGDist_binP5 + 
               NichMT_CurrDist_binP5 + NichMT_HTMDist_binP5 + NichMT_LGMDist_binP5 + NichMT_LIGDist_binP5 )/12


# Opción 2:
# Proporción de 12 modelos donde la especie presenta idoneidad ambiental, 
# ponderando primero por tipos de nichos

NichAct_Refugio = (NichAct_CurrDist_binP5 + NichAct_HTMDist_binP5 + NichAct_LGMDist_binP5 + NichAct_LIGDist_binP5)/4
NichGla_Refugio = (NichGla_CurrDist_binP5 + NichGla_HTMDist_binP5 + NichGla_LGMDist_binP5 + NichGla_LIGDist_binP5)/4
NichMT_Refugio = (NichMT_CurrDist_binP5 + NichMT_HTMDist_binP5 + NichMT_LGMDist_binP5 + NichMT_LIGDist_binP5)/4

Refugio2 = (NichAct_Refugio + NichGla_Refugio + NichMT_Refugio)/3

# Corto usando la bioregión del Chaco de Argentina
Rf_crop = crop(Refugio1, ChacoArg_Bioregion) 
Rf_rast = mask(Rf_crop, ChacoArg_Bioregion)

# Plot
Rf_Mapa =  tm_shape(Sudam, bbox = ChacoArg_Bioregion) + tm_polygons(col = "lightgrey") + 
  tm_shape(Rf_rast) + tm_raster(style = "cont",
                               palette =viridisLite::viridis(20),
                               title= "Refugio",
                               legend.format =list(text.separator="-"),
                               legend.reverse = T) +
  tm_shape(Sudam) + tm_borders(lwd = 2) +
  tm_shape (ProvSudam) + tm_borders() + 
  tm_layout(legend.position = c("right","bottom")) + 
  tmap_options(check.and.fix = TRUE)
Rf_Mapa
tmap_save(Rf_Mapa, "Rf_mapa.pdf")

# 2.3. Indice de conservación -------------------------------------------------------------------------

IC_rast = (IG_idw + Rf_rast) / 2
# Plot
IC_Mapa =  tm_shape(Sudam, bbox = ChacoArg_Bioregion) + tm_polygons(col = "lightgrey") + 
  tm_shape(IC_rast) + tm_raster(style = "cont",
                                palette =viridisLite::viridis(20),
                                title="Índice de Conservación ",
                                legend.format =list(text.separator="-"),
                                legend.reverse = T) +
  tm_shape(Sudam) + tm_borders(lwd = 2) +
  tm_shape (ProvSudam) + tm_borders() + 
  tm_layout(legend.position = c("right","bottom")) + 
  tmap_options(check.and.fix = TRUE)
IC_Mapa

tmap_save(IC_Mapa, "IC_mapa.pdf")

# junto los tres mapas del indice de conseervación en un solo grafico
IC_maps = tmap_arrange(IG_Mapa, Rf_Mapa, IC_Mapa, nrow = 1, asp = NULL)


# 3. Indice de priorización ---------------------------------------------------
# promedio entre el indice de conservacion y el de priorizacion

IP_rast = (IC_rast + IA_rast)/2
writeRaster(IP_rast, "Priorizacion.tif", overwrite = T)
# Mapa
IP_Mapa =  tm_shape(Sudam, bbox = ChacoArg_Bioregion) + tm_polygons(col = "lightgrey") + 
  tm_shape(IP_rast) + tm_raster(style = "cont",
                                palette =viridisLite::viridis(20),
                                title="Índice de Priorización",
                                legend.format =list(text.separator="-"),
                                legend.reverse = T) +
  tm_shape(Sudam) + tm_borders(lwd = 2) +
  tm_shape (ProvSudam) + tm_borders(col = "black") + 
  tm_layout(legend.position = c("right","bottom")) + 
  tmap_options(check.and.fix = TRUE)
IP_Mapa

tmap_save(IP_Mapa, "4_IP_mapa.pdf")




