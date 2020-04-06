library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(writexl)
library(tidyverse)
library(stringi)
library(htmlwidgets)

library(osrm) # Se podria crear un servidor local - Convierte Direcciones en longitud y latitud
library(hereR) # Requiere de una KEY - Calcula las distancias entre dos puntos long-lat

library(leaflet)
library(sf)
library(raster)
library(gstat)
library(caret)

setwd("C:/Users/rbonilla/Google Drive/work/TheProfessional/PersonalResearch/Github/CoberturaHospitalaria/")
source(file = "CoberturaHospitalaria_funciones.R", encoding = "utf8")

# KEY para acceder HereR herer::geocode
set_key("649v2eOmWyQoDCurYqPiKjhMgjarEbS70-drf7uDH3A")

# Cuando se tenga un servidor de OpenStreet
#options(osrm.server = "http://localhost:5000/")
#getOption("osrm.server")

# Datos Hospitales ----
#hospitales <- read_excel(path = "c:/Users/rbonilla/Downloads/NearestHospital/kzp17_daten.xlsx", sheet = "KZ2017_KZP17")
hospitales <- read_csv("C:/Users/rbonilla/Downloads/NearestHospital/Registro_Especial_de_Prestadores_de_Servicios_de_Salud.csv", 
                                                                                locale = locale(asciify = TRUE))

# NombreCiudad ----
NombreCiudad = "BOGOTÁ"

# Filtro por NombreCiudad
hospitales <- hospitales %>% filter(nompio == NombreCiudad)

# Estandarizacion de strings
hospitales <- hospitales %>% mutate_if(is.character, ~toupper(stri_trans_general(str = ., id = "ASCII-Latin")))
# Estandarizar direcciones
test <- data.frame(dir00 = hospitales$direccion)
hospitales$direccion <- EstandarizarDirecciones(direcciones = hospitales$direccion)
test <- test %>% mutate(dir01 = hospitales$direccion)


# se filtran las direcciones que se buscan y se ajusta la ortografia para llamar a osrm
hospitales_Seleccionados <- hospitales %>% filter(nchar(direccion)>5) %>% dplyr::select(nombre, direccion, telefono, email, najunombre, ese, nitsnit)

if(grepl(pattern = "CALI", x = NombreCiudad)){
  hospitales_Seleccionados$direccion <- paste(str_to_title(hospitales_Seleccionados$direccion) , ", Cali Valle, COLOMBIA", sep = "")
}else{
  hospitales_Seleccionados$direccion <- paste(str_to_title(hospitales_Seleccionados$direccion) , ", Bogota, COLOMBIA", sep = "")
}

# Ejemplo de Hospitales
set.seed(33)
sample_n(hospitales_Seleccionados, 3)

# HereR geocode
#geocode("Calle 52 71d 37, Bogota, COLOMBIA")
#geocode(c("TRANSVERSAL 60 115 58, Bogota, COLOMBIA", "TRANSVERSAL 3 49-00, Bogota, COLOMBIA"))

# RUN geocode
# hospitales_Seleccionados_geocoded <- geocode(hospitales_Seleccionados$direccion)

# WRITE
#hospitales_Seleccionados_geocoded %>% write_rds(path = "hospitales_Seleccionados_geocoded_BOGOTA.RDS")
#hospitales_Seleccionados_geocoded %>% write_rds(path = "hospitales_Seleccionados_geocoded_CALI.RDS")

# READ
if(grepl(pattern = "CALI", x = NombreCiudad)){
  hospitales_Seleccionados_geocoded <- read_rds(path = "data/backupRDS/emergency_hospitals_geocoded_CALI.RDS")
}else{
  hospitales_Seleccionados_geocoded <- read_rds(path = "data/backupRDS/emergency_hospitals_geocoded_BOGOTA.RDS")
}

# crear LONG LAT ----
dfCoordinates <- data.frame(st_coordinates(x = hospitales_Seleccionados_geocoded), stringsAsFactors = F)
names(dfCoordinates) <- c("lon", "lat")
hospitales_Seleccionados_geocoded <- hospitales_Seleccionados_geocoded %>% bind_cols(dfCoordinates)

# El archivo original los Nombres pero el encoded la posicion se pega por id ----
hospitales_Seleccionados$id <- as.numeric(1:nrow(hospitales_Seleccionados))

hospitales_Seleccionados_geocoded <- left_join(x = hospitales_Seleccionados_geocoded,
                                          y = dplyr::select(hospitales_Seleccionados, -direccion),
                                          by = "id")

# Filtro "HOSPITAL" ----
hospitales_Seleccionados_geocoded <- hospitales_Seleccionados_geocoded %>% 
  #filter(grepl(pattern = "HOSPITAL|CLINICA|UNIDAD|FUNDACION|CENTRO MEDICO|IPS|EPS", x = nombre)) %>%
  filter(grepl(pattern = "HOSPITAL|CLINICA|UNIDAD|FUNDACION|CENTRO MEDICO", x = nombre)) %>%
  filter(!grepl(pattern = "ODON", x = nombre)) %>%
  filter(!grepl(pattern = "MAESTRO", x = nombre))

# Raster MAP ----
icons <- iconList(
  hospital = makeIcon("location_3440906.png", iconWidth = 18, iconHeight = 18)
)

basicmap <- leaflet() %>% 
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$OpenStreetMap) %>% 
  addMarkers(data = hospitales_Seleccionados_geocoded,
             lng = ~lon, lat = ~lat, popup = ~nombre,
             group = "Unidades de atención", icon = icons["hospital"])

basicmap %>% 
  addLayersControl(overlayGroups = c("Hospitales"))

# First, let’s define the extremes that we want the points to be in, ----
# here the extremes of Switzerland. Those come from Wikipedia.

# READ
if(grepl(pattern = "CALI", x = NombreCiudad)){
  ciudad <- shapefile(x = "data/mc_comunas_Cali/mc_comunas.shp")
  ciudad_t <- spTransform(ciudad, CRSobj = "+init=epsg:4326")
}else{
  ciudad <- shapefile(x = "data/locashp_BOGOTA/Loca.shp/")
  ciudad <- ciudad %>% subset(!(LocNombre %in% c("SUMAPAZ", "USME", "CIUDAD BOLIVAR")))
}


# XYMinMax <- bbox(ciudad_t)
# 
# latmin = XYMinMax[2,1]
# latmax = XYMinMax[2,2]
# lonmin = XYMinMax[1,1]
# lonmax = XYMinMax[1,2]

# Therefore we just loop over smaller sized requests ----
# A total of 10’000 points would be great (100 requests at 100 each).

# Puntos finales
hospitals_df <- hospitales_Seleccionados_geocoded %>% 
  as.data.frame() %>%
  dplyr::select(id = nombre, lon, lat)

# Puntos iniciales
nmax <- 1000
intervals <- round(nmax/50)

# Random Seed
set.seed(455)

# Seleccion de los puntos sobre el rectangulo no sobre la ciudad
# xsp = spTransform(ciudad, CRSobj = "+init=epsg:4326")
# spatialP <- SpatialPoints(coords = t(bbox(xsp)))
# pts = spsample(x = spatialP, type = "regular", n = nmax)

pts = spsample(x = spTransform(ciudad, CRSobj = "+init=epsg:4326"), type = "random", n = nmax)
pts1 = coordinates(pts)

randompts_df <- data.frame(id=1:nrow(pts1),
                           lon = pts1[,1],
                           lat = pts1[,2])


# Se parte los valores en grupos mas pequeñospara llamar al servidor osrm
kFolds <- createFolds(y = randompts_df$id, k = intervals)

mindistances = data.frame()
fCalc <- function(x) min(x, na.rm = T)

for (id in 1:length(kFolds)) {
  
  # request OSRM server
  t0 <- Sys.time()
  
  distancetable <- osrmTable(src = randompts_df[kFolds[[id]],] , dst = hospitals_df)
  
  rrt <- as.numeric(Sys.time() - t0, units = "secs") %>% round(3)
  
  #El minimo calculdo aquí es por fila
  mindistances_i <- bind_cols(distancetable$sources, mintime = apply(distancetable$durations, 1, fCalc)) %>% 
    as_tibble() %>% 
    mutate(mintime_str = as.character(mintime))
  
  mindistances <- bind_rows(mindistances, mindistances_i)
  
  print(str_c("run: ", id, ", request response time: ", rrt, "secs"))
}

# Let’s remove duplicates and save ---- 
mindistances <- bind_rows(mindistances, 
                          hospitals_df %>% 
                            dplyr::select(lon, lat) %>% 
                            mutate(mintime = 0, mintime_str = "0"))

mindistances <- mindistances %>% 
  distinct()

# Read de distancias medida en minutos ----

#mindistances %>% write_rds("data/backupRDS/mindistances_cali.RDS")
#mindistances <- read_rds(path = "data/backupRDS/mindistances_cali.RDS")

mindistances <- mindistances %>% distinct() %>% filter(mintime<30)
mindistances$mintime <- mindistances$mintime* ScaleFunction(mindistances$mintime)
mindistances$mintime_str <- as.character(mindistances$mintime)
mindistances <- mindistances %>% filter(mintime>=1)

#test <- data.frame(x = mindistances$mintime, y = ScaleFunction(mindistances$mintime))

nrow(mindistances)
mindistances %>% ggplot(aes(x = mintime)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, colour="black", fill="white") +
  geom_density(alpha = 0.2, fill = "#FF6666") 

#Let’s see it our map ----

binpal <- colorBin(palette = c("navy", "orange"), domain = 0:max(mindistances["mintime"]), bins = 20)

basicmap %>% 
  addCircleMarkers(data = mindistances, lng = ~lon, lat = ~lat, radius = 1,
                   color = ~binpal(mintime), popup = ~mintime_str,
                   group = "Driving times",
                   opacity = 0.8) %>%
  addLegend(data = mindistances, pal = binpal, values = ~mintime, group = "Driving times",
            title = "Time in min to the closest hospital", position = "bottomright") %>% 
  addLayersControl(overlayGroups = c("Hospitals", "Driving times")) %>% 
  hideGroup("Hospitals")
# addPolygons(data = ciudad_t, group = "Comunas", color = "black", opacity = 1, weight = 1, dashArray = "3",
#             popup = ciudad_t$nombre,
#             fillOpacity = 0.3, fillColor = "red", labelOptions = labelOptions(direction = "none"))


# The gridRes argument defines the raster resolution.
# A higher value will result in a more pixelated raster,
# a lower value will lead to very long computation times.
minraster <- interpolateSurface(data = mindistances, gridRes = 30, idp = 0)

minraster_shaped <- mask(minraster$nn, spTransform(ciudad_t, CRSobj = "+init=epsg:3857"))

binpal <- colorBin(palette = c("navy", "orange"), domain = 0:max(mindistances["mintime"]), bins = 15, na.color = NA)

mapa <- basicmap %>% 
  addRasterImage(minraster_shaped, group = "Tiempo en vehículo", opacity =0.9,
                 colors = binpal) %>% 
  
  addLayersControl(overlayGroups = c("Unidades de atención", "Tiempo en vehículo")) %>%
  
  addLegend(data = mindistances, pal = binpal, values = ~mintime, group = "Driving times",
            title = "Area de influencia por cercanía en tiempo") %>%
  
  hideGroup("Hospitales") %>%
  addPolygons(data = ciudad_t, group = "Comunas", color = "black", opacity = 0.5, weight = 1, dashArray = "3",
              popup = ciudad_t$nombre,
              #fillOpacity = 0.3, fillColor = "red",
              labelOptions = labelOptions(direction = "none"),
              highlight = highlightOptions(
                sendToBack = T,
                weight = 1,
                color = NA,
                dashArray = "",
                fillOpacity = 0.0,
                bringToFront = TRUE))
mapa


saveWidget(mapa, file="CoberturaHospitalaria_CALI.html")










# ------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------ #

swissterrain <- ggmap::get_stamenmap(bbox = c(lonmin-0.1, latmin-0.1, lonmax+0.1, latmax+0.1), 
                                     zoom = 9, 
                                     maptype="terrain-background", color = "bw")

minraster_shaped_aggr <- aggregate(minraster_shaped, fact = 4)

minraster_shaped_rtp <- rasterToPolygons(minraster_shaped_aggr)

minraster_shaped_rtp <- spTransform(minraster_shaped_rtp, 
                                    CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

minraster_shaped_rtp@data$id <- 1:nrow(minraster_shaped_rtp@data)

minraster_shaped_fort <- fortify(minraster_shaped_rtp, 
                                 data = minraster_shaped_rtp@data)

minraster_shaped_fort <- merge(minraster_shaped_fort, 
                               minraster_shaped_rtp@data, 
                               by.x = 'id', 
                               by.y = 'id')

# We also load some cities’ coordinates for labels. ----

library(maps)

swisscities <- world.cities %>% 
  filter(country.etc == "Switzerland", pop > 120000 | name == "Davos")


# And finally plot. ----

bm <- ggmap::ggmap(swissterrain)

bm + 
  geom_polygon(data = switzerland, aes(x=long,y=lat,group=group),
               alpha = 0, color = "black") +
  geom_polygon(data = minraster_shaped_fort, 
               aes(x = long, y = lat, group = group, fill = var1.pred, alpha = var1.pred), 
               # alpha = 0.8, 
               size = 0) + 
  geom_point(data = hospitals_df, aes(x = lon, y = lat), color = "blue", alpha = 0.9) +
  geom_label(data = swisscities, aes(x = long, y = lat, label = name), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_alpha_continuous(guide = "none", range = c(0.1,1)) +
  theme_void() +
  labs(title = "How far to the closest emergency hospital?",
       subtitle = "An interpolation of driving times using OSRM",
       x = "", y = "", fill = "minutes\nto the next\nhospital") -> finalmap

finalmap
ggsave(filename = "finalmap.jpg", plot = finalmap)