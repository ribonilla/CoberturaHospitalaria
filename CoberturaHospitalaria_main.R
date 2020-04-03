library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(writexl)

library(osrm)
library(tidyverse)
#library(ggmap)
library(hereR)

library(leaflet)
library(sf)
library(raster)
library(gstat)
library(stringi)

setwd("C:/Users/rbonilla/Google Drive/work/TheProfessional/PersonalResearch/Github/NearestHospital")
source(file = "NearestHospital_Funciones.R", encoding = "utf8")
set_key("649v2eOmWyQoDCurYqPiKjhMgjarEbS70-drf7uDH3A")
getOption("osrm.server")

# Cuando se tenga un servidor de OpenStreet
#options(osrm.server = "http://localhost:5000/")
#getOption("osrm.server")

# Datos Hospitales ----
#hospitales <- read_excel(path = "c:/Users/rbonilla/Downloads/NearestHospital/kzp17_daten.xlsx", sheet = "KZ2017_KZP17")
hospitales <- read_csv("C:/Users/rbonilla/Downloads/NearestHospital/Registro_Especial_de_Prestadores_de_Servicios_de_Salud.csv", 
                                                                                locale = locale(asciify = TRUE))
# Filtro Bogota
hospitales <- hospitales %>% filter(nompio == "BOGOTÁ")

# Nombres Direcciones
hospitales <- hospitales %>% mutate_if(is.character, ~toupper(stri_trans_general(str = ., id = "ASCII-Latin")))

hospitales$direccion <- gsub(pattern = "[.]", replacement = " ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = " NO ", replacement = " ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "N°", replacement = " ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "Nº", replacement = " ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "Nª", replacement = " ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "#", replacement = " ", x = hospitales$direccion)



hospitales$direccion <- gsub(pattern = "CLL ", replacement = "CALLE ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "AV ", replacement = "AVENIDA ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "KRA ", replacement = "CARRERA ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "KR ", replacement = "CARRERA ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "AK ", replacement = "AVENIDA CARRERA ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "AC ", replacement = "AVENIDA CALLE ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "CRA ", replacement = "CARRERA ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "CR ", replacement = "CARRERA ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "CL ", replacement = "CALLE ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "TV ", replacement = "TRANSVERSAL ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "TRV ", replacement = "TRANSVERSAL ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "TRANV ", replacement = "TRANSVERSAL ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "TRANSV ", replacement = "TRANSVERSAL ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "DG ", replacement = "DIAGONAL ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "^K ", replacement = "CARRERA ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "NUM ", replacement = " ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "NUMERO ", replacement = " ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "AUTO ", replacement = "AUTOPISTA ", x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = "AUTOP N ", replacement = "AUTOPISTA NORTE ", x = hospitales$direccion)


hospitales$direccion <-gsub(pattern = "\\s*\\([^\\)]+\\)",replacement = " ",x = hospitales$direccion)
hospitales$direccion <- gsub(pattern = " OF.*| CN.*| CON.*| EDF.*| EDIF.*| LOC.*| PIS.*| ED.*| TO.*| BARR.*| CS.*| AP.*| IN.*| PI.*| Y .*| LC.*| PSO.*| P .*| P-.*| SEGUNDO.*| PLA.*| PRIM.*| 1ER.*| DENTRO.*| ENTRA.*| BODEG.*| SALA.*| CENTRO COM.*| DEPART.*| CASA.*| CC.*",
                            replacement = "", x = hospitales$direccion)

hospitales$direccion <- gsub(pattern = "-", replacement = " ", x = hospitales$direccion)
hospitales$direccion <- str_squish(string = hospitales$direccion)

emergency_hospitales <- hospitales %>% filter(nchar(direccion)>5) %>% dplyr::select(nombre, direccion, telefono, email, najunombre, ese, nitsnit)
emergency_hospitales$direccion <- paste(str_to_title(hospitales$direccion) , " Bogota, COLOMBIA", sep = "")

# three random hospitales
set.seed(33)
sample_n(emergency_hospitales, 3)

# number of hospitales
nrow(emergency_hospitales)

# HereR geocode

#geocode(addresses = "Transversal 55 108 35 Bogota, COLOMBIA")
#geocode("Calle 52 71d 37, Bogota, COLOMBIA")
#autocomplete(c("Unicentro, Bogota, Colombia", "Unicentro, Cali, Colombia"))
#reverse_geocode(poi = poi)



# RUN geocode
#emergency_hospitales_geocoded <- geocode(addresses = emergency_hospitales$direccion)


# WRITE
#emergency_hospitales_geocoded %>% write_rds(path = "emergency_hospitales_geocoded_BOGOTA.RDS")

# READ
emergency_hospitales_geocoded <- read_rds(path = "emergency_hospitales_geocoded_BOGOTA.RDS")

# crear LONG LAT ----
dfCoordinates <- data.frame(st_coordinates(x = emergency_hospitales_geocoded), stringsAsFactors = F)
names(dfCoordinates) <- c("lon", "lat")
emergency_hospitales_geocoded <- emergency_hospitales_geocoded %>% bind_cols(dfCoordinates)


# El archivo original tiene la Istitucion pero el encoded la posicion se pega por id ----
emergency_hospitales$id <- as.numeric(1:nrow(emergency_hospitales))

emergency_hospitales_geocoded <- left_join(x = emergency_hospitales_geocoded,
                                          y = dplyr::select(emergency_hospitales, -direccion),
                                          by = "id")

# Filtro "HOSPITAL" ----
emergency_hospitales_geocoded <- emergency_hospitales_geocoded %>% 
  filter(grepl(pattern = "HOSPITAL|CLINICA|UNIDAD", x = nombre)) %>%
  filter(!grepl(pattern = "ODON", x = nombre))
  #filter(!grepl(pattern = "MAIL", x = email))
  

# Raster MAP ----
icons <- iconList(
  hospital = makeIcon("location_3440906.png", iconWidth = 18, iconHeight = 18)
)

basicmap <- leaflet() %>% 
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$OpenStreetMap) %>% 
  addMarkers(data = emergency_hospitales_geocoded,
             lng = ~lon, lat = ~lat, popup = ~nombre,
             group = "IPS", icon = icons["hospital"])

basicmap %>% 
  addLayersControl(overlayGroups = c("hospitales"))

# First, let’s define the extremes that we want the points to be in, ----
# here the extremes of Switzerland. Those come from Wikipedia.

latmin <- 4.403634
latmax <- 4.807018
lonmin <- -74.219840
lonmax <- -74.008549

n <- 50

set.seed(1273)
randompts <- tibble(id=1:n,
                    lon = runif(n, min = lonmin, max = lonmax),
                    lat = runif(n, min = latmin, max = latmax))

basicmap %>% 
  addCircleMarkers(data = randompts, lng = ~lon, lat = ~lat, color = "red", radius = 1,
                   group = "Random Points") %>% 
  addLayersControl(overlayGroups = c("hospitales", "Random Points"))

# OSRM server we use osrm::osrmTable(src, dst) ----
# Note also that we need to input data frames with the columns id/lon/lat. Tibbles would not work.

randompoints_df <- randompts %>% 
  as.data.frame()

hospitales_df <- emergency_hospitales_geocoded %>% 
  as.data.frame() %>%
  dplyr::select(id = nombre, lon, lat)

t0 <- Sys.time()
distancetable <- osrmTable(src = randompoints_df, dst = hospitales_df)

Sys.time() - t0

distancetable %>% summary()

# Extract the minimal driving times ----
mindistances <- bind_cols(distancetable$sources, mintime = apply(distancetable$durations, 1, min)) %>% 
  as_tibble() %>% 
  mutate(mintime_str = as.character(mintime)) %>% 
  distinct()

binpal <- colorBin("Reds", domain = 0:max(mindistances["mintime"]))

basicmap %>% 
  addCircleMarkers(data = mindistances, lng = ~lon, lat = ~lat, radius = 2,
                   color = ~binpal(mintime), popup = ~mintime_str,
                   group = "Driving times") %>% 
  addLayersControl(overlayGroups = c("hospitales", "Driving times"))


# Therefore we just loop over smaller sized requests ----
# A total of 10’000 points would be great (100 requests at 100 each).

nruns <- 200
n <- 50

set.seed(1465)

for (r in 1:nruns) {
  
  randompts <- tibble(id=1:n,
                      lon = runif(n, min = lonmin, max = lonmax),
                      lat = runif(n, min = latmin, max = latmax))
  
  randompoints_df <- randompts %>% 
    as.data.frame()
  hospitales_df <- emergency_hospitales_geocoded %>% 
    as.data.frame() %>%
    dplyr::select(id = nombre, lon, lat)
  
  # request OSRM server
  t0 <- Sys.time()
  
  distancetable <- osrmTable(src = randompoints_df, dst = hospitales_df)
  #distancetable <- osrmTable(src = randompoints_df, dst = hospitales_df)
  
  rrt <- as.numeric(Sys.time() - t0, units = "secs") %>% round(3)
  
  mindistances_i <- bind_cols(distancetable$sources, mintime = apply(distancetable$durations, 1, min)) %>% 
    as_tibble() %>% 
    mutate(mintime_str = as.character(mintime))
  
  mindistances <- bind_rows(mindistances, mindistances_i)
  
  print(str_c("run: ", r, ", request response time: ", rrt, "secs"))
}

# Let’s remove duplicates and save ---- 
mindistances <- bind_rows(mindistances, 
                          hospitales_df %>% 
                            dplyr::select(lon, lat) %>% 
                            mutate(mintime = 0, mintime_str = "0"))

mindistances <- mindistances %>% 
  distinct()


# Read de distancias medida en minutos ----

#mindistances %>% write_rds("mindistances_BOGOTA.RDS")
#mindistances <- read_rds(path = "mindistances_BOGOTA.RDS")
mindistances <- mindistances %>% distinct() %>% filter(mintime<30)
mindistances$mintime <- mindistances$mintime*5
mindistances$mintime_str <- as.character(mindistances$mintime)
mindistances <- mindistances %>% filter(mintime!=0)

nrow(mindistances)
mindistances %>% ggplot(aes(x = mintime)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1, colour="black", fill="white") +
  geom_density(alpha = 0.2, fill = "#FF6666") 

#Let’s see it our map ----

binpal <- colorBin(palette = c("navy", "orange"), domain = 0:max(mindistances["mintime"]), bins = 20)

basicmap %>% 
  addCircleMarkers(data = mindistances, lng = ~lon, lat = ~lat, radius = 2,
                   color = ~binpal(mintime), popup = ~mintime_str,
                   group = "Driving times",
                   opacity = 0.8) %>%
  addLegend(data = mindistances, pal = binpal, values = ~mintime, group = "Driving times",
            title = "Time in min to the closest hospital") %>% 
  addLayersControl(overlayGroups = c("hospitales", "Driving times")) %>% 
  hideGroup("hospitales")


# The gridRes argument defines the raster resolution.
# A higher value will result in a more pixelated raster,
# a lower value will lead to very long computation times.
minraster <- interpolateSurface(mindistances, gridRes = 100)

# Read Interpolation de Bogota ----
#minraster %>% write_rds("minraster_BOGOTA.RDS")
#minraster <- read_rds("minraster_BOGOTA.RDS")

# Lastly, before we add it to the interactive map, ----
# we want to cut it according to the country’s border.
# For this we use a shape file from GADM that we can
# automatically download in R with raster::getData("GADM", country, level).
# We then use raster::mask() to cut the raster accordingly.

#switzerland <- getData("GADM", country = "CHE", level = 0)
bogota <- shapefile(x = "locashp_BOGOTA/Loca.shp")
bogota <- bogota %>% subset(!(LocNombre %in% c("SUMAPZ", "USME", "CIUDAD BOLIVAR")))

#switzerland_t <- spTransform(switzerland, CRSobj = "+init=epsg:3857")
bogota_t <- spTransform(bogota, CRSobj = "+init=epsg:3857")


#minraster_shaped <- mask(minraster$nn, switzerland_t)
minraster_shaped <- mask(minraster$nn, bogota_t)

binpal <- colorBin(palette = c("navy", "orange"), domain = 0:max(mindistances["mintime"]), bins = 20, na.color = NA)


basicmap %>% 
  addRasterImage(minraster_shaped, group = "Driving times", opacity = 0.8,
                 colors = binpal) %>% 
  
  addLayersControl(overlayGroups = c("hospitales", "Driving times")) %>%
  addLegend(data = mindistances, pal = binpal, values = ~mintime, group = "Driving times",
            title = "Time in min to the closest hospital") %>% 
  addLayersControl(overlayGroups = c("hospitales", "Driving times")) %>% 
  hideGroup("hospitales") %>%
  addCircleMarkers(data = mindistances, lng = ~lon, lat = ~lat, radius = 5,
                   color = ~binpal(mintime), popup = ~mintime_str,
                   group = "Driving times",
                   opacity = 0.2, fillOpacity = 0.2, weight = 1)



# Create a static map ----
# Next, we need to tweak the raster we created a little,
# because ggplot2 cannot plot those directly.
# Probably there would be a more elegant way of doing this, but it works.

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
  geom_point(data = hospitales_df, aes(x = lon, y = lat), color = "blue", alpha = 0.9) +
  geom_label(data = swisscities, aes(x = long, y = lat, label = name), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_alpha_continuous(guide = "none", range = c(0.1,1)) +
  theme_void() +
  labs(title = "How far to the closest emergency hospital?",
       subtitle = "An interpolation of driving times using OSRM",
       x = "", y = "", fill = "minutes\nto the next\nhospital") -> finalmap

finalmap
ggsave(filename = "finalmap.jpg", plot = finalmap)