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
library(rgdal)
#library(sp)

# Interpolation ----
interpolateSurface <- function(data, gridRes = 500, idp = 1){
  
  # Make new dataset, that will be spatial - sp:: class
  data_sp <- data %>% as.data.frame()
  # Make data as spatial object - spatialPointsDataFrame
  coordinates(data_sp) <- ~lon+lat
  # Define CRS - coordinate reference system
  proj4string(data_sp) <- crs("+init=epsg:4326")
  # Make it as of object of sf:: class
  crs <- "+init=epsg:4326"
  coords <- c("lon", "lat")
  # (sf: Simple feature collection)
  data_sf <- st_as_sf(data_sp, coords = coords, crs = crs)
  # WEB Mercator projection
  data_sp_mp <- spTransform(data_sp, CRSobj = "+init=epsg:3857")
  
  # Bounding box, resolution and grid for interpolation
  pts = spsample(x = spTransform(ciudad, CRSobj = "+init=epsg:3857"), type = "regular", n = 10000)
  proj4string(pts) <- crs("+init=epsg:3857")
  
  # Interpolate the grid cells Nearest neighbour
  r.raster <- raster::raster()
  extent(r.raster) <- extent(pts) # set extent
  res(r.raster) <- gridRes # 500 # set cell size
  crs(r.raster) <- crs("+init=epsg:3857") # set CRS
  gs <- gstat(formula = mintime~1, 
              locations = data_sp_mp, 
              nmax = 100, 
              set = list(idp = idp))
  nn <- interpolate(r.raster, gs)
  
  # Result rasters - surfaces
  data_list <- list(data_sf, nn)
  names(data_list) <- c("data", "nn")
  return(data_list)
}



# The gridRes argument defines the raster resolution.
# A higher value will result in a more pixelated raster,
# a lower value will lead to very long computation times.
#minraster <- interpolateSurface(mindistances, gridRes = 500)

# Funcion de escalamiento para los tiempos

ScaleFunction <- function(x, L = 1, k = 1, x0 = 0){
  x = L / (1 + exp(-k * (x-x0)))
  return(x+1)
}

#ScaleFunction(20, k = 0.05)


# Estandarizacion Direcciones ----
EstandarizarDirecciones <- function(direcciones){
 
  direcciones <- toupper(stri_trans_general(str = direcciones, id = "ASCII-Latin"))
  
  direcciones <- gsub(pattern = "[.]", replacement = " ", x = direcciones)
  direcciones <- gsub(pattern = " NO ", replacement = " ", x = direcciones)
  direcciones <- gsub(pattern = "N°", replacement = " ", x = direcciones)
  direcciones <- gsub(pattern = "Nº", replacement = " ", x = direcciones)
  direcciones <- gsub(pattern = "Nª", replacement = " ", x = direcciones)
  direcciones <- gsub(pattern = "#", replacement = " ", x = direcciones)
  
  direcciones <- gsub(pattern = "CLL ", replacement = "CALLE ", x = direcciones)
  direcciones <- gsub(pattern = "AV ", replacement = "AVENIDA ", x = direcciones)
  direcciones <- gsub(pattern = "KRA ", replacement = "CARRERA ", x = direcciones)
  direcciones <- gsub(pattern = "KR ", replacement = "CARRERA ", x = direcciones)
  direcciones <- gsub(pattern = "AK ", replacement = "AVENIDA CARRERA ", x = direcciones)
  direcciones <- gsub(pattern = "AC ", replacement = "AVENIDA CALLE ", x = direcciones)
  direcciones <- gsub(pattern = "CRA ", replacement = "CARRERA ", x = direcciones)
  direcciones <- gsub(pattern = "CR ", replacement = "CARRERA ", x = direcciones)
  direcciones <- gsub(pattern = "CL ", replacement = "CALLE ", x = direcciones)
  direcciones <- gsub(pattern = "TV ", replacement = "TRANSVERSAL ", x = direcciones)
  direcciones <- gsub(pattern = "TRV ", replacement = "TRANSVERSAL ", x = direcciones)
  direcciones <- gsub(pattern = "TRANV ", replacement = "TRANSVERSAL ", x = direcciones)
  direcciones <- gsub(pattern = "TRANSV ", replacement = "TRANSVERSAL ", x = direcciones)
  direcciones <- gsub(pattern = "DG ", replacement = "DIAGONAL ", x = direcciones)
  direcciones <- gsub(pattern = "^K ", replacement = "CARRERA ", x = direcciones)
  direcciones <- gsub(pattern = "NUM ", replacement = " ", x = direcciones)
  direcciones <- gsub(pattern = "NUMERO ", replacement = " ", x = direcciones)
  direcciones <- gsub(pattern = "AUTO ", replacement = "AUTOPISTA ", x = direcciones)
  direcciones <- gsub(pattern = "AUTOP N ", replacement = "AUTOPISTA NORTE ", x = direcciones)
  
  
  direcciones <-gsub(pattern = "\\s*\\([^\\)]+\\)",replacement = " ",x = direcciones)
  direcciones <- gsub(pattern = " OF.*| CN.*| CON.*| EDF.*| EDIF.*| LOC.*| PIS.*| ED.*| TO.*| BARR.*| CS.*| AP.*| IN.*| PI.*| Y .*| LC.*| PSO.*| P .*| P-.*| SEGUNDO.*| PLA.*| PRIM.*| 1ER.*| DENTRO.*| ENTRA.*| BODEG.*| SALA.*| CENTRO COM.*| DEPART.*| CASA.*| CC.*",
                               replacement = "", x = direcciones)
  
  direcciones <- gsub(pattern = "-", replacement = " ", x = direcciones)
  direcciones <- str_squish(string = direcciones)
  
  return(direcciones)
}
