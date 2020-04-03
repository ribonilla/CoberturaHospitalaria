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
interpolateSurface <- function(data, gridRes = 500){
  
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
  boxx = data_sp_mp@bbox
  deltaX = as.integer((boxx[1,2] - boxx[1,1]) + 1.5)
  deltaY = as.integer((boxx[2,2] - boxx[2,1]) + 1.5)
  gridSizeX = deltaX / gridRes
  gridSizeY = deltaY / gridRes
  grd = GridTopology(boxx[,1], c(gridRes,gridRes), c(gridSizeX,gridSizeY))
  pts = SpatialPoints(coordinates(grd))
  proj4string(pts) <- crs("+init=epsg:3857")
  
  # Interpolate the grid cells Nearest neighbour
  r.raster <- raster::raster()
  extent(r.raster) <- extent(pts) # set extent
  res(r.raster) <- gridRes # 500 # set cell size
  crs(r.raster) <- crs("+init=epsg:3857") # set CRS
  gs <- gstat(formula = mintime~1, 
              locations = data_sp_mp, 
              nmax = 100, 
              set = list(idp = 0))
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

#ScaleFunction(1, x0 = 10)


# Estandarizacion Direcciones ----

