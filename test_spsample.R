
pts = spsample(x = spTransform(ciudad, CRSobj = "+init=epsg:3857"), type = "regular", n = 10000)

proj4string(pts) <- crs("+init=epsg:3857")

# Interpolate the grid cells Nearest neighbour
r.raster <- raster::raster()
extent(r.raster) <- extent(pts) # set extent
#res(r.raster) <- gridRes # 500 # set cell size
res(r.raster) <- 50 # 500 # set cell size
crs(r.raster) <- crs("+init=epsg:3857") # set CRS

#r.raster <- raster(spTransform(ciudad, CRSobj = "+init=epsg:3857"), resolution = 100000)


gs <- gstat(formula = mintime~1, 
            locations = data_sp_mp, 
            nmax = 100, 
            set = list(idp = 0))

nn <- interpolate(r.raster, gs)

# Result rasters - surfaces
data_list <- list(data_sf, nn)
names(data_list) <- c("data", "nn")

minraster <- data_list

minraster_shaped <- mask(minraster$nn, spTransform(ciudad, CRSobj = "+init=epsg:3857"))

binpal <- colorBin(palette = c("navy", "orange"), domain = 0:max(mindistances["mintime"]), bins = 20, na.color = NA)

basicmap %>% 
  addRasterImage(minraster_shaped, group = "Driving times", opacity = 0.9,
                 colors = binpal) %>% 
  
  addLayersControl(overlayGroups = c("Hospitals", "Driving times")) %>%
  
  addLegend(data = mindistances, pal = binpal, values = ~mintime, group = "Driving times",
            title = "Time in min to the closest hospital") %>%
  
  hideGroup("Hospitals") %>%
  addPolygons(data = ciudad_t, group = "Comunas", color = "black", opacity = 1, weight = 1, dashArray = "3",
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



