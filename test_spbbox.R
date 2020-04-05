library(caret)


hospitals_df <- hospitales_Seleccionados_geocoded %>% 
  as.data.frame() %>%
  dplyr::select(id = nombre, lon, lat)



nmax <- 1000
intervals <- 20

pts = spsample(x = spTransform(ciudad, CRSobj = "+init=epsg:4326"), type = "nonaligned", n = nmax)
pts1 = coordinates(pts) %>% as.data.frame()

randompts_df <- data.frame(id=1:nrow(pts1),
                    lon = pts1$x1,
                    lat = pts1$x2)

kFolds <- createFolds(y = randompts_df$id, k = intervals)

length(kFolds)

# Random Seed
set.seed(2365)
mindistances = data.frame()

for (id in 1:length(kFolds)) {
  
  
  
  # request OSRM server
  t0 <- Sys.time()
  
  distancetable <- osrmTable(src = randompts_df[kFolds[[id]],] , dst = hospitals_df)
  #distancetable <- osrmTable(src = randompoints_df, dst = hospitals_df)
  
  rrt <- as.numeric(Sys.time() - t0, units = "secs") %>% round(3)
  
  #El minimo calculdo aquí es por fila
  mindistances_i <- bind_cols(distancetable$sources, mintime = apply(distancetable$durations, 1, min)) %>% 
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

