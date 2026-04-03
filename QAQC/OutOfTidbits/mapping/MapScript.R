# Mapping tidbits detections ----

## Load ----

### Packages ----

library(tidyverse)
library(sf)
library(ggspatial)
library(raster)


### Paths ---- 

gdrive_pathh <- "C:/Users/sande/My Drive (sander.elliott@maine.edu)/Code/Sturgeon_TidBits"

### data ---- 

ast_hst <- read.csv(file.path(gdrive_pathh, "QAQC/OutOfTidbits/mapping/data/ast_hst.csv"))

sns_hst <- read.csv(file.path(gdrive_pathh, "QAQC/OutOfTidbits/mapping/data/sns_hst.csv"))

dep <- read.csv(file.path(gdrive_pathh, "QAQC/OutOfTidbits/mapping/data/deployments.csv"))

stg_det <- rbind(ast_hst, sns_hst) %>% 
  filter(Event == "Detection")

glimpse(stg_det)

## Prep ----

unique(stg_det$AltFishID)

stg_det_map <- stg_det %>%
  filter(!is.na(Easting),
         Easting != 999999) %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 26919) %>%   
  mutate(
    Easting  = st_coordinates(.)[,1],
    Northing = st_coordinates(.)[,2]) %>%
  st_transform(4326) %>%                                        
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2],
    AltFishID = case_when(AltFishID == "" ~ "Tidbits", 
                          .default = AltFishID),
    Period = as.Date(Period),
    Year = year(Period))

glimpse(stg_det_map) 

## Plot ----

### Without Map ----

ggplot(data = stg_det_map) +
  geom_point(aes(x = Longitude, y = Latitude, color = AltFishID), size = 2, alpha = 0.7) +
  labs(title = "Detections",
       x = "Longitude",
       y = "Latitude",
       color = "AltID") +
  theme_classic() 

### With Map ----


bb <- st_bbox(stg_det_map)
osm <- osm.raster(bb, type = "cartolight")

osm_df <- raster::as.data.frame(osm, xy = TRUE)

stg_det_map_3857 <- st_transform(stg_det_map, 3857)

ggplot() +
  geom_raster(
    data = osm_df,
    aes(x = x, y = y,
        fill = rgb(layer.1/255, layer.2/255, layer.3/255))
  ) +
  scale_fill_identity() +
  geom_sf(data = stg_det_map_3857,
          aes(color = AltFishID),
          size = 1, alpha = 0.7) +
  coord_sf(crs = 3857) +
  labs(title = "Detections",
       subtitle = "Base map from Carto",
       x = "Longitude",
       y = "Latitude",
       color = "Source") +
  theme_classic()

### By Year ----

years <- sort(unique(stg_det_map$Year))

n_years <- length(unique(stg_det_map$Year))

for (i in 1:length(years)) {
  year <- years[i]
  stg_det_map_i <- stg_det_map %>% 
    filter(Year == year)
  
  bb <- st_bbox(stg_det_map_i)
  osm <- osm.raster(bb, type = "cartolight")
  
  osm_df <- raster::as.data.frame(osm, xy = TRUE)
  
  stg_det_map_3857 <- st_transform(stg_det_map_i, 3857)
  
  mp <- ggplot() +
    geom_raster(
      data = osm_df,
      aes(x = x, y = y,
          fill = rgb(layer.1/255, layer.2/255, layer.3/255))
    ) +
    scale_fill_identity() +
    geom_sf(
      data = stg_det_map_3857,
      aes(color = AltFishID),
      size = 1, alpha = 0.7
    ) +
    scale_color_manual(
      values = c(
        "DMR Legacy" = "red",
        "Tidbits"    = "blue"
      )
    ) +
    coord_sf(crs = 3857) +
    labs(
      title = paste("Year", year),
      subtitle = "Detections",
      x = "Longitude",
      y = "Latitude",
      color = "Source"
    ) +
    theme_classic()

ggsave(paste0(gdrive_pathh, "/QAQC/OutOfTidbits/mapping/outputs/", year, ".jpg"), 
       mp)

}


## Deployments ----

glimpse(dep)

dep_map <- dep %>%
  filter(!is.na(Easting),
         Easting != 999999,
         TagType == "Acoustic") %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 26919) %>%   
  mutate(
    Easting  = st_coordinates(.)[,1],
    Northing = st_coordinates(.)[,2]) %>%
  st_transform(4326) %>%                                        
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude  = st_coordinates(.)[,2])

bb <- st_bbox(dep_map)
osm <- osm.raster(bb, type = "cartolight")

osm_df <- raster::as.data.frame(osm, xy = TRUE)

dep_map_3857 <- st_transform(dep_map, 3857)

ggplot() +
  geom_raster(
    data = osm_df,
    aes(x = x, y = y,
        fill = rgb(layer.1/255, layer.2/255, layer.3/255))
  ) +
  scale_fill_identity() +
  geom_sf(data = dep_map_3857) +
  coord_sf(crs = 3857) +
  labs(title = "Depl;oyments",
       x = "Longitude",
       y = "Latitude") +
  theme_classic()



## Pulling Problems

no_coords <- stg_det %>% 
  filter(is.na(Easting))

write.csv(no_coords, file.path(gdrive_pathh, "QAQC/OutOfTidbits/problems/nocoord.csv"), row.names = FALSE)

off_coords <- stg_det %>% 
  filter(Easting == 999999)

write.csv(off_coords, file.path(gdrive_pathh, "QAQC/OutOfTidbits/problems/off_coord.csv"), row.names = FALSE)
