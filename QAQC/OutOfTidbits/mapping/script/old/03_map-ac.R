library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(lubridate)
library(ggmap)


##Find year with lots of AST
ASTyrsum <- asthst_ac %>% 
  filter(Event == "Detection") %>% 
  mutate(Period = mdy_hm(Period),
    Year = year(Period),
    Month = month(Period)) %>% 
  group_by(Year) %>% 
  summarise(n_fish = n_distinct(FishID),
            n_months = n_distinct(Month),
            n_detections = n())
  


### Show where the sturgeon are on a daily basis in 2024
stg_avg_day <- stghst_ac %>% 
  group_by(FishID, Period) %>% 
  reframe(avg_east = mean(Easting),
            avg_north = mean(Northing),
          Species) %>% 
  filter(! is.na(avg_north)) %>% 
  mutate(Period = mdy_hm(Period)) %>% 
  filter(year(Period) == 2024)
head(stg_avg_day)

#Convert to sf 
stg_avg_day_sf <- stg_avg_day %>%
  st_as_sf(coords = c("avg_east", "avg_north"), crs = 26919) %>%
  st_transform(crs = 4326)  # Convert to lat/lon

# Convert back to dataframe for faster plotting
stg_avg_day_df <- stg_avg_day_sf %>%
  mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) %>%
  as.data.frame()
head(stg_avg_day_df)


# Plot without background map first

ggplot(data = stg_avg_day_df) +
  geom_point(aes(x = Longitude, y = Latitude, color = Species), size = 2, alpha = 0.7) +
  labs(title = "Average Fish Locations by Day",
       x = "Longitude",
       y = "Latitude",
       color = "Species") +
  theme_minimal() 


##Plot with map
ggplot(data = stg_avg_day_df) +
  annotation_map_tile(type = "cartolight", zoom = 12) +  
  geom_point(aes(x = Longitude, y = Latitude, color = Species), size = 2, alpha = 0.7) +
  coord_sf(crs = 4326, 
           xlim = c(min(stg_avg_day_df$Longitude) - 0.15, 
                    max(stg_avg_day_df$Longitude) + 0.15),
           ylim = c(min(stg_avg_day_df$Latitude) - 0.05,  
                    max(stg_avg_day_df$Latitude) + 0.05),
           expand = FALSE) + 
  labs(title = "Average Fish Locations by Day 2024",
       subtitle = "Base map from Carto",
       x = "Longitude",
       y = "Latitude",
       color = "Species") +
  theme_minimal()


### Plot all detection locations
detloc <- stghst_ac %>% 
  filter(Event == "Detection",
         ! DeploymentUID %in% troubling_sturgeon_detections$DeploymentUID) %>% 
  reframe(Easting, Northing) %>% 
  unique() 

detloc_sf <- detloc %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 26919) %>%
  st_transform(crs = 4326)

# Convert back to dataframe for faster plotting
detloc_df <- detloc_sf %>%
  mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) %>%
  as.data.frame()
head(detloc_df)


# Plot detloc without background map first

ggplot(data = detloc_df) +
  geom_point(aes(x = Longitude, y = Latitude), size = 2, alpha = 0.7) +
  labs(title = "All Acoustic Detection Locations for Sturgepn in Tidbits",
       x = "Longitude",
       y = "Latitude")  +
  theme_minimal() 



##Plot with map

ggplot(data = detloc_df) +
  annotation_map_tile(type = "cartolight", zoom = 12) +  
  geom_point(aes(x = Longitude, y = Latitude), size = 2, alpha = 0.7) +
  coord_sf(crs = 4326, 
           xlim = c(min(detloc_df$Longitude) - 0.15, 
                    max(detloc_df$Longitude) + 0.15),
           ylim = c(min(detloc_df$Latitude) - 0.05,  
                    max(detloc_df$Latitude) + 0.05),
           expand = FALSE) + 
  labs(title = "All Acoustic Detection Locations for Sturgeon in Tidbits",
       subtitle = "Base map from Carto",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


### Map 2017 AST

AST2017 <- asthst_ac %>% 
  bind_rows(sns_susp_det) %>% 
  filter(Event == "Detection") %>% 
  mutate(Period = mdy_hm(Period),
         Year = year(Period),
         Month = month(Period)) %>% 
  filter(Year == "2017")



#Convert to sf 
AST2017_sf <- AST2017 %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 26919) %>%
  st_transform(crs = 4326)  # Convert to lat/lon

# Convert back to dataframe for faster plotting
AST2017_df <- AST2017_sf %>%
  mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) %>%
  as.data.frame()
head(AST2017_df)


# Plot without background map first

ggplot(data = AST2017_df) +
  geom_point(aes(x = Longitude, y = Latitude), size = 2, alpha = 0.7) +
  labs(title = "Atlantic Sturgeon Detections",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() 



##Plot with map

ggplot(data = AST2017_df) +
  annotation_map_tile(type = "cartolight", zoom = 10) +  
  geom_point(aes(x = Longitude, y = Latitude), size = 2, alpha = 0.7) +
  coord_sf(crs = 4326, 
           xlim = c(min(AST2017_df$Longitude) - 0.15, 
                    max(AST2017_df$Longitude) + 0.15),
           ylim = c(min(AST2017_df$Latitude) - 0.05,  
                    max(AST2017_df$Latitude) + 0.05),
           expand = FALSE) + 
  facet_wrap(~ Month) +  # Create a separate map for each month
  labs(title = "AST Detections (2017)",
       subtitle = "Base map from Carto",
       x = "Longitude",
       y = "Latitude",
       color = "Species") +
  theme_minimal()


###Move
library(move)
library(moveVis)

AST2017sum <- AST2017 %>% 
  group_by(FishID) %>% 
  summarise(n_locations = n_distinct(Northing),
            n_det = n()) 


AST2017_df$FishID <- as.factor(AST2017_df$FishID)
AST_movevis <- AST2017_df %>% group_by(FishID) %>% filter(!FishID == "AST-2009-007",
                                                          !FishID == "AST-2017-001",
                                                          !FishID == "AST-2017-008",
                                                          !FishID == "AST-2017-006",
                                                          !duplicated(Period))

AST_movevis_df <- as.data.frame(AST_movevis)
AST_movevis_df$FishID <- factor(AST_movevis_df$FishID, levels = unique(AST_movevis_df$FishID))

# Use an updated projection string for WGS84
proj_string <- "+proj=longlat +datum=WGS84 +no_defs"

# Create the move object
move_data <- move(
  x = AST_movevis_df$Longitude,
  y = AST_movevis_df$Latitude,
  time = AST_movevis_df$Period,
  proj = proj_string,
  animal = AST_movevis_df$FishID,
  data = AST_movevis_df
)


m <- align_move(move_data, res = 1, unit = "days")

ext <- extent(m)
ext@xmin <- ext@xmin - 1
ext@xmax <- ext@xmax + 1



frames <- frames_spatial(m, map_service = "esri", map_type = "world_ocean_base", 
                         alpha = 0.7, path_size = .5, equidistant = FALSE, margin_factor = 1.2, path_legend = TRUE) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%  # Remove "m" here
  add_progress()

plot(frames[[100]])


animate_frames(frames, out_file = "G:/My Drive/Umaine R/Sturgeon Background/figs/ast7.mp4",
               fps = 8)


##SNS 2017

SNS2017 <- snshst_ac %>% 
  filter(!FishID %in% snsfsh_ac_susp$FishID,
         Event == "Detection") %>% 
  mutate(Period = mdy_hm(Period),
         Year = year(Period),
         Month = month(Period)) %>% 
  filter(Year == "2017")

#Convert to sf 
SNS2017_sf <- SNS2017 %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 26919) %>%
  st_transform(crs = 4326)  # Convert to lat/lon

# Convert back to dataframe for faster plotting
SNS2017_df <- SNS2017_sf %>%
  mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) %>%
  as.data.frame()
head(AST2017_df)


# Plot without background map first

ggplot(data = SNS2017_df) +
  geom_point(aes(x = Longitude, y = Latitude), size = 2, alpha = 0.7) +
  labs(title = "Shornose Sturgeon Detections",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() 



##Plot with map

ggplot(data = SNS2017_df) +
  annotation_map_tile(type = "cartolight", zoom = 10) +  
  geom_point(aes(x = Longitude, y = Latitude), size = 2, alpha = 0.7) +
  coord_sf(crs = 4326, 
           xlim = c(min(AST2017_df$Longitude) - 0.15, 
                    max(AST2017_df$Longitude) + 0.15),
           ylim = c(min(AST2017_df$Latitude) - 0.05,  
                    max(AST2017_df$Latitude) + 0.05),
           expand = FALSE) + 
  facet_wrap(~ Month) +  # Create a separate map for each month
  labs(title = "SNS Detections (2017)",
       subtitle = "Base map from Carto",
       x = "Longitude",
       y = "Latitude",
       color = "Species") +
  theme_minimal()


###Move

SNS20171loc <- SNS2017 %>% 
  group_by(FishID) %>% 
  summarise(n_locations = n_distinct(Northing)) %>% 
  filter(n_locations < 3)


library(move)
library(moveVis)

SNS2017_df$FishID <- as.factor(SNS2017_df$FishID)
SNS_movevis <- SNS2017_df %>% group_by(FishID) %>% filter(!FishID %in% SNS20171loc$FishID,
                                                          !duplicated(Period))

SNS_movevis_df <- as.data.frame(SNS_movevis)
SNS_movevis_df$FishID <- factor(SNS_movevis_df$FishID, levels = unique(SNS_movevis_df$FishID))

# Use an updated projection string for WGS84
proj_string <- "+proj=longlat +datum=WGS84 +no_defs"

# Create the move object
move_data <- move(
  x = SNS_movevis_df$Longitude,
  y = SNS_movevis_df$Latitude,
  time = SNS_movevis_df$Period,
  proj = proj_string,
  animal = SNS_movevis_df$FishID,
  data = SNS_movevis_df
)


m <- align_move(move_data, res = 1, unit = "days")

ext <- extent(m)
ext@xmin <- ext@xmin - 1
ext@xmax <- ext@xmax + 1



frames <- frames_spatial(m, map_service = "esri", map_type = "world_ocean_base", 
                         alpha = 0.7, path_size = .5, equidistant = FALSE, margin_factor = 1.2, path_legend = FALSE) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%  # Remove "m" here
  add_progress()

plot(frames[[100]])


animate_frames(frames, out_file = "G:/My Drive/Umaine R/Sturgeon Background/figs/SNS2.mp4",
               fps = 8)






# Loop through each month and generate individual plots
for (m in unique(stg_avg_day_df$month)) {
  
  # Filter data for the current month
  month_data <- filter(stg_avg_day_df, month == m)
  
  # Create the plot with facets for Species
  p <- ggplot(data = month_data) +
    annotation_map_tile(type = "cartolight", zoom = 12) +  
    geom_point(aes(x = Longitude, y = Latitude, color = Species), size = 2, alpha = 0.7) +
    coord_sf(crs = 4326, 
             xlim = c(min(stg_avg_day_df$Longitude) - 0.15, 
                      max(stg_avg_day_df$Longitude) + 0.15),
             ylim = c(min(stg_avg_day_df$Latitude) - 0.05,  
                      max(stg_avg_day_df$Latitude) + 0.05),
             expand = FALSE) + 
    facet_wrap(~ Species) +  # Side-by-side maps for species
    labs(title = paste("Mean Daily Fish Locations - Month", m, "(2024)"),
         subtitle = "Base map from Carto",
         x = "Longitude",
         y = "Latitude",
         color = "Species") + 
    scale_color_manual(values = c("AST" = "blue", "SNS" = "red"),
                       labels = c("AST" = "Atlantic Sturgeon", "SNS" = "Shortnose Sturgeon")) + 
    theme_minimal()
  
  # Save each month's map as a PNG file
  ggsave(filename = paste0("figs/Fish locs by month/Fish_Map_2024_", m, ".png"), plot = p, width = 10, height = 5)
}




detloc_year <- stghst_ac %>% 
  filter(Event == "Detection",
         ! DeploymentUID %in% troubling_sturgeon_detections$DeploymentUID) %>% 
  mutate(Period = mdy_hm(Period), 
         Year = year(Period)) %>% 
  reframe(Easting, Northing, Year) %>% 
  unique() 
head(detloc_year)


detloc_year_sf <- detloc_year %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 26919) %>%
  st_transform(crs = 4326)

# Convert back to dataframe for faster plotting
detloc_year_df <- detloc_year_sf %>%
  mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) %>%
  as.data.frame()
head(detloc_year_df)


# Plot detloc without background map first

ggplot(data = detloc_year_df) +
  geom_point(aes(x = Longitude, y = Latitude), size = 2, alpha = 0.7) +
  labs(title = "All Acoustic Detection Locations for Sturgepn in Tidbits",
       x = "Longitude",
       y = "Latitude")  +
  theme_minimal() 


head(detloc_year_df)
  
### Show detections by year
for (y in unique(detloc_year_df$Year)) {
  
  # Filter data for the current year
  year_data <- filter(detloc_year_df, Year == y)
  
  # Create the plot
  p <- ggplot(data = year_data) +
    annotation_map_tile(type = "cartolight", zoom = 10) +  
    geom_point(aes(x = Longitude, y = Latitude), size = 2, alpha = 0.7) +
    coord_sf(crs = 4326, 
             xlim = c(min(detloc_year_df$Longitude) - 0.15, 
                      max(detloc_year_df$Longitude) + 0.15),
             ylim = c(min(detloc_year_df$Latitude) - 0.05,  
                      max(detloc_year_df$Latitude) + 0.05),
             expand = FALSE) + 
    labs(title = paste("Sturgeon Detection Locations", y),
         subtitle = "Base map from Carto",
         x = "Longitude",
         y = "Latitude") + 
    theme_minimal()
  
  # Save each years's map as a jpeg file
  ggsave(filename = paste0("figs/Fish locs by year/sturgeon_map_", y, ".jpeg"), plot = p, width = 10, height = 5)
}


### Heatmap

register_google(key = "AIzaSyBUBnJcHBkqmqHyeZxCslHqXOVBfoIX_MQ") # If you need Google API key for `ggmap`

# Load your data, assuming it's already available in `stg_avg_day_df`

# Set up the bounding box for the map using min and max latitudes and longitudes
bbox <- c(left = -69.2,
          bottom = 44.4,
          right = -68.6,
          top = 44.9)

# Get the map with CartoDB (light) tiles
base_map <- get_map(location = c(left = -69.2,
                      bottom = 44.4,
                      right = -68.6,
                      top = 44.9), maptype = "roadmap",
                    zoom = 10)

# plot map
ggmap(base_map) +
  geom_point(data = stg_avg_day_df, aes(x = Longitude, y = Latitude, color = Species), size = 2, alpha = 0.7) +
  facet_wrap(~ Species) + 
  labs(title = "Fish Locations by Species (2024)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


# Plot the heatmap
ggmap(base_map) +
  stat_density2d(data = stg_avg_day_df, aes(x = Longitude, y = Latitude, fill = after_stat(level)), 
                 geom = "polygon", alpha = 0.5, bins = 15) +
  scale_fill_viridis_c(option = "magma", name = "Density") +  # Adjust color scale
  facet_wrap(~ Species) +  # Separate maps by month and species
  labs(title = "Fish Density Heatmap (2024)",
       subtitle = "Base map from Carto",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()








