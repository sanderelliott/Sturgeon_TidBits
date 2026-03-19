### Script for assembling OTN Tag sheet with UMAINE and Tidbits Data

# Load ----

## Packages ----

library(tidyverse)

## Path ----

#gdrive_base <- "C:/Users/sande/My Drive (sander.elliott@maine.edu)"
gdrive_base <- "C:/Users/sander.elliott/My Drive (sander.elliott@maine.edu)"
gdrive_path <- file.path(gdrive_base, "Code", "Sturgeon_TidBits", "QAQC", "OTN sheet")

## Data ----
getwd()

tid.ast.hst <- read.csv("OTN sheet/data/TidAST.csv")
tid.sns.hst <- read.csv("OTN sheet/data/TidSNS.csv")
tid.hst <- rbind(tid.ast.hst, tid.sns.hst)

tid.ast.tag <- read.csv("OTN sheet/data/tid.AST.tag.csv") 
tid.sns.tag <- read.csv("OTN sheet/data/tid.SNS.tag.csv")
tid.tag <- rbind(tid.ast.tag, tid.sns.tag)

gayle.otn <- read.csv("OTN sheet/data/GayleOTN.csv")

# Work ----

## Fix dates

tag_new <- tid.tag %>%
  mutate(year = lubridate::year(lubridate::mdy_hm(CaptureDate)),
         CaptureDate = mdy_hm(CaptureDate)) %>% 
  filter(year >= 2015)

hst_new <- tid.hst %>%
  mutate(year = lubridate::year(lubridate::ymd_hms(CaptureDate)),
         CaptureDate = ymd_hms(CaptureDate)) %>% 
  filter(year >= 2015) %>% 
  group_by(FishID, CaptureDate) %>%
  slice(1) %>%
  ungroup()


## Combine history and tag

glimpse(tag_new)
glimpse(hst_new)
tid <- inner_join(tag_new, hst_new,
                  by = c("FishID", "CaptureDate"))
glimpse(tid)


glimpse(tid)

## fix tagID and remove ones already in sheet

glimpse(gayle.otn)
glimpse(tid)

gayle.ids <- paste(gayle.otn$TAG_CODE_SPACE, gayle.otn$TAG_ID_CODE, sep = "-")

tid.cln <- tid %>% 
  select(
    -matches("\\.x$"),
    -matches("\\.y$")) %>% 
  mutate(
         TagID = paste(AcousticCodeSpace, AcousticIDCode, sep = "-")) %>% 
  filter(! TagID %in% gayle.ids) %>% 
  st_as_sf(coords = c("Easting", "Northing"), crs = 32619) %>%   # UTM Zone 19N (WGS84)
  st_transform(4326) %>%                                         # convert to lat/long
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude  = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() 

glimpse(tid.cln)

write.csv(tid.cln, "OTN sheet/data/tid.cln.csv", row.names = FALSE, na = "")



## Lat Long

tid.cln <- tid.cln %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 32619) %>%   # UTM Zone 19N (WGS84)
  st_transform(4326) %>%                                         # convert to lat/long
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude  = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()



