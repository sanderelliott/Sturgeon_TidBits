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

tid.ast <- read.csv("OTN sheet/data/TidAST.csv")
tid.sns <- read.csv("OTN sheet/data/TidSNS.csv")
tid <- rbind(tid.ast, tid.sns)

gayle.otn <- read.csv("OTN sheet/data/GayleOTN.csv")

# Work ----

## Remove tags from tid already in form and pre 2015

glimpse(gayle.otn)
glimpse(tid)

gayle.ids <- paste(gayle.otn$TAG_CODE_SPACE, gayle.otn$TAG_ID_CODE, sep = "-")

tid.cln <- tid %>% 
  mutate(CaptureDate = as.Date(CaptureDate),
         y = year(CaptureDate)) %>% 
  filter(! TagID %in% gayle.ids,
         y > 2014) %>% 
  select(-y)


#write.csv(tid.cln, "OTN sheet/data/tid.cln.csv")

write.csv(tidclean, )

## Rework tid fields to match OTN Sheet 





