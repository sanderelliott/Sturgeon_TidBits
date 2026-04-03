setwd("G:/My Drive/Umaine R/Sturgeon Background")
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(lubridate)

astfsh_ac <- read.csv("data/astfsh_ac.csv")
snsfsh_ac <- read.csv("data/snsfsh_ac.csv")
asthst_ac <- read.csv("data/asthst_ac.csv")
snshst_ac <- read.csv("data/snshst_ac.csv")
deployments <- read.csv("data/deployments.csv")



stgfsh_ac <- rbind(snsfsh_ac, astfsh_ac)[-c(213, 269), ]
###import from excel had two completely blank rows. all fish had capture location so I used is.na to remove blank rows
stghst_ac <- rbind(snshst_ac, asthst_ac)[-c(100455, 116868), ]
  

names(stghst_ac)

stghst_ac <- stghst_ac %>% select(Period, Species, FishID, Event, pings, FirstTS, LastTS, Easting, Northing, 
                   RKM, SiteCode, DeploymentUID)
head(stghst_ac)

names(stgfsh_ac)

stgfsh_ac <- stgfsh_ac %>% 
  select(FishID, Species, ForkLength, TotalLength, Mass, Sex, 
          Interorbital, InsideMouth, OutsideMouth, CaptureDate,  Recapture, System)
head(stgfsh_ac)

stgnorcp <- stgfsh_ac %>% filter(! Recapture == 1) %>% 
  select(FishID, ForkLength, TotalLength, Mass, Sex, 
          Interorbital, InsideMouth, OutsideMouth, CaptureDate) %>% 
  unique()
head(stgnorcp)

stgcmb_ac <- stghst_ac %>% 
  left_join(stgnorcp, by = "FishID")


stg_2024 <- stgcmb_ac %>% 
  mutate(Period = mdy_hm(Period),
         CaptureDate = mdy_hm(CaptureDate),
         FirstTS = mdy_hm(FirstTS),
         LastTS = mdy_hm(LastTS)) %>% 
  filter(year(Period) == 2024) %>% 
  filter(Event == "Detection")
head(stg_2024)

head(snsfsh_ac)

snsfsh_ac_susp <- snsfsh_ac %>%
  filter(grepl("AST", FishID))


head(snsfsh_ac_susp)

troubling_sturgeon_detections <- stghst_ac %>% 
  filter(Event == "Detection",
         DeploymentUID == "6841" | DeploymentUID == "5539" | DeploymentUID == "6842" | DeploymentUID == "53" | 
           is.na(Easting))
head(troubling_sturgeon_detections)



sns_susp_det <- stghst_ac %>% 
  filter(FishID %in% snsfsh_ac_susp$FishID)
head(sns_susp_det)


#write.csv(stg_2024, "data/stg2024.csv")
#write.csv(snsfsh_ac_susp, "data/sns_susp.csv")
  
