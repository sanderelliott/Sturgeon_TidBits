head(stgcmb)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(lubridate)



### Create dataset with only one event per day to remove fish going back and forth between recievers multiple times skewing data
stgcmb_penobscot_day <- stgcmb_penebscot %>% 
  reframe(Period, Species, FishID, RKM) %>% 
  unique()
head(stgcmb_penobscot_day)


### Plot of all historic etections by river kilometer by species 
ggplot(stgcmb_penobscot_day, aes(x = RKM, fill = Species)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Number of Historic Detections by RKM",
    x = "RKM (River Kilometer)",
    y = "Number of Detections",
    fill = "Species"
  ) +
  scale_fill_manual(
    values = c("AST" = "blue", "SNS" = "red"),
    labels = c("AST" = "Atlantic Sturgeon", "SNS" = "Shortnose Sturgeon")
  ) +
  theme_minimal()


### make the same plot for each species individually





