library(readr)
library(dplyr)

names(alldays)
sizeclass <- alldays %>% 
  mutate(FL = case_when(FL == 1 ~ 109,
                         TRUE ~ FL),
         Sex = case_when( Sex %in% c("M", "F") ~ Sex,
                          TRUE ~ "UNK"),
    class = case_when(Species == "AST" & FL < 100 ~ "juvenile", 
                            Species == "AST" & FL >= 100 & FL <= 130 ~ "sub-adult", 
                            Species == "AST" & FL > 130 ~ "adult",
                            Species == "SNS" & FL < 45 ~ "juvenile", 
                            Species == "SNS" & FL >= 45 & FL <= 60 ~ "sub-adult", 
                            Species == "SNS" & FL > 60 ~ "adult"))
juve <- sizeclass %>% 
  filter(class == "juvenile")

glimpse(sizeclass)

iaf_tags <- sizeclass %>% 
  transmute(Species = Species,
           System = River,
           PIT = PIT.CODE,
           Acoustic = V16.CODE.Full,
           SizeClass = class,
           ForkLength = FL,
           TotalLength = TL)

write.csv(iaf_tags, 
          "C:/Users/sander.elliott/OneDrive - University of Maine System/Desktop/Github/Sturgeon_TidBits/QAQC/Outputs/2025TagListSTG.csv",
          row.names = FALSE)
