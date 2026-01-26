getwd()

## Load Packages ----

#install.packages("readr")
#install.packages("readxl") 
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyr")

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# Load UMaine Data

um2006 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 1,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2007 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 2,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2008 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 3,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2009 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 4,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2010 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 5,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2011 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 6,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2012 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 7,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2013 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 8,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2014 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 9,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2015 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 10,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2016 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 11,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2017 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 12,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2018 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 13,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2019 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 14,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))
um2020 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 15,
                     na = c("", "none", "None", "NONE", " ", "NA", "N/A"))

names(um2006)

# Bin similar sets ----

all_um <- list(
  um2006 = um2006,
  um2007 = um2007,
  um2008 = um2008,
  um2009 = um2009,
  um2010 = um2010,
  um2011 = um2011,
  um2012 = um2012,
  um2013 = um2013,
  um2014 = um2014,
  um2015 = um2015,
  um2016 = um2016,
  um2017 = um2017,
  um2018 = um2018,
  um2019 = um2019,
  um2020 = um2020
)

colnames_list <- lapply(all_um, names)

unique_sets <- unique(colnames_list)

matches <- lapply(unique_sets, function(x) {
  names(which(sapply(colnames_list, identical, x)))
})

matches


bin0610 <- rbind(um2006, um2007, um2008, um2010)

#bin1213 <- rbind(um2012, um2013) ###didnt work

# transfer sheets to encounter form ----

## Read in example encounter forms

encexic <- read.csv("QAQC/IntoTidBits/Encounter Forms/2025/output/UEF_IC_AST_SNS_SE.csv")
encecrcnt <- read.csv("QAQC/IntoTidBits/Encounter Forms/2025/output/UEF_RCNT_AST_SNS_SE.csv")
  
## 2006 -2010 ----
tagid_cols0610 <- c("PIT ID", "Carlin ID", "Coded Type", "Cont. Type")

bin0610 <- bin0610 %>% 
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")

## Pull out multiple acoustic tags to deal with later

bin0610multac <- bin0610 %>% 
  filter(!is.na(`Coded Type`) & !is.na(`Cont. Type`))

## Initial capture dataset 

bin0610cln <- bin0610 %>%
         filter(!(!is.na(`Coded Type`) & !is.na(`Cont. Type`))) %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols0610)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(`Coded Type`) | !is.na(`Cont. Type`) ~ "Acoustic",
      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None"
    )
  ) %>%
  select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    Name     = replace_na(Name, ""),
    Comments = str_c(Comments, Name, sep = " "),
    Comments = str_squish(Comments),
    Comments = na_if(Comments, ""),
    `Mass (kg)` = as.numeric(`Mass (kg)`)
  )

bin0610ic <- bin0610cln %>% 
  filter(`Recap (y/n)` == "N")

tfn
names(bin0610ic)

tid0610ic <- tidsheet_inc(bin0610ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                          Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                          tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = NA_character_,
                          exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                          Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                          Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments)

tid0610ic <- tid0610ic %>% 
  mutate(Encounter_Disposition = case_when(str_detect(str_to_lower(Notes), "necropsy") ~ "Dead",
      TRUE ~ Encounter_Disposition),
      Release_Status = case_when(str_detect(str_to_lower(Notes), "necropsy") ~ "No",
                                        TRUE ~ Release_Status))


## NEED TO DO RIVER AND ACOUSTIC TAG INFORMATION DOWN THE ROAD ##

bin0610rc <- bin0610cln %>% 
  filter(`Recap (y/n)` == "Y")

tfn
names(bin0610rc)

tid0610rc <- tidsheet_rc(bin0610rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                         Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                         tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = NA_character_,
                         exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000), 
                         Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                         Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments)



## NEED TO WORK OUT EVENT AND OBSERVED TAGS DOWN ROAD WHEN ALL TOGETHER FOR FISH HISTORY ##



## 2011 ----

tagid_cols2011 <- c("PIT ID", "Carlin ID", "Coded Type")

um2011cln <- um2011 %>%
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2011)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(`Coded Type`) ~ "Acoustic",
      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  select(-n_ids) %>%
  mutate(
    Notes = str_c("Genetic ID", `gen. ID`, sep = " "))

## Initial Captures

um2011ic <- um2011cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2011ic)

tid2011ic <- tidsheet_inc(um2011ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location,
                          Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                          tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = NA_character_,
                          exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                          Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                          Outside.Mouth = `Outside Mouth (mm)`, Notes = Notes)


## Recaptures

um2011rc <- um2011cln %>% 
  filter(`Recap (y/n)` == "Y") 

tfn
names(um2011rc)

tid2011rc <- tidsheet_rc(um2011rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location,
                         Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                         tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = NA_character_,
                         exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                         Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                         Outside.Mouth = `Outside Mouth (mm)`, Notes = Notes)






















