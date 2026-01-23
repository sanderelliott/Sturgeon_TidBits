getwd()

## Load Packages ----
library(readr)
library(readxl)
library(dplyr)

# Load UMaine Data

um2006 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 1)
um2007 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 2)
um2008 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 3)
um2009 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 4)
um2010 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 5)
um2011 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 6)
um2012 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 7)
um2013 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 8)
um2014 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 9)
um2015 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 10)
um2016 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 11)
um2017 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 12)
um2018 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 13)
um2019 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 14)
um2020 <- read_excel("QAQC/IntoTidBits/UMaineHistoricData/data/Penobscot Capture Data 2006 - 2020.xlsx", sheet = 15)

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

bin1213 <- rbind(um2012, um2013)

# transfer sheets to encounter form ----

## Read in example encounter forms

encexic <- read.csv("QAQC/IntoTidBits/Encounter Forms/2025/output/UEF_IC_AST_SNS_SE.csv")
encecrcnt <- read.csv("QAQC/IntoTidBits/Encounter Forms/2025/output/UEF_RCNT_AST_SNS_SE.csv")
  
## 2006 -2010 ----

names(bin0610)

tid0610icmult <- bin0610 %>%
  filter(`Recap (y/n)` == "N",
         Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")
         
         tagtype == "Multiple") %>% 
  transmute(
    Event = event,
    Species = Species,
    Rearing_Origin = "Wild",
    Stage = NA,
    System = River,
    Encounter_Time_zone = "Eastern",
    Encounter_Timestamp = Date,
    Encounter_Location = Site,
    Encounter_Easting  = Easting,
    Encounter_Northing = Northing,
    Encounter_UTMZone  = "19",
    Encounter_Disposition = "Live",
    Release_Status = "Yes",
    Release_Timezone = "Eastern",
    Release_Timestamp = Date,
    Release_Location = Site,
    Release_Easting = Easting,
    Release_Northing = Northing,
    TagType = tagtype,
    TagManufacturer = "Innovosea",
    TagModel = "V16-4x-BLU-1",
    TagSerialNumber = Serial_N,
    EstTagLife = "2560",
    Acoustic_ID = V16.CODE.Full,
    Acoustic_Sensor_type = NA,
    Acoustic_.Sensor_idcode = NA,
    Acoustic_Sensor_value = NA,
    PIT_ID = PIT.CODE,
    Radio_ID = NA,
    Radio_Freq = NA,
    Radio_BurstRate = NA,
    External_TagID = NA,
    Mark.Observed = "None",
    Mark.Applied = "None",
    VIE = "None",
    ForkLength..cm. = FL,
    TotalLength..cm. = TL,
    Mass..g. = NA,
    Sex = Sex,
    STRG_Interorbital.mm. = Interorbital,
    STRG_InsideMouth.mm.  = Inside.Mouth,
    STRG_OutsideMouth.mm. = Outside.Mouth,
    Notes = Notes,
    Operator.s. = "JZ,SE")




