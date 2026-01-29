# Packages ----
#install.packages("readr")
#install.packages("readxl") 

library(readr)
library(readxl)

# Load Data ----

## UMaine data

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

## Read in example encounter forms

encexic <- read.csv("QAQC/IntoTidBits/Encounter Forms/2025/output/UEF_IC_AST_SNS_SE.csv")
encexrcnt <- read.csv("QAQC/IntoTidBits/Encounter Forms/2025/output/UEF_RCNT_AST_SNS_SE.csv")


## Read in example history

tidSNShst <- read.csv("QAQC/IntoTidBits/UMaineHistoricData/data/tid_SNShist_allyrs.csv")
tidASThst <- read.csv("QAQC/IntoTidBits/UMaineHistoricData/data/tid_ASThist_allyrs.csv")




# FUNCTIONS ----

tidsheet_inc <- function(x, Species, River, Date, Site, Easting, Northing,
                         tagtype, tagman, tagmod, Serial_N, taglif, acid, exid, pitid,
                         FL, TL, Mass, Sex, Interorbital, Inside.Mouth, Outside.Mouth, Notes) {
  desired_column_order <- c(
    "Event", "Species", "Rearing_Origin", "Stage", "System",
    "Encounter_Time_zone", "Encounter_Timestamp", "Encounter_Location",
    "Encounter_Easting", "Encounter_Northing", "Encounter_UTMZone",
    "Encounter_Disposition", "Release_Status", "Release_Timezone",
    "Release_Timestamp", "Release_Location", "Release_Easting",
    "Release_Northing", "TagType", "TagManufacturer", "TagModel",
    "TagSerialNumber", "EstTagLife", "Acoustic_ID", "Acoustic_Sensor_type",
    "Acoustic_.Sensor_idcode", "Acoustic_Sensor_value", "PIT_ID",
    "Radio_ID", "Radio_Freq", "Radio_BurstRate", "External_TagID",
    "Mark.Observed", "Mark.Applied", "VIE", "ForkLength..cm.",
    "TotalLength..cm.", "Mass..g.", "Sex", "STRG_Interorbital.mm.",
    "STRG_InsideMouth.mm.", "STRG_OutsideMouth.mm.", "Notes",
    "Operator.s."
  )
  
  x %>%
    mutate(
      Event = "Initial Capture",
      Species = {{ Species }},
      Rearing_Origin = "Wild",
      Stage = NA,
      System = {{ River }},
      Encounter_Time_zone = "Eastern",
      Encounter_Timestamp = {{ Date }},
      Encounter_Location = {{ Site }},
      Encounter_Easting  = {{ Easting }},
      Encounter_Northing = {{ Northing }},
      Encounter_UTMZone  = "19",
      Encounter_Disposition = "Live",
      Release_Status = "Yes",
      Release_Timezone = "Eastern",
      Release_Timestamp = {{ Date }},
      Release_Location = {{ Site }},
      Release_Easting = {{ Easting }},
      Release_Northing = {{ Northing }},
      TagType = {{ tagtype }},
      TagManufacturer = {{ tagman }},
      TagModel = {{ tagmod }},
      TagSerialNumber = {{ Serial_N }},
      EstTagLife = {{ taglif }},
      Acoustic_ID = {{ acid }},
      Acoustic_Sensor_type = NA,
      Acoustic_.Sensor_idcode = NA,
      Acoustic_Sensor_value = NA,
      PIT_ID = {{ pitid }},
      Radio_ID = NA,
      Radio_Freq = NA,
      Radio_BurstRate = NA,
      External_TagID = {{ exid }},
      Mark.Observed = "None",
      Mark.Applied = "None",
      VIE = "None",
      ForkLength..cm. = {{ FL }},
      TotalLength..cm. = {{ TL }},
      Mass..g. = {{ Mass }},
      Sex = {{ Sex }},
      STRG_Interorbital.mm. = {{ Interorbital }},
      STRG_InsideMouth.mm.  = {{ Inside.Mouth }},
      STRG_OutsideMouth.mm. = {{ Outside.Mouth }},
      Notes = {{ Notes }},
      Operator.s. = NA,
      .keep = "none"
    ) %>%
    select(all_of(desired_column_order))
}

tfn <- c("Species", "River", "Date", "Site", "Easting", "Northing",
         "tagtype", "tagman", "tagmod", "Serial_N", "taglif", "acid", "exid", "pitid",
         "FL", "TL", "Mass", "Sex", 'Interorbital', "Inside.Mouth", "Outside.Mouth", "Notes")

tidsheet_rc <- function(x, Species, River, Date, Site, Easting, Northing,
                        tagtype, tagman, tagmod, Serial_N, taglif, acid, exid, pitid,
                        FL, TL, Mass, Sex, Interorbital, Inside.Mouth, Outside.Mouth, Notes) {
  
  desired_column_order <- c(
    "Event", "Species", "Rearing_Origin", "Stage", "System",
    "Encounter_Time_zone", "Encounter_Timestamp", "Encounter_Location",
    "Encounter_Easting", "Encounter_Northing", "Encounter_UTMZone",
    "Encounter_Disposition", "Release_Status", "Release_Timezone",
    "Release_Timestamp", "Release_Location", "Release_Easting",
    "Release_Northing",
    "Observed_Acoustic_ID", "Observed_PIT_ID", "Observed_Radio_ID",
    "Observed_External_TagID",
    "TagType", "TagManufacturer", "TagModel", "TagSerialNumber",
    "EstTagLife", "Acoustic_ID", "Acoustic_Sensor_type",
    "Acoustic_.Sensor_idcode", "Acoustic_Sensor_value",
    "PIT_ID", "Radio_ID", "Radio_Freq", "External_TagID",
    "Clip1", "Clip2", "VIE",
    "ForkLength..cm.", "TotalLength..cm.", "Mass..g.", "Sex",
    "STRG_Interorbital.mm.", "STRG_InsideMouth.mm.",
    "STRG_OutsideMouth.mm.", "Notes", "Operator.s."
  )
  
  x %>%
    mutate(
      Event = "Recapture",
      Species = {{ Species }},
      Rearing_Origin = "Wild",
      Stage = NA,
      System = NA,
      Encounter_Time_zone = "Eastern",
      Encounter_Timestamp = {{ Date }},
      Encounter_Location = {{ Site }},
      Encounter_Easting  = {{ Easting }},
      Encounter_Northing = {{ Northing }},
      Encounter_UTMZone  = "19",
      Encounter_Disposition = "Live",
      Release_Status = "Yes",
      Release_Timezone = "Eastern",
      Release_Timestamp = {{ Date }},
      Release_Location = {{ Site }},
      Release_Easting = {{ Easting }},
      Release_Northing = {{ Northing }},
      Observed_Acoustic_ID = NA,
      Observed_PIT_ID = NA,
      Observed_Radio_ID = NA,
      Observed_External_TagID = NA,
      TagType = {{ tagtype }},
      TagManufacturer = {{ tagman }},
      TagModel = {{ tagmod }},
      TagSerialNumber = {{ Serial_N }},
      EstTagLife = {{ taglif }},
      Acoustic_ID = {{ acid }},
      Acoustic_Sensor_type = NA,
      Acoustic_.Sensor_idcode = NA,
      Acoustic_Sensor_value = NA,
      PIT_ID = {{ pitid }},
      Radio_ID = NA,
      Radio_Freq = NA,
      External_TagID = {{ exid }},
      Clip1 = NA,
      Clip2 = NA,
      VIE = "None",
      ForkLength..cm. = {{ FL }},
      TotalLength..cm. = {{ TL }},
      Mass..g. = {{ Mass }},
      Sex = {{ Sex }},
      STRG_Interorbital.mm. = {{ Interorbital }},
      STRG_InsideMouth.mm.  = {{ Inside.Mouth }},
      STRG_OutsideMouth.mm. = {{ Outside.Mouth }},
      Notes = {{ Notes }},
      Operator.s. = NA,
      .keep = "none") %>%
    select(all_of(desired_column_order))
}