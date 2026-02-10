getwd()

# Load Packages ----

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyr")
#install.packages("lubridate")

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(sf)

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

# transfer sheets to encounter form by year ----

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
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    Name = replace_na(Name, ""),
    dna = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
    Comments = str_c(Comments, Name, sep = " "),
    Comments = str_squish(Comments),
    Comments = na_if(Comments, ""),
    `Mass (kg)` = as.numeric(`Mass (kg)`)
  )


bin0610ic <- bin0610cln %>% 
  filter(`Recap (y/n)` == "N")

tfn
names(bin0610ic)

um_enc0610ic <- tidsheet_inc(bin0610ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                          Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                          tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                          exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                          Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                          Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

um_enc0610ic <- um_enc0610ic %>% 
  mutate(Encounter_Disposition = case_when(str_detect(str_to_lower(Notes), "necropsy") ~ "Dead",
      TRUE ~ Encounter_Disposition),
      Release_Status = case_when(str_detect(str_to_lower(Notes), "necropsy") ~ "No",
                                        TRUE ~ Release_Status))


## NEED TO DO RIVER AND ACOUSTIC TAG INFORMATION DOWN THE ROAD ##

bin0610rc <- bin0610cln %>% 
  filter(`Recap (y/n)` == "Y")

tfn
names(bin0610rc)

um_enc0610rc <- tidsheet_rc(bin0610rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                         Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                         tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                         exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000), 
                         Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                         Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)



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
  dplyr::select(-n_ids) %>%
  mutate(
    dna = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
    `Mass (kg)` = as.numeric(`Mass (kg)`)
  )

## Initial Captures

um2011ic <- um2011cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2011ic)

um_enc2011ic <- tidsheet_inc(um2011ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location,
                          Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                          tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                          exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                          Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                          Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2011rc <- um2011cln %>% 
  filter(`Recap (y/n)` == "Y") 

tfn
names(um2011rc)

um_enc2011rc <- tidsheet_rc(um2011rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location,
                         Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                         tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                         exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                         Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                         Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)



## 2012 ---- 

tagid_cols2012 <- c("PIT ID", "Carlin ID", "Coded Type")

um2012cln <- um2012 %>%
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon") %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2011)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(`Coded Type`) ~ "Acoustic",
      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
    `Mass (kg)` = as.numeric(`Mass (kg)`))


## Initial Captures 

um2012ic <- um2012cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2012ic)

um_enc2012ic <- tidsheet_inc(um2012ic, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                             Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)




## Recaptures 

um2012rc <- um2012cln %>% 
  filter(`Recap (y/n)` == "Y")

tfn
names(um2012rc)

um_enc2012rc <- tidsheet_rc(um2012rc, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                            Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## 2013 ----


tagid_cols2013 <- c("PIT ID", "Carlin ID", "Code") ## no tag type 

um2013cln <- um2013 %>%
  mutate(
    US_LAT  = ifelse(nchar(`US LAT`)  > 8, substr(`US LAT`,  1, 8), `US LAT`),
    US_LONG = ifelse(nchar(`US LONG`) > 8, substr(`US LONG`, 1, 8), `US LONG`),
    US_lat_dd = dms_to_dd(US_LAT),
    US_lon_dd = -dms_to_dd(US_LONG)   # negative for western hemisphere
  ) %>%
  # convert to sf using lon, lat
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  # transform to UTM zone 19N (NAD83)
  st_transform(26919) %>%
  # extract UTM coordinates into new columns
  mutate(
    US_Easting  = st_coordinates(.)[, 1],
    US_Northing = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2013)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(`Coded Type`) ~ "Acoustic",
      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    dna = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2013cln$US_Easting)
unique(um2013cln$US_Northing)

glimpse(um2013cln)

## Initial Captures 

um2013ic <- um2013cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2013ic)

um_enc2013ic <- tidsheet_inc(um2013ic, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                             Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2013rc <- um2013cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2013rc <- tidsheet_rc(um2013ic, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                             Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2014 ----

tagid_cols2014 <- c("PIT ID", "Carlin ID", "Code") ## No tag type

glimpse(um2014)

um2014cln <- um2014 %>%
  mutate(
    US_LAT  = ifelse(is.na(`US LAT`), "44.40.32", `US LAT`), ## replacing NAs with nearby site. Best option i can think of
    US_LONG = ifelse(is.na(`US LONG`), "68.48.42", `US LONG`),
    US_lat_dd = dms_to_dd(US_LAT),
    US_lon_dd = -dms_to_dd(US_LONG)   # negative for western hemisphere
  ) %>%
  # convert to sf using lon, lat
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  # transform to UTM zone 19N (NAD83)
  st_transform(26919) %>%
  # extract UTM coordinates into new columns
  mutate(
    US_Easting  = st_coordinates(.)[, 1],
    US_Northing = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2013)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(`Coded Type`) ~ "Acoustic",
      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2014cln$US_Easting)
unique(um2014cln$US_Northing)

glimpse(um2014cln)

## Initial Captures 

um2014ic <- um2014cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2014ic)

um_enc2014ic <- tidsheet_inc(um2014ic, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                             Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2014rc <- um2014cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2014rc <- tidsheet_rc(um2014ic, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                            Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2015 ---- 

tagid_cols2015 <- c("PIT ID", "External Tag", "Code") ## No tag type excep where it is omfg jpigaehoi'grsiOHGOHi;EHGOW

glimpse(um2015)

unique(um2015$`US LAT`)
unique(um2015$`US LONG`) # cry a lot

um2015cln <- um2015 %>%
  mutate(
    US_lat_dd = dms_to_dd_any(`US LAT`),
    US_lon_dd = -dms_to_dd_any(`US LONG`)
  ) %>%
  # convert to sf using lon, lat
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  # transform to UTM zone 19N (NAD83)
  st_transform(26919) %>%
  # extract UTM coordinates into new columns
  mutate(
    US_Easting  = st_coordinates(.)[, 1],
    US_Northing = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2015)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(`Coded Type`) ~ "Acoustic",
      !is.na(`External Tag`) ~ "External",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2015cln$US_Easting)
unique(um2015cln$US_Northing)

glimpse(um2015cln)

## Initial Captures

um2015ic <- um2015cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2015ic)

um_enc2015ic <- tidsheet_inc(um2015ic, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                             Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2015rc <- um2015cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2015rc <- tidsheet_rc(um2015rc, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                            Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2016 ----

## not looking at new tag for external tags 

glimpse(um2016)

tagid_cols2016 <- c("PIT ID", "External Tag", "Code") 

unique(um2016$`US LAT`)
unique(um2016$`US LONG`)  

um2016cln <- um2016 %>%
  mutate(
    US_Lat = ifelse(is.na(`US LAT`), "44.56.515", `US LAT`),
    US_Long = ifelse(is.na(`US LONG`), "68.38.678", `US LONG`),
    US_lat_dd = dmm_to_dd(US_Lat),
    US_lon_dd = -dmm_to_dd_(US_Long)
  ) %>%
  # convert to sf using lon, lat
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  # transform to UTM zone 19N (NAD83)
  st_transform(26919) %>%
  # extract UTM coordinates into new columns
  mutate(
    US_Easting  = st_coordinates(.)[, 1],
    US_Northing = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2015)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(`Coded Type`) ~ "Acoustic",
      !is.na(`External Tag`) ~ "External",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2016cln$US_Easting)
unique(um2016cln$US_Northing)

glimpse(um2016cln)

## Initial Captures

um2015ic <- um2015cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2015ic)

um_enc2015ic <- tidsheet_inc(um2015ic, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                             Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2015rc <- um2015cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2015rc <- tidsheet_rc(um2015rc, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                            Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

# Bind Sheets Together ----

um_enc_combinedic <- rbind(um_enc0610ic, um_enc2011ic, um_enc2012ic)


# Compare to Tidbits ----

tidhst <- rbind(tidASThst, tidSNShst) %>% 
  filter(Event == "Initial Capture/Release (PIT tag)" | 
           Event == "Initial Capture/Release (PIT tag)" | 
           Event == "Recapture (PIT tag)" | 
           Event == "Recapture (Acoustic tag)" | 
           Event == "Initial Capture/Release (Visual tag)" | 
           Event == "Recapture (Visual tag)")

tid0612 <- tidhst %>% 
  filter(Event == "Initial Capture/Release (PIT tag)" | 
           Event == "Initial Capture/Release (PIT tag)" | 
           Event == "Recapture (PIT tag)" | 
           Event == "Recapture (Acoustic tag)" | 
           Event == "Initial Capture/Release (Visual tag)" | 
           Event == "Recapture (Visual tag)") %>% 
  mutate(Period = as.Date(Period), 
         Year = year(Period)) %>% 
  filter(Year < 2013) %>% 
  dplyr::select(-Year)






##### JUNK
mutate(
  Comments = replace_na(Comments, ""),
  `gen. ID` = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
  Comments = str_c(Comments, `gen. ID`, sep = " "),
  Comments = str_squish(Comments),
  Comments = na_if(Comments, ""),
  `Mass (kg)` = as.numeric(`Mass (kg)`))



