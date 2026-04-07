getwd()

# Load Packages ----

#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyr")
#install.packages("lubridate")

library(tidyverse)
library(stringr)
library(sf)




# transfer sheets to encounter form by year ----

## 2006 ---- 

glimpse(um2006)

um2006 <- um2006 %>% 
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")

um2006multac <- um2006 %>%  ## multiple ac tags to add later
  filter(!is.na(`Coded Type`) & !is.na(`Cont. Type`))

glimpse(um2006)

unique(um2006$`US Northing`)
unique(um2006$`US Easting`) 

tagid_cols0610 <- c("PIT ID", "Coded Type", "Cont. Type")

um2006cln <- um2006 %>%
  filter(!(!is.na(`Coded Type`) & !is.na(`Cont. Type`))) %>% 
  mutate(
    `US Northing` = ifelse(is.na(`US Northing`), `DS Northing`, 
                           `US Northing`), ## DS coords when no US coords
    `US Easting` = ifelse(is.na(`US Easting`), `DS Easting`, 
                           `US Easting`), 
    n_ids = rowSums(!is.na(across(all_of(tagid_cols0610)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) | !is.na(`Cont. Type`) ~ "Acoustic",
#      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None"
    )
  ) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    Name = replace_na(Name, ""),
    dna = str_c("Genetic ID: ", `gen. ID`),
    Comments = str_c(Comments, Name, sep = " "),
    Comments = str_squish(Comments),
    Comments = na_if(Comments, ""),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

## Initial Capture 

um2006ic <- um2006cln %>% 
  filter(`Recap (y/n)` == "N")

tfn
names(um2006ic)

um_enc2006ic <- tidsheet_inc(um2006ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                             Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

um_enc2006ic <- um_enc2006ic %>% 
  mutate(Encounter_Disposition = case_when(str_detect(str_to_lower(Notes), "necropsy") ~ "Dead",
                                           TRUE ~ Encounter_Disposition),
         Release_Status = case_when(str_detect(str_to_lower(Notes), "necropsy") ~ "No",
                                    TRUE ~ Release_Status))


## Recapture

um2006rc <- um2006cln %>% 
  filter(`Recap (y/n)` == "Y")

tfn
names(um2006rc)

um_enc2006rc <- tidsheet_rc(um2006rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                            Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000), 
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## 2007 ---- 

glimpse(um2007)

um2007 <- um2007 %>% 
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")

um2007multac <- um2007 %>%  ## multiple ac tags to add later
  filter(!is.na(`Coded Type`) & !is.na(`Cont. Type`))

unique(um2007$`US Northing`)
unique(um2007$`US Easting`) 



um2007cln <- um2007 %>%
  filter(!(!is.na(`Coded Type`) & !is.na(`Cont. Type`))) %>% 
  mutate(
    `US Northing` = ifelse(is.na(`US Northing`), `DS Northing`, 
                           `US Northing`), ## DS coords when no US coords
    `US Easting` = ifelse(is.na(`US Easting`), `DS Easting`, 
                          `US Easting`), 
    n_ids = rowSums(!is.na(across(all_of(tagid_cols0610)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) | !is.na(`Cont. Type`) ~ "Acoustic",
#      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None"
    )
  ) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    Name = replace_na(Name, ""),
    dna = str_c("Genetic ID: ", `gen. ID`),
    Comments = str_c(Comments, Name, sep = " "),
    Comments = str_squish(Comments),
    Comments = na_if(Comments, ""),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

## Initial Capture 

um2007ic <- um2007cln %>% 
  filter(`Recap (y/n)` == "N")

tfn
names(um2007ic)

um_enc2007ic <- tidsheet_inc(um2007ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                             Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recapture

um2007rc <- um2007cln %>% 
  filter(`Recap (y/n)` == "Y")

tfn
names(um2007rc)

um_enc2007rc <- tidsheet_rc(um2007rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                            Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000), 
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2008 ---- 

glimpse(um2008)

um2008 <- um2008 %>% 
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")

um2008multac <- um2008 %>%  ## multiple ac tags to add later
  filter(!is.na(`Coded Type`) & !is.na(`Cont. Type`))

glimpse(um2008)

unique(um2008$`US Northing`)
unique(um2008$`US Easting`) 

unique(um2008$`US LAT`)
unique(um2008$`US LONG`) 

um2008ll<- um2008 %>%
  filter(is.na(`US Northing`)) %>% 
  mutate(
    US_lat_dd = dms_to_dd(`US LAT`),
    US_lon_dd = -dms_to_dd(`US LONG`)
  ) %>%
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  st_transform(26919) %>%
  mutate(
    `US Easting`  = st_coordinates(.)[, 1],
    `US Northing` = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() 

unique(um2008ll$`US Northing`)
unique(um2008ll$`US Easting`)

um2008en <- um2008 %>% 
  filter(! is.na(`US Easting`))

um2008cln <- rbind(um2008en, um2008ll) %>% 
  filter(!(!is.na(`Coded Type`) & !is.na(`Cont. Type`))) %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols0610)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) | !is.na(`Cont. Type`) ~ "Acoustic",
 #     !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None"
    )
  ) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    Name = replace_na(Name, ""),
    dna = str_c("Genetic ID: ", `gen. ID`),
    Comments = str_c(Comments, Name, sep = " "),
    Comments = str_squish(Comments),
    Comments = na_if(Comments, ""),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

## Initial Capture 

um2008ic <- um2008cln %>% 
  filter(`Recap (y/n)` == "N")

tfn
names(um2008ic)

um_enc2008ic <- tidsheet_inc(um2008ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                             Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recapture

um2008rc <- um2008cln %>% 
  filter(`Recap (y/n)` == "Y")

tfn
names(um2008rc)

um_enc2008rc <- tidsheet_rc(um2008rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                            Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000), 
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2009 ---- 

glimpse(um2009)

um2009 <- um2009 %>% 
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")

um2009multac <- um2009 %>%  ## multiple ac tags to add later
  filter(!is.na(`Coded Type`) & !is.na(`Cont. Type`))

glimpse(um2009)

unique(um2009$`US Northing`) ### and then came the dark times
unique(um2009$`US Easting`) 

unique(um2009$`US LAT`)
unique(um2009$`US LONG`) 

um2009cln <- um2009 %>%
  mutate(
    `US LAT` = case_when(`US LAT` == "68.49.00" ~ "44.40.50",
                         `US LAT` == "68.48.59" ~ "44.40.55", ## unswitch lat long
                          .default = `US LAT`),
    `US LONG` = case_when(`US LONG` == "44.40.50" ~ "68.49.00",
                         `US LONG` == "44.40.55" ~ "68.48.59",
                         .default = `US LONG`),
    US_lat_dd = dms_to_dd(`US LAT`),
    US_lon_dd = -dms_to_dd(`US LONG`)) %>%
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  st_transform(26919) %>%
  mutate(
    `US Easting`  = st_coordinates(.)[, 1],
    `US Northing` = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  filter(!(!is.na(`Coded Type`) & !is.na(`Cont. Type`))) %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols0610)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) | !is.na(`Cont. Type`) ~ "Acoustic",
#      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None"
    )
  ) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    Name = replace_na(Name, ""),
    dna = str_c("Genetic ID: ", `gen. ID`),
    Comments = str_c(Comments, Name, sep = " "),
    Comments = str_squish(Comments),
    Comments = na_if(Comments, ""),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2009cln$`US Northing`) 
unique(um2009cln$`US Easting`)

## Initial Capture 

um2009ic <- um2009cln %>% 
  filter(`Recap (y/n)` == "N")

tfn
names(um2009ic)

um_enc2009ic <- tidsheet_inc(um2009ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                             Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recapture

um2009rc <- um2009cln %>% 
  filter(`Recap (y/n)` == "Y")

tfn
names(um2009rc)

um_enc2009rc <- tidsheet_rc(um2009rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                            Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000), 
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2010 ---- 

glimpse(um2010)

um2010 <- um2010 %>% 
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")

glimpse(um2010)

unique(um2010$`US Northing`)
unique(um2010$`US Easting`) 

um2010cln <- um2010 %>%
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols0610)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) | !is.na(`Cont. Type`) ~ "Acoustic",
#      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None"
    )
  ) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = str_c("Genetic ID: ", `gen. ID`),
    `Mass (kg)` = as.numeric(`Mass (kg)`))


## Initial Capture 

um2010ic <- um2010cln %>% 
  filter(`Recap (y/n)` == "N")

tfn
names(um2010ic)

um_enc2010ic <- tidsheet_inc(um2010ic, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                             Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recapture

um2010rc <- um2010cln %>% 
  filter(`Recap (y/n)` == "Y")

tfn
names(um2010rc)

um_enc2010rc <- tidsheet_rc(um2010rc, Species = Species, River = NA_character_, Date = `Pull Date & Time`, Site = Location, 
                            Easting = `US Easting`, Northing = `US Northing`, tagtype = tagtype, tagman = NA_character_,
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000), 
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2011 ----

tagid_cols2011 <- c("PIT ID", "Coded Type")

unique(um2011$`US Northing`)
unique(um2011$`US Easting`)

um2011cln <- um2011 %>%
  mutate(
    `US Northing` = ifelse(is.na(`US Northing`), "4947221", `US Northing`),
    `US Easting` = ifelse(is.na(`US Easting`), ## replace missing coords
                                 "514616", `US Easting`),
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2011)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) ~ "Acoustic",
 #     !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = str_c("Genetic ID: ", `gen. ID`),
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

tagid_cols2012 <- c("PIT ID", "Coded Type")

unique(um2012$`US Northing`)
unique(um2012$`US Easting`) ## tough one

um2012 <- um2012 %>%
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")

um2012ll <- um2012 %>% 
  filter(grepl("\\s", `US Northing`)) %>% 
  mutate(
    US_lat_dd = dms_to_dd_any(`US Northing`),
    US_lon_dd = -dms_to_dd_any(`US Easting`)
  ) %>%
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  st_transform(26919) %>%
  mutate(
    `US Easting`  = st_coordinates(.)[, 1],
    `US Northing` = st_coordinates(.)[, 2],
    `US Easting` = as.character(`US Easting`),
    `US Northing` = as.character(`US Northing`)) %>%
  st_drop_geometry() 

unique(um2012ll$`US Northing`)
unique(um2012ll$`US Easting`)

um2012en <- um2012 %>% 
  filter(!grepl("\\s", `US Northing`) | is.na(`US Northing`)) %>% 
  mutate(`US Northing` = ifelse(is.na(`US Northing`), "4947480", `US Northing`),
         `US Easting` = ifelse(is.na(`US Easting`), ## replace missing coords
                               "514598", `US Easting`))


um2012cln <- bind_rows(um2012ll, um2012en) %>%
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon") %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2011)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) ~ "Acoustic",
 #     !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = str_c("Genetic ID: ", `gen. ID`),
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


tagid_cols2013 <- c("PIT ID", "Code") ## no tag type 

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
      !is.na(Code) ~ "Acoustic",
#      !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    dna = str_c("Genetic ID: ", `gen. ID`),
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

um_enc2013rc <- tidsheet_rc(um2013rc, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                             Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2014 ----

tagid_cols2014 <- c("PIT ID",  "Code") ## No tag type

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
      !is.na(Code) ~ "Acoustic",
   #   !is.na(`Carlin ID`) ~ "Carlin",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = str_c("Genetic ID: ", `gen. ID`),
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

um_enc2014rc <- tidsheet_rc(um2014rc, Species = Species, River = NA_character_, Date = `Pull Date`, Site = Location,
                            Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `Carlin ID`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2015 ---- 

tagid_cols2015 <- c("PIT ID", "Code") ## No tag type excep where it is omfg jpigaehoi'grsiOHGOHi;EHGOW

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
      !is.na(Code) ~ "Acoustic",
  #    !is.na(`External Tag`) ~ "External",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = str_c("Genetic ID: ", `gen. ID`),
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

tagid_cols2016 <- c("PIT ID",  "Code") 

unique(um2016$`US LAT`)
unique(um2016$`US LONG`)  

um2016cln <- um2016 %>%
  mutate(
    US_Lat = ifelse(is.na(`US LAT`), "44.56.515", `US LAT`), ## missing value from milford
    US_Long = ifelse(is.na(`US LONG`), "68.38.678", `US LONG`),
    US_lat_dd = dmm_to_dd(US_Lat),
    US_lon_dd = -dmm_to_dd(US_Long)
  ) %>%
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  st_transform(26919) %>%
  mutate(
    US_Easting  = st_coordinates(.)[, 1],
    US_Northing = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2015)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) ~ "Acoustic",
#      !is.na(`External Tag`) ~ "External",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    `Right Ventral scute count` = if_else(is.na(`Right Ventral scute count`),
                                          "", str_c("Right Ventral scute count: ", 
                                                    `Right Ventral scute count`)),
    `Left Ventral scute count` = if_else(is.na(`Left Ventral scute count`),
                                          "", str_c("Left Ventral scute count: ", 
                                                    `Left Ventral scute count`)),
    Comments = str_c(Comments, `Right Ventral scute count`,
                     `Left Ventral scute count`, sep = " "),
    Comments = str_squish(Comments),
    Comments = na_if(Comments, ""),
    dna = str_c("Genetic ID: ", `gen. ID`),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2016cln$US_Easting)
unique(um2016cln$US_Northing)

glimpse(um2016cln)

## Initial Captures

um2016ic <- um2016cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2016ic)

um_enc2016ic <- tidsheet_inc(um2016ic, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                             Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2016rc <- um2016cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2016rc <- tidsheet_rc(um2016rc, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                            Easting = US_Easting, Northing = US_Northing, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2017 ----


glimpse(um2017) # some dmm some easting northing

tagid_cols2017 <- c("PIT ID", "Code") 

unique(um2017$`US LAT`)
unique(um2017$`US LONG`)  

um2017ll<- um2017 %>%
  filter(is.na(`US Northing`) | Location == "Milford Fish Lift") %>% 
  mutate(
    US_Lat = ifelse(is.na(`US LAT`), "44.56.515", `US LAT`), ## missing value from milford
    US_Long = ifelse(is.na(`US LONG`), "68.38.678", `US LONG`),
    US_lat_dd = dmm_to_dd(US_Lat),
    US_lon_dd = -dmm_to_dd(US_Long)
  ) %>%
  st_as_sf(coords = c("US_lon_dd", "US_lat_dd"), crs = 4326) %>%
  st_transform(26919) %>%
  mutate(
    `US Easting`  = st_coordinates(.)[, 1],
    `US Northing` = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  dplyr::select(-US_Lat, -US_Long)

um2017en <- um2017 %>% 
  filter(! is.na(`US Easting`))

um2017cln <- rbind(um2017en, um2017ll) %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2017)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) ~ "Acoustic",
#      !is.na(`External Tag`) ~ "External",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    Comments = replace_na(Comments, ""),
    `Right Side Scutes` = if_else(is.na(`Right Side Scutes`),
                                          "", str_c("Right Side scute count: ", 
                                                    `Right Side Scutes`)),
    `Left Side Scutes` = if_else(is.na(`Left Side Scutes`),
                                         "", str_c("Left Side scute count: ", 
                                                   `Left Side Scutes`)),
    `Belly Scutes` = if_else(is.na(`Belly Scutes`),
                                 "", str_c("Belly Scutes: ", 
                                           `Belly Scutes`)),
    Comments = str_c(Comments, `Right Side Scutes`, `Left Side Scutes`,
                     `Belly Scutes`, sep = " "),
    Comments = str_squish(Comments),
    Comments = na_if(Comments, ""),
    dna = str_c("Genetic ID: ", `gen. ID`),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2017cln$`US Easting`)
unique(um2017cln$`US Northing`)

glimpse(um2017cln)

## Initial Captures

um2017ic <- um2017cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2017ic)

um_enc2017ic <- tidsheet_inc(um2017ic, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                             Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = Sex, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2017rc <- um2017cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2017rc <- tidsheet_rc(um2017rc, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                            Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = Sex, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)



## 2018 ----


glimpse(um2018) 

tagid_cols2018 <- c("PIT ID",  "Code") 

unique(um2018$`US Northing`)
unique(um2018$`US Easting`)  


um2018cln <- um2018 %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2018)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) ~ "Acoustic",
 #     !is.na(`External Tag`) ~ "External",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = str_c("Genetic ID: ", `gen. ID`),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2018cln$`US Easting`)
unique(um2018cln$`US Northing`)

glimpse(um2018cln)

## Initial Captures

um2018ic <- um2018cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2018ic)

um_enc2018ic <- tidsheet_inc(um2018ic, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                             Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2018rc <- um2018cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2018rc <- tidsheet_rc(um2018rc, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                            Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = `Sex (M/F)`, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2019 ----


glimpse(um2019) 

tagid_cols2019 <- c("PIT ID",  "Code") 

unique(um2019$`US Northing`) ## this is bad
unique(um2019$`US Easting`)  ## how?
unique(um2019$Location)

um_enc_combinedic <- bind_rows(um_enc2006ic, um_enc2007ic, um_enc2008ic, um_enc2009ic,
                           um_enc2010ic, um_enc2011ic, um_enc2012ic, um_enc2013ic, 
                           um_enc2014ic, um_enc2015ic, um_enc2016ic, um_enc2017ic,
                           um_enc2018ic)## need all the loc columns i can get

sapply(encexic, class)

glimpse(um_enc_combinedic)

locations <- um_enc_combinedic %>% 
  group_by(Release_Location) %>% 
  summarise(Easting = mean(Release_Easting),
            Northing = mean(Release_Northing))

glimpse(locations)

um2019cln <- um2019 %>% 
  mutate(
    `US Northing` = case_when(Location == "Bucks ledge" | Location == "Bucks Ledge" ~
                                "4947728",
                              Location == "Milford Fishway" ~ "4976567",
                              Location == "Bartlett Cove" ~ "4950716",
                              Location == "Waterfront" ~ "4958033"),
    `US Easting` = case_when(Location == "Bucks ledge" | Location == "Bucks Ledge" ~
                                "514518.0",
                              Location == "Milford Fishway" ~ "528026.7",
                              Location == "Bartlett Cove" ~ "512195.6",
                              Location == "Waterfront" ~ "517071.0"),
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2018)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) ~ "Acoustic",
 #     !is.na(`External Tag`) ~ "External",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = str_c("Genetic ID: ", `gen. ID`),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

unique(um2019cln$`US Easting`)
unique(um2019cln$`US Northing`)

glimpse(um2019cln)

## Initial Captures

um2019ic <- um2019cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2019ic)

um_enc2019ic <- tidsheet_inc(um2019ic, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                             Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = Sex, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)


## Recaptures

um2019rc <- um2019cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2019rc <- tidsheet_rc(um2019rc, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                            Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = Sex, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

## 2020 ----

glimpse(um2020) 

tagid_cols2020 <- c("PIT ID", "Code") 

unique(um2020$`US Northing`)
unique(um2020$`US Easting`)  ## good things do happen sometimes


um2020cln <- um2020 %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols2020)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) ~ "Acoustic",
   #   !is.na(`External Tag`) ~ "External",
      !is.na(`PIT ID`) ~ "PIT",
      TRUE ~ "None")) %>%
  dplyr::select(-n_ids) %>%
  mutate(
    dna = str_c("Genetic ID: ", `gen. ID`),
    `Mass (kg)` = as.numeric(`Mass (kg)`))

glimpse(um2020cln)

## Initial Captures

um2020ic <- um2020cln %>% 
  filter(`Recap (y/n)` == "N") 

tfn
names(um2020ic)

um_enc2020ic <- tidsheet_inc(um2020ic, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                             Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                             tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                             exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                             Sex = Sex, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                             Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)



## Recaptures

um2020rc <- um2020cln %>% 
  filter(`Recap (y/n)` == "Y")

um_enc2020rc <- tidsheet_rc(um2020rc, Species = Species, River = NA_character_, Date = `Set Date`, Site = Location,
                            Easting = `US Easting`, Northing = `US Northing`, tagman = NA_character_, tagtype = tagtype, 
                            tagmod = `Coded Type`, Serial_N = `Coded Serial #`, taglif = NA_character_, acid = Code,
                            exid = `External Tag`, pitid = `PIT ID`, FL = `FL (cm)`, TL = `TL (cm)`, Mass = (`Mass (kg)` *1000),
                            Sex = Sex, Interorbital = `I-orb. (mm)`, Inside.Mouth = `Inside Mouth (mm)`, 
                            Outside.Mouth = `Outside Mouth (mm)`, Notes = Comments, dna = dna)

# Bind Sheets Together ----

um_enc_combinedic <- bind_rows(um_enc2006ic, um_enc2007ic, um_enc2008ic, um_enc2009ic,
                               um_enc2010ic, um_enc2011ic, um_enc2012ic, um_enc2013ic, 
                               um_enc2014ic, um_enc2015ic, um_enc2016ic, um_enc2017ic,
                               um_enc2018ic, um_enc2019ic, um_enc2020ic) 


glimpse(um_enc_combinedic)

locations <- um_enc_combinedic %>% 
  group_by(Release_Location) %>% 
  summarise(Easting = mean(Release_Easting),
            Northing = mean(Release_Northing))

um_enc_combinedrc <- bind_rows(um_enc2006rc, um_enc2007rc, um_enc2008rc, um_enc2009rc,
                               um_enc2010rc, um_enc2011rc, um_enc2012rc, um_enc2013rc, 
                               um_enc2014rc, um_enc2015rc, um_enc2016rc, um_enc2017rc,
                               um_enc2018rc, um_enc2019rc, um_enc2020rc) 


glimpse(um_enc_combinedrc)

#write.csv(um_enc_combinedic,
#          file.path(gdrive_path, "UEF_AST_SNS_UMaineHistoric_IC_Working.csv"),
#          row.names = FALSE, na = "")

#write.csv(um_enc_combinedrc,
#          file.path(gdrive_path, "UEF_AST_SNS_UMaineHistoric_RCNT_Working.csv"),
#          row.names = FALSE, na = "")


# Tag info ----

## PIT tags of acoustic tagged fish ----

tid_ac_fsh <- rbind(tid_ast_ac_fsh, tid_sns_ac_fsh)
glimpse(tid_ac_fsh)

tid_fsh <- rbind(tid_ast_fsh, tid_sns_fsh)
glimpse(tid_fsh)

tid_ac_tag <- rbind(tid_ast_ac_tag, tid_sns_ac_tag)
glimpse(tid_ac_tag)

ac_ic <- um_enc_combinedic %>% 
  filter(
    !is.na(Acoustic_ID)
  )
glimpse(ac_ic)

ac_rc <- um_enc_combinedrc %>% 
  filter(
    !is.na(Acoustic_ID) | !is.na(Observed_Acoustic_ID)
  )
glimpse(ac_rc)

miss_ac_ic <- ac_ic %>% 
  filter(!PIT_ID %in% tid_ac_tag$PITIDNo)
glimpse(miss_ac_ic)

miss_ac_rc <- ac_rc %>% 
  filter(!PIT_ID %in% tid_ac_tag$PITIDNo)
glimpse(miss_ac_rc)

pit_tid_miss_ac <- tid_fsh %>% 
  filter(TagID %in% miss_ac_ic$PIT_ID |
           TagID %in% miss_ac_rc$PIT_ID
  )

tid_miss_ac <- tid_fsh %>% 
  filter(FishID %in% pit_tid_miss_ac$FishID
         )
glimpse(tid_miss_ac)

length(unique(tid_miss_ac$FishID))

mult_ac <- rbind(um2006multac, um2007multac, um2008multac, um2009multac)


pit_tid_mult_ac <- tid_fsh %>% 
  filter(TagID %in% mult_ac$`PIT ID`)

tid_mult_ac <- tid_fsh %>% 
  filter(FishID %in% pit_tid_mult_ac$FishID)

write.csv(miss_ac_ic, file.path(gdrive_path, "output/miss_ac_ic.csv"),
          row.names = FALSE, na = "")

write.csv(miss_ac_rc, file.path(gdrive_path, "output/miss_ac_rc.csv"),
          row.names = FALSE, na = "")

write.csv(tid_miss_ac, file.path(gdrive_path, "output/tid_miss_ac.csv"),
          row.names = FALSE, na = "")

write.csv(mult_ac, file.path(gdrive_path, 
                             "output/legacy.mulitple.acoustic.tag.csv"), 
          row.names = FALSE, na = "")

write.csv(tid_mult_ac, file.path(gdrive_path, 
                             "output/tidbits.mulitple.acoustic.tag.csv"), 
          row.names = FALSE, na = "")


## Get tag codes ----

icIDs <- um_enc_combinedic %>% 
  filter(! is.na(Acoustic_ID)) %>% 
  mutate(y = year(Encounter_Timestamp)) %>% 
  dplyr::select(Acoustic_ID, TagModel, TagSerialNumber, y)  
head(icIDs)

rcIDs <- um_enc_combinedrc %>% 
  filter(! is.na(Acoustic_ID)) %>% 
  mutate(y = year(Encounter_Timestamp)) %>% 
  dplyr::select(Acoustic_ID, TagModel, TagSerialNumber, y) 
head(rcIDs)

IDs <- rbind(icIDs, rcIDs) %>% 
  unique()


#write.csv(IDs,
#         file.path(gdrive_path, "output/tagIDs.csv"),
#          row.names = FALSE, na = "")





## Add in fill ids ----


## Initial Capture

glimpse(um_enc_combinedic)

f_id <- read.csv(file.path(gdrive_path, "data/full_id.csv"), na = "")

ac_id_ic <- um_enc_combinedic %>% 
  mutate(Acoustic_numeric = suppressWarnings(as.integer(Acoustic_ID))) %>% 
  filter(
    is.na(Acoustic_ID) | Acoustic_numeric %in% f_id$Partial_ID)%>% 
  left_join(.,f_id, by = c("Acoustic_numeric" = "Partial_ID"), suffix = c("", "_f"),
            relationship = "many-to-one") %>% 
  mutate(
         EstTagLife = EstTagLife_f,
         Acoustic_ID = AcousticID,
         Acoustic_Sensor_type = SensorType)
ac_id_icf <- ac_id_ic[,1:45]
  


problem_ids_ic <- um_enc_combinedic %>% 
  mutate(Acoustic_numeric = suppressWarnings(as.integer(Acoustic_ID))) %>% 
  filter(!is.na(Acoustic_ID), !Acoustic_numeric %in% f_id$Partial_ID) %>% 
  dplyr::select(- Acoustic_numeric)

## Recapture

ac_id_rc <- um_enc_combinedrc %>% 
  mutate(Acoustic_numeric = suppressWarnings(as.integer(Acoustic_ID))) %>% 
  filter(
    is.na(Acoustic_ID) | Acoustic_numeric %in% f_id$Partial_ID) %>% 
  left_join(f_id, by = c("Acoustic_numeric" = "Partial_ID"), suffix = c("", "_f"),
            relationship = "many-to-one") %>% 
  mutate(
    EstTagLife = EstTagLife_f,
    Acoustic_ID = AcousticID,
    Acoustic_Sensor_type = SensorType)
ac_id_rcf <- ac_id_rc[,1:48]


problem_ids_rc <- um_enc_combinedrc %>% 
  mutate(Acoustic_numeric = suppressWarnings(as.integer(Acoustic_ID))) %>% 
  filter(!is.na(Acoustic_ID), !Acoustic_numeric %in% f_id$Partial_ID) %>% 
  dplyr::select(- Acoustic_numeric)


# QAQC ----

## PIT ----

bad_pitic <- ac_id_icf %>% filter(str_detect(PIT_ID, "\\*"))
bad_pitrc <- ac_id_rcf %>% filter(str_detect(PIT_ID, "\\*"))

pitclnic <- ac_id_icf %>% filter(!str_detect(PIT_ID, "\\*"))
pitclnrc <- ac_id_rcf %>% filter(!str_detect(PIT_ID, "\\*"))


## System ----

system_ic <- pitclnic %>% 
  mutate(System = ifelse(Encounter_Easting < 496000, "Kennebec", "Penobscot"),
         Species = case_when(Species == "AST" ~ "Atlantic Sturgeon",
                             Species == "SNS" ~ "Shortnose Sturgeon",
                             .default = Species))

system_rc <- pitclnrc %>% 
  mutate(System = ifelse(Encounter_Easting < 496000, "Kennebec", "Penobscot"),
         Species = case_when(Species == "AST" ~ "Atlantic Sturgeon",
                             Species == "SNS" ~ "Shortnose Sturgeon",
                             .default = Species))

## EVENT ----

system_rcnt <- system_rc %>% 
  mutate(Event = if_else( !is.na(Acoustic_ID) & 
                             !(Acoustic_ID %in% unique(system_ic$Acoustic_ID)), 
                           "New Tag", Event ),
          Observed_Acoustic_ID = ifelse(Event == "Recapture", Acoustic_ID, NA_character_),
          Observed_PIT_ID = PIT_ID)


# Compare to Tidbits ----

tidhst <- rbind(tidASThst, tidSNShst) %>% 
  filter(Event == "Initial Capture/Release (PIT tag)" | 
           Event == "Initial Capture/Release (Acoustic tag)" | 
           Event == "Recapture (PIT tag)" | 
           Event == "Recapture (Acoustic tag)" | 
           Event == "Initial Capture/Release (Visual tag)" | 
           Event == "Recapture (Visual tag)") %>% 
  mutate(Period = ymd_hms(Period))

tidPIT <- tidhst %>% 
  filter(tagtype == 'PIT')

system_pit <- c(system_ic$PIT_ID, system_rcnt$PIT_ID) %>% 
  unique()

tidpit <- tidPIT$TagId %>% 
  unique()

system_pit <- data.frame(PIT_ID = unique(c(system_ic$PIT_ID, system_rcnt$PIT_ID)))
tidpit <- data.frame(PIT_ID = unique(tidPIT$TagId))

new_pit <- anti_join(system_pit, tidpit, by = "PIT_ID")


new_pit <- setdiff(system_pit$PIT_ID, tidpit$PIT_ID)


new_pitic <- system_ic %>% 
  filter(PIT_ID %in% new_pit)

new_pitrcnt <- system_rcnt %>% 
  filter(PIT_ID %in% new_pit)

system_rcnt %>% 
  summarise(n_distinct(PIT_ID))

tidPIT %>% 
  summarise(n_distinct(TagId))

tidevent <- tidhst %>% 
  mutate(date = format(Period, "%Y-%m-%d"))

glimpse(tidevent)
glimpse(system_ic)

by <- join_by(date, PIT_ID == TagId)

unq_umaineic <- system_ic %>% 
  mutate(date = format(Encounter_Timestamp, "%Y-%m-%d")) %>% 
  anti_join(.,
    tidevent,
    by)



unq_umainercnt <- system_rcnt %>% 
  mutate(date = format(Encounter_Timestamp, "%Y-%m-%d")) %>% 
  anti_join(.,
            tidevent,
            by)

match_tidic <- system_ic %>% 
  mutate(date = format(Encounter_Timestamp, "%Y-%m-%d")) %>%
  inner_join( tidevent, by)

match_tidrc <- system_rcnt %>% 
  mutate(date = format(Encounter_Timestamp, "%Y-%m-%d")) %>%
  inner_join( tidevent, by)

ic_exids <- match_tidic %>% 
  filter(Species.x == "Shortnose Sturgeon") %>% 
  dplyr::select(External_TagID)


rc_exids <- match_tidrc %>% 
  filter(Species.x == "Shortnose Sturgeon") %>% 
  dplyr::select(External_TagID)

exids <- rbind(ic_exids, rc_exids) %>% 
  filter(! is.na(External_TagID)) %>% 
  summarise(n_distinct(External_TagID))


write.csv(new_pitic, file.path(gdrive_path, "output/newUEFic.csv"),
                    row.names = FALSE, na = "")

write.csv(new_pitrcnt, file.path(gdrive_path, "output/newUEFrcnt.csv"),
          row.names = FALSE, na = "")


glimpse(system_ic)

  










