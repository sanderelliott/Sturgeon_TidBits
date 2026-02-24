# JUNK ----

mutate(
  Comments = replace_na(Comments, ""),
  `gen. ID` = if_else(is.na(`gen. ID`), "", str_c("Genetic ID: ", `gen. ID`) ),
  Comments = str_c(Comments, `gen. ID`, sep = " "),
  Comments = str_squish(Comments),
  Comments = na_if(Comments, ""),
  `Mass (kg)` = as.numeric(`Mass (kg)`))

## 2006 -2010
tagid_cols0610 <- c("PIT ID", "Carlin ID", "Coded Type", "Cont. Type")

bin0610 <- bin0610 %>% 
  filter(Species == "Atlantic Sturgeon" | Species == "Shortnose Sturgeon")

## Pull out multiple acoustic tags to deal with later

bin0610multac <- bin0610 %>% 
  filter(!is.na(`Coded Type`) & !is.na(`Cont. Type`))

## Initial capture dataset 

unique(um2016$`US LAT`)
unique(um2016$`US LONG`) 

bin0610cln <- bin0610 %>%
  filter(!(!is.na(`Coded Type`) & !is.na(`Cont. Type`))) %>% 
  mutate(
    n_ids = rowSums(!is.na(across(all_of(tagid_cols0610)))),
    tagtype = case_when(
      n_ids > 1 ~ "Multiple",
      !is.na(Code) | !is.na(`Cont. Type`) ~ "Acoustic",
      !is.na(`Carlin ID`) ~ "Carlin",
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


## Recapture

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



