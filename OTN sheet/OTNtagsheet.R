### Script for assembling OTN Tag sheet with UMAINE and Tidbits Data

# Load ----

## Packages ----

## Path ----

#gdrive_base <- "C:/Users/sande/My Drive (sander.elliott@maine.edu)"
gdrive_base <- "C:/Users/sander.elliott/My Drive (sander.elliott@maine.edu)"
gdrive_path <- file.path(gdrive_base, "Code", "Sturgeon_TidBits", "QAQC", "OTN sheet")

## Data ----
getwd()

tid.ast <- read.csv("OTN sheet/data/TidAST.csv")
tid.sns <- read.csv("OTN sheet/data/TidSNS.csv")

gayle.otn <- read.csv("OTN sheet/data/GayleOTN.csv")

umainetags <- 