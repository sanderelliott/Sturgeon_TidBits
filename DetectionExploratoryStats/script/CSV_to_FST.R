#install.packages("fst")   # once
library(fst)

# Change the csv file to fst due to large file size. 
df <- read.table(
  "/Volumes/T7 Shield/2025_ATS_detections_fish_histories_all_utf8.csv",
  sep = ",",
  header = TRUE,
  stringsAsFactors = FALSE
)
dim(df)
names(df)
names(df)[1:10]
str(df$Period)   # or whatever your time column is

str(df$Period)
head(df$Period, 10)

df <- df %>%
  mutate(Period = ymd_hms(Period, tz = "America/New_York"))

# optional drop
df <- df %>% select(-any_of(c("SensorType","avgSensorValue", "avgCompute", "Codespace", "AltFishID")))

write_fst(df, "/Volumes/T7 Shield/2025_ATS_Radio_FST.fst")
rm(df); gc()