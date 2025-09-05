library(tidyverse)   
library(lubridate)
library(dplyr)

# read the dataset
df_raw <- read.csv("data/raw/massa_CAMELS_CH_obs_based_2161.csv", stringsAsFactors = FALSE)

# show basic information about the dataset
cat("Dataset dimensions:", dim(df_raw), "\n")
cat("Column names:\n")
print(names(df_raw))

# first 10 rows
head(df_raw, 10)

# rename columns 
df_massa <- df_raw %>%
  # rename(new_name = old_name)
  rename(
    date_yyyymmdd = date,
    discharge_vol_m3s = discharge_vol.m3.s.,        # m3/s
    discharge_specific_mm = discharge_spec.mm.d.,   # mm/d  
    waterlevel_m = waterlevel.m.,                   # m above sea level
    precipitation_mm = precipitation.mm.d.,         # mm/d
    temp_min = temperature_min.degC.,               # Celsius
    temp_mean = temperature_mean.degC.,             # Celsius
    temp_max = temperature_max.degC.,               # Celsius
    rel_sunshine_duration = rel_sun_dur...,         # % 
    snow_water_equiv_mm = swe.mm.                   # mm (snow water equivalent)
  ) %>%
  
  # convert date column
  mutate(
    date_yyyymmdd = as.Date(date_yyyymmdd, format = "%Y-%m-%d")
  )

# check data types (date_yyyymmdd as date, all others as numeric)
str(df_massa)

# check for  Missing Values (NaN Treatment)

df_massa %>%
  summarise_all(~sum(is.na(.))) 

# only snow_water_equiv_mm has NAs
summary(df_massa$snow_water_equiv_mm)

# find first non-missing value
first_valid <- which(!is.na(df_massa$snow_water_equiv_mm))[1]
df_massa$date_yyyymmdd[first_valid]

# count missing vs available
sum(is.na(df_massa$snow_water_equiv_mm))
sum(!is.na(df_massa$snow_water_equiv_mm))


# impute the NaN`s with 0 and add new column for imputed Yes=1, No=0 to flag them
df_massa$imputed_snow_water_equiv <- ifelse(is.na(df_massa$snow_water_equiv_mm), 1, 0)
df_massa$imputed_snow_water_equiv
tail(df_massa$imputed_snow_water_equiv)
colnames(df_massa)
df_massa$snow_water_equiv_mm[is.na(df_massa$snow_water_equiv_mm)] <- 0

df_massa %>%
  summarise_all(~sum(is.na(.)))

# rearrange columns
df_massa <- df_massa %>%
  select(
    # Date first
    date_yyyymmdd,
    # other variables
    waterlevel_m, discharge_vol_m3s, discharge_specific_mm,  
    precipitation_mm, temp_min, temp_mean, temp_max, 
    rel_sunshine_duration,
    # Snow water equivalent and its imputation flag
    snow_water_equiv_mm, imputed_snow_water_equiv
  )

colnames(df_massa)
str(df_massa)

write.csv(df_massa, "data/processed/massa_cleaned.csv", row.names = FALSE)
