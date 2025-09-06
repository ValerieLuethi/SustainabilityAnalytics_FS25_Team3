library(tidyverse)   
library(lubridate)
library(dplyr)
library(tseries)

# read the dataset
df_raw <- read.csv("data/raw/brig_CAMELS_CH_obs_based_2346.csv", stringsAsFactors = FALSE)

# show basic information about the dataset
cat("Dataset dimensions:", dim(df_raw), "\n")
cat("Column names:\n")
print(names(df_raw))

# first 10 rows
head(df_raw, 10)
range(min(df_raw$date), max(df_raw$date))

# rename columns 
df_rhone <- df_raw %>%
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
str(df_rhone)

# check for  Missing Values (NaN Treatment)
df_rhone %>%
  summarise_all(~sum(is.na(.))) 

# only snow_water_equiv_mm has NAs (6453)
summary(df_rhone$snow_water_equiv_mm)

# find first non-missing value: 1998-09-02
first_valid <- which(!is.na(df_rhone$snow_water_equiv_mm))[1]
df_rhone$date_yyyymmdd[first_valid]

# count missing vs available
sum(is.na(df_rhone$snow_water_equiv_mm))
sum(!is.na(df_rhone$snow_water_equiv_mm))


# impute the NaN`s with 0 and add new column for imputed Yes=1, No=0 to flag them
df_rhone$imputed_snow_water_equiv <- ifelse(is.na(df_rhone$snow_water_equiv_mm), 1, 0)
df_rhone$imputed_snow_water_equiv
tail(df_rhone$imputed_snow_water_equiv)
colnames(df_rhone)
df_rhone$snow_water_equiv_mm[is.na(df_rhone$snow_water_equiv_mm)] <- 0

df_rhone %>%
  summarise_all(~sum(is.na(.)))

# rearrange columns
df_rhone <- df_rhone %>%
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

colnames(df_rhone)
str(df_rhone)

write.csv(df_rhone, "data/processed/rhone_cleaned.csv", row.names = FALSE)
plot(df_rhone$date_yyyymmdd, df_rhone$waterlevel_m)

# 30 highest water levels
head(df_rhone[order(-df_rhone$waterlevel_m), ], 30)

# 10 lowest water levels
head(df_rhone[order(df_rhone$waterlevel_m), ], 10)
# waterlevel range is between 665.061 to 668.359

# qqplot of waterlevel
par(mar = c(4, 4, 2, 1))
qqnorm(df_rhone$waterlevel_m, main = "QQ-Plot of water level vs. Normal distribution")
qqline(df_rhone$waterlevel_m, col = "red")

## TS Analysis- daily
freq_daily <- 365.2422

# creating time series object
ts_rhone <- ts(df_rhone$waterlevel_m, 
              start = c(year(min(df_rhone$date_yyyymmdd)),
                        yday(min(df_rhone$date_yyyymmdd))), 
              frequency = freq_daily)

# run initial ADF
test <- adf.test(ts_rhone)
cat("Original series: Dickey-Fuller =", round(test$statistic, 3),
    ", p-value =", test$p.value, "\n")
# according to ADF waterlevel_m is stationary

# plot daily TS of Rhone waterlevel
plot(ts_rhone, 
     main = "Daily Time Series of Rhone Water Level", 
     ylab = "Water Level (m)",
     xlab = "Time",
     type = "l",
     col = "blue")

# Add smoothed trend line
lines(lowess(ts_rhone), col = "red", lwd = 2)

# ACF and PACF of daily TS
# ACF really slow decay, PACF no cut-off....Trend? Differencing?
acf(ts_rhone, main = "Daily TS ACF")
pacf(ts_rhone, main = "Daily TS PACF")

## STL decomp 
decomp_stl_rhone <- stl(ts_rhone, s.window = "periodic")
plot(decomp_stl_rhone)
mtext(paste("STL Decomposition of Rhone Water Level-s.window Periodic"))

# plot ACF and PACF
par(mfrow = c(1, 2))
remainder_periodic <- decomp_stl_rhone$time.series[,"remainder"] 
acf(remainder_periodic, main = "ACF-periodic")
pacf(remainder_periodic, main = "PACF-periodic")
adf.test(remainder_periodic)

# Salomons suggestion for starting setting: s.window=2001, t.window=5001
decomp_stl_rhone_2 <- stl(ts_rhone, s.window = 2001, t.window = 5001)
plot(decomp_stl_rhone_2)
mtext(paste("STL Decomposition of Rhone Water Level-s.window 2001 and t.window=5001"))

# plot ACF and PACF
par(mfrow = c(1, 2))
remainder_2 <- decomp_stl_rhone_2$time.series[,"remainder"]
#same pattern: ACF slow decay plus now some periodicity, PACF no cut-off
acf(remainder_2, main = "ACF-2001/5001")
pacf(remainder_2, main = "PACF-2001/5001") 
adf.test(remainder_2)



## if needed:
# doing monthly df to be able to compare with monthly massa df
df_rhone_monthly <- df_rhone %>%
  mutate(
    year = year(date_yyyymmdd),
    month = month(date_yyyymmdd)
  ) %>%
  group_by(year, month) %>%
  summarise(
    date_yyyymmdd = first(date_yyyymmdd),
    waterlevel_m = mean(waterlevel_m),
    discharge_vol_m3s = mean(discharge_vol_m3s),
    precipitation_mm = sum(precipitation_mm),
    temp_mean = mean(temp_mean),
    .groups = "drop"
  )

cat("Monthly data created:", nrow(df_rhone_monthly), "observations\n") 
cat("Date range:", as.character(min(df_rhone_monthly$date_yyyymmdd)), "to", as.character(max(df_rhone_monthly$date_yyyymmdd)), "\n")
