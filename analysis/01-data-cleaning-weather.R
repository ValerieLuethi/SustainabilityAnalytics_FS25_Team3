# libraries
library(readr)
library(dplyr)
library(lubridate)
library(tseries)

# read the dataset
df_jungfraujoch <- read.delim("data/raw/weather_monthly_jungfraujoch.csv", sep = ";")
# convert timestamp to datetime
df_jungfraujoch <- df_jungfraujoch %>%
  mutate(reference_timestamp = dmy_hm(reference_timestamp),
         year_month = floor_date(reference_timestamp, "month"))
# only keep columns needed for analysis
df_jungfraujoch_cleaned <- df_jungfraujoch %>% 
  select(station_abbr, reference_timestamp, ths200m0, th9120mv)
# rename columns
colnames(df_jungfraujoch_cleaned) <- c('station_abbr', 'reference_timestamp', 'temp_mean', 'temp_deviation')
head(df_jungfraujoch_cleaned)
# check for NAs
colSums(is.na(df_jungfraujoch_cleaned)) # no NAs

# create time series object
# temp_mean monthly series
temp_mean_ts_jungfraujoch <- ts(df_jungfraujoch_cleaned$temp_mean,
                   start = c(1933, 1), frequency = 12)
# temp_deviation monthly series
temp_deviation_ts_jungfraujoch <- ts(df_jungfraujoch_cleaned$temp_deviation,
                        start = c(1933, 1), frequency = 12)
# plots
plot(temp_mean_ts_jungfraujoch, main = "Monthly Mean Temperature Jungfraujoch", ylab = "Temp (°C)")
plot(temp_deviation_ts_jungfraujoch, main = "Monthly Temperature Deviation Jungfraujoch", ylab = "Temp (°C)")

# check stationarity
adf.test(temp_mean_ts_jungfraujoch) # p-value is less than .05 hence stationary
adf.test(temp_deviation_ts_jungfraujoch) # p-value is less than .05 hence stationary

# decomposition (trend, seasonality, residuals)
temp_mean_ts_jungfraujoch_comp=decompose(temp_deviation_ts_jungfraujoch)
temp_deviation_ts_jungfraujoch_comp=decompose(temp_deviation_ts_jungfraujoch)
plot(temp_mean_ts_jungfraujoch_comp)
plot(temp_deviation_ts_jungfraujoch_comp)
# STL decomposition
stl_mean <- stl(temp_mean_ts_jungfraujoch, s.window = "periodic")
stl_dev <- stl(temp_deviation_ts_jungfraujoch, s.window = "periodic")
plot(stl_mean)
plot(stl_dev)

# acf and pacf
acf(temp_mean_ts_jungfraujoch)
pacf(temp_mean_ts_jungfraujoch)

acf(temp_deviation_ts_jungfraujoch)
pacf(temp_deviation_ts_jungfraujoch)

