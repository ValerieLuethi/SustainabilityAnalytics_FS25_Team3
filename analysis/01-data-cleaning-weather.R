# libraries
library(readr)
library(dplyr)
library(lubridate)
library(tseries)
library(urca)

# read the datasets
df_jungfraujoch <- read.delim("data/raw/weather_monthly_jungfraujoch.csv", sep = ";")
df_sion <- read.delim("data/raw/weather_monthly_sion.csv", sep = ';')
df_brig_precipitation <- read.delim("data/raw/weather_rain_monthly_brig.csv")



# convert timestamp to datetime
df_jungfraujoch <- df_jungfraujoch %>%
  mutate(reference_timestamp = dmy_hm(reference_timestamp),
         year_month = floor_date(reference_timestamp, "month"))
# only keep columns needed for analysis
df_jungfraujoch_cleaned <- df_jungfraujoch %>% 
  select(station_abbr, reference_timestamp, ths200m0, th9120mv)
# rename columns
colnames(df_jungfraujoch_cleaned) <- c('station_abbr', 'reference_timestamp', 'temp_mean_C', 'temp_deviation_C')
head(df_jungfraujoch_cleaned)
summary(df_jungfraujoch_cleaned)
# check for NAs
colSums(is.na(df_jungfraujoch_cleaned)) # no NAs

# create time series object
# temp_mean monthly series
temp_mean_ts_jungfraujoch <- ts(df_jungfraujoch_cleaned$temp_mean_C,
                   start = c(1933, 1), frequency = 12)
# temp_deviation monthly series
temp_deviation_ts_jungfraujoch <- ts(df_jungfraujoch_cleaned$temp_deviation_C,
                        start = c(1933, 1), frequency = 12)

# plots
plot(temp_mean_ts_jungfraujoch, main = "Monthly Mean Temperature Jungfraujoch", ylab = "Temp (°C)")
plot(temp_deviation_ts_jungfraujoch, main = "Monthly Temperature Deviation Jungfraujoch", ylab = "Temp (°C)")

# decomposition (trend, seasonality, residuals)
temp_mean_ts_jungfraujoch_comp=decompose(temp_mean_ts_jungfraujoch)
temp_deviation_ts_jungfraujoch_comp=decompose(temp_deviation_ts_jungfraujoch)
plot(temp_mean_ts_jungfraujoch_comp)
plot(temp_deviation_ts_jungfraujoch_comp)

# adjust for seasonality
temp_mean_adj <- temp_mean_ts_jungfraujoch - temp_mean_ts_jungfraujoch_comp$seasonal
temp_dev_adj  <- temp_deviation_ts_jungfraujoch - temp_deviation_ts_jungfraujoch_comp$seasonal
plot(temp_mean_adj)
plot(temp_dev_adj)

# check stationarity
adf.test(temp_mean_adj)
adf.test(temp_dev_adj)

# check ACF and PACF
acf(temp_mean_adj_diff, main = "ACF: Differenced Temp Mean")
pacf(temp_mean_adj_diff, main = "PACF: Differenced Temp Mean")

acf(temp_dev_adj_diff, main = "ACF: Differenced Temp Deviation")
pacf(temp_dev_adj_diff, main = "PACF: Differenced Temp Deviation")


