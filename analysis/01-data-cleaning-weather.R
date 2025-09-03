# libraries
library(readr)
library(dplyr)
library(lubridate)
library(tseries)

# read weather datasets
df_jungfraujoch <- read.delim("data/raw/weather_monthly_jungfraujoch.csv", header=TRUE, sep = ";")
head(df_jungfraujoch)
str(df_jungfraujoch)
df_sion <- read.delim("data/raw/weather_monthly_sion.csv", header=TRUE, sep = ';')
head(df_sion)
str(df_sion)

# convert timestamp to datetime
df_jungfraujoch <- df_jungfraujoch %>%
  mutate(reference_timestamp = dmy_hm(reference_timestamp))
str(df_jungfraujoch)
df_sion <- df_sion %>% 
  mutate(reference_timestamp = dmy_hm(reference_timestamp))
str(df_sion)

# only keep columns needed for analysis
df_jungfraujoch_reduced <- df_jungfraujoch %>% 
  select(station_abbr, reference_timestamp, ths200m0)
df_sion_reduced <- df_sion %>% 
  select(station_abbr, reference_timestamp, ths200m0)

# rename columns
colnames(df_jungfraujoch_reduced) <- c('station_abbr', 'reference_timestamp', 'temp_mean_C')
colnames(df_sion_reduced) <- c('station_abbr', 'reference_timestamp', 'temp_mean_C')

# check for NAs
colSums(is.na(df_jungfraujoch_reduced)) # no NAs
colSums(is.na(df_sion_reduced)) # no NAs

# create time series object
# temp_mean monthly series
ts_jungfraujoch <- ts(df_jungfraujoch_reduced$temp_mean_C,
                      start = c(1933, 1),
                      end = c(1933 + (nrow(df_jungfraujoch_reduced)-1) %/% 12,
                              (nrow(df_jungfraujoch_reduced)-1) %% 12 + 1),
                      frequency = 12)
str(ts_jungfraujoch)

ts_sion_full <- ts(df_sion_reduced$temp_mean_C,
              start = c(1864, 1),
              end = c(1864 + (nrow(df_sion_reduced)-1) %/% 12,
                      (nrow(df_sion_reduced)-1) %% 12 + 1),
              frequency = 12)
str(ts_sion_full)

# subset of sion data to overlap with jungfrau data
ts_sion <- window(ts_sion_full, start = c(1933, 1), end = end(ts_jungfraujoch))
start(ts_sion)
end(ts_sion)
length(ts_sion)

# summary statistics
summary(ts_jungfraujoch)
summary(ts_sion)

# plots
par(mfrow=c(2,1))
plot(ts_jungfraujoch, main="Jungfraujoch Monthly Temperature", ylab="°C")
plot(ts_sion, main="Sion Monthly Temperature", ylab="°C")

# overlay plot
plot(ts_jungfraujoch, col="blue", ylab="Temperature (°C)", main="Jungfraujoch vs Sion")
lines(ts_sion, col="red")
legend("topright", legend=c("Jungfraujoch","Sion"), col=c("blue","red"), lty=1)

# decomposition
ts_jungfraujoch_comp = decompose(ts_jungfraujoch)
plot(ts_jungfraujoch_comp)
ts_sion_comp = decompose(ts_sion)
plot(ts_sion_comp)

# check stationarity
adf.test(ts_jungfraujoch, alternative = "stationary")
adf.test(ts_sion, alternative = "stationary")

# plot trend
plot(ts_jungfraujoch_comp$trend, main="Trend Jungfraujoch")
plot(ts_sion_comp$trend, main="Trend Sion")

# acf and pacf of residuals
residuals_jungfrau <- ts_jungfraujoch_comp$random
residuals_clean_jungfrau <- na.omit(residuals_jungfrau)
acf(residuals_clean_jungfrau)
pacf(residuals_clean_jungfrau)

residuals_sion <- ts_sion_comp$random
residuals_clean_sion <- na.omit(residuals_sion)
acf(residuals_clean_sion)
pacf(residuals_clean_sion)

# pacf indicates seasonality



