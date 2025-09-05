# libraries
library(readr)
library(dplyr)
library(lubridate)
library(tseries)
library(zoo)
library(forecast)

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

# acf and pacf
acf(ts_jungfraujoch) # acf indicates seasonality
pacf(ts_jungfraujoch) # damped sinusoid

acf(ts_sion) # acf indicates seasnality
pacf(ts_sion) # damped sinusoid

# decomposition
ts_jungfraujoch_comp = decompose(ts_jungfraujoch)
plot(ts_jungfraujoch_comp)
ts_sion_comp = decompose(ts_sion)
plot(ts_sion_comp) # upward trend starting aaround 1980

# check stationarity
adf.test(ts_jungfraujoch, alternative = "stationary") # p = .01 indicates stationarity
adf.test(ts_sion, alternative = "stationary") # p = 0.1 indicates stationarity

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
pacf(residuals_clean_sion) # pacf shows strong negative lags - AR process?

# decomposition with stl
ts_jungfraujoch_stl <- stl(ts_jungfraujoch, s.window = "periodic") # assumes strong, stable seasonality
plot(ts_jungfraujoch_stl, main = "s.window periodic")
ts_jungfraujoch_stl_13 <- stl(ts_jungfraujoch, s.window = 13) # 1 year smooth
plot(ts_jungfraujoch_stl_13, main = "s.window = 13")
ts_jungfraujoch_stl_25 <- stl(ts_jungfraujoch, s.window = 25) # 2 year smooth
plot(ts_jungfraujoch_stl_25, main = "s.window = 25")
ts_jungfraujoch_stl_37 <- stl(ts_jungfraujoch, s.window = 37) # 3 year smooth
plot(ts_jungfraujoch_stl_37, main = "s.window = 37")
ts_jungfraujoch_stl_37_121 <- stl(ts_jungfraujoch, s.window = 37, t.window=121) # 3 year smooth
plot(ts_jungfraujoch_stl_37_121, main = "s.window = 37, t.window=121")
ts_jungfraujoch_stl_49 <- stl(ts_jungfraujoch, s.window = 49)
plot(ts_jungfraujoch_stl_49)

remainder_periodic <- ts_jungfraujoch_stl$time.series[, "remainder"]
acf(remainder_periodic, main = "ACF of STL remainder, periodic")
pacf(remainder_periodic, main = "PACF of STL remainder, periodic")

remainder_13 <- ts_jungfraujoch_stl_13$time.series[, "remainder"]
acf(remainder_13, main = "ACF of STL remainder, s.window = 13")
pacf(remainder_13, main = "PACF of STL remainder, s.window = 13")

remainder_25 <- ts_jungfraujoch_stl_25$time.series[, "remainder"]
acf(remainder_25, main="ACF of STL remainder, s.window = 25")
pacf(remainder_25, main = "PACF of STL remainder, s.window = 25")

remainder_37 <- ts_jungfraujoch_stl_37$time.series[, "remainder"]
acf(remainder_37, main = "ACF of STL remainder, s.window = 37") # ACF lags 2 and 5 persist
pacf(remainder_37, main = "PACF of STL remainder, s.window = 37")

remainder_37_121 <- ts_jungfraujoch_stl_37_121$time.series[, "remainder"]
acf(remainder_37_121)
pacf(remainder_37_121) # no change compared to without t.window

remainder_49 <- ts_jungfraujoch_stl_49$time.series[, "remainder"]
acf(remainder_49, main="ACF of STL remainder, s.window=49") # no change compared to 37 window
pacf(remainder_49, main="PACF of STL remainder, s.window=49")

# Extract components
components <- ts_jungfraujoch_stl_37_121$time.series

# Create a time index (Date or Year-Month depending on your ts object)
dates <- time(ts_jungfraujoch)

# Build a data frame
df_jungfraujoch_processed <- data.frame(
  date       = dates,
  observed   = as.numeric(ts_jungfraujoch),
  trend      = as.numeric(components[, "trend"]),
  seasonal   = as.numeric(components[, "seasonal"]),
  remainder  = as.numeric(components[, "remainder"])
)

head(df_jungfraujoch_processed)
str(df_jungfraujoch_processed)

# create processed data file
write.csv(df_jungfraujoch_processed, "data/processed/jungfraujoch_temperature.csv", row.names = FALSE)
# RDS
saveRDS(df_jungfraujoch_processed, "data/processed/jungfraujoch_temperature.rds")

''' combine the two datasets for plotting
dates <- seq.Date(from = as.Date("1933-01-01"),
                  by = "month", 
                  length.out = length(ts_jungfraujoch))
df_temperature <- data.frame(
  date = dates,
  jungfraujoch = as.numeric(ts_jungfraujoch),
  sion = as.numeric(ts_sion)
)
head(df_temperature)
str(df_temperature)

# multivariate ts object
ts_temperature <- ts(df_temperature[, c("jungfraujoch", "sion")],
                 start = c(1933, 1),
                 frequency = 12)
head(ts_temperature)
str(ts_temperature) '''
