library(tseries)
library(astsa)
library(imputeTS)
library(forecast)
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# read the dataset
df_massa <- read.csv("data/processed/massa_cleaned.csv", stringsAsFactors = FALSE)
# check data types
str(df_massa)

# convert to date and numeric
df_massa$date_yyyymmdd <- as.Date(df_massa$date_yyyymmdd)
df_massa$snow_water_equiv_mm <- as.numeric(df_massa$snow_water_equiv_mm)
df_massa$imputed_snow_water_equiv <- as.numeric(df_massa$imputed_snow_water_equiv)
str(df_massa)

# create monthly df
df_monthly <- df_massa %>%
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

cat("Monthly data created:", nrow(df_monthly), "observations\n") 
cat("Date range:", as.character(min(df_monthly$date_yyyymmdd)), "to", as.character(max(df_monthly$date_yyyymmdd)), "\n")

# create ts object for warerlevel
waterlevel_ts <- ts(df_monthly$waterlevel_m, 
                    start = c(year(min(df_monthly$date_yyyymmdd)),
                              month(min(df_monthly$date_yyyymmdd))),
                    frequency = 12)

plot(waterlevel_ts, main = "Original Monthly Water Level", 
     ylab = "Water Level (m)", xlab = "Time")

adf.test(waterlevel_ts) #p-value 0.01 (<0.05) so according to ADF stationary

stl_decomp <- stl(waterlevel_ts, s.window = "periodic")
plot(stl_decomp)


par(mfrow = c(1, 2))
acf(waterlevel_ts)
pacf(waterlevel_ts)

remainder_waterlevel <- stl_decomp$time.series[,"remainder"]
adf.test(remainder_waterlevel) # p-value 0.01
acf(remainder_waterlevel, main = "ACF - STL Remainder")
pacf(remainder_waterlevel, main = "PACF - STL Remainder")
par(mfrow = c(1, 1))

### TESTING s.window and t.window
#s.window=13, t.window=default
stl_1 <- stl(waterlevel_ts, s.window = 13)
plot(stl_1)

remainder_stl1 <- stl_1$time.series[,"remainder"]
adf.test(remainder_stl1) # p-value 0.01
acf(remainder_stl1, main = "ACF - STL Remainder")
pacf(remainder_stl1, main = "PACF - STL Remainder")
par(mfrow = c(1, 1))


#s.window=13, t.window=121
stl_2 <- stl(waterlevel_ts, s.window = 13, t.window = 121)
plot(stl_2)

remainder_stl2 <- stl_2$time.series[,"remainder"]
adf.test(remainder_stl2) # p-value 0.01
acf(remainder_stl2, main = "ACF - STL Remainder")
pacf(remainder_stl2, main = "PACF - STL Remainder")
par(mfrow = c(1, 1))


#s.window="periodic", t.window=121
stl_3 <- stl(waterlevel_ts, s.window = "periodic", t.window = 121)
plot(stl_3)

remainder_stl3 <- stl_3$time.series[,"remainder"]
adf.test(remainder_stl3) # p-value 0.01
acf(remainder_stl3, main = "ACF - STL Remainder")
pacf(remainder_stl3, main = "PACF - STL Remainder")
par(mfrow = c(1, 1))


#s.window="periodic", t.window=121
stl_4 <- stl(waterlevel_ts, s.window = 35)
plot(stl_4)

remainder_stl4 <- stl_4$time.series[,"remainder"]
adf.test(remainder_stl4) # p-value 0.01
acf(remainder_stl4, main = "ACF - STL Remainder")
pacf(remainder_stl4, main = "PACF - STL Remainder")
par(mfrow = c(1, 1))

#s.window=35, t.window=121
stl_5 <- stl(waterlevel_ts, s.window = 35, t.window=121)
plot(stl_5)

remainder_stl5 <- stl_5$time.series[,"remainder"]
adf.test(remainder_stl5) # p-value 0.01
acf(remainder_stl5, main = "ACF - STL Remainder")
pacf(remainder_stl5, main = "PACF - STL Remainder")
par(mfrow = c(1, 1))

## take decomp with s.window=35, t.window=121 as final decomp and create df
stl_result <- stl(waterlevel_ts, s.window = 35, t.window=121)
trend_comp <- as.numeric(stl_result$time.series[,"trend"])
seasonal_comp <- as.numeric(stl_result$time.series[,"seasonal"])
remainder_comp <- as.numeric(stl_result$time.series[,"remainder"])

# Create the det_stoch dataframe
det_stoch <- data.frame(
  year_month = format(df_monthly$date_yyyymmdd, "%b %Y"),
  waterlevel_m = df_monthly$waterlevel_m,
  trend = trend_comp,
  seasonal = seasonal_comp,
  deterministic = trend_comp + seasonal_comp,
  stochastic = remainder_comp
)


# save det_stoch df as CSV
write.csv(det_stoch, "data/processed/waterlevel_det_stoch.csv", row.names = FALSE)
                