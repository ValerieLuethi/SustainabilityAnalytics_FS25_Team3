library(dplyr)
library(lubridate)

# load merged dataset
df_merged <- readRDS("data/processed/df_merged.rds")
head(df_merged)
# select columns relevant for ccf analysis
df_ccf <- df_merged %>%
  select(
    Date,
    remainder_jungfrau,
    remainder_precipitation,
    stochastic_glacier,
    stochastic_waterlevel
  )
head(df_ccf)

# create ts objects
# Number of observations
n <- nrow(df_ccf)
# Create time series objects (monthly frequency)
# Example: temperature and glacier mass balance
start_year <- year(min(df_ccf$Date))
start_month <- month(min(df_ccf$Date))

# Create monthly time series objects
temp_ts <- ts(df_ccf$remainder_jungfrau,
              start = c(start_year, start_month),
              frequency = 12)

glacier_ts <- ts(df_ccf$stochastic_glacier,
                 start = c(start_year, start_month),
                 frequency = 12)
precip_ts <- ts(df_ccf$remainder_precipitation,
                start = c(start_year, start_month),
                frequency = 12)

waterlevel_ts <- ts(df_ccf$stochastic_waterlevel,
                    start = c(start_year, start_month),
                    frequency = 12)

# Influence of temperature, glacier mass balance and precipitation on river level
# plot remainders
plot(temp_ts)
acf(temp_ts)
pacf(temp_ts)
plot(glacier_ts)
acf(glacier_ts)
pacf(glacier_ts)
plot(precip_ts)
acf(precip_ts)
pacf(precip_ts)
plot(waterlevel_ts)
acf(waterlevel_ts)
pacf(waterlevel_ts)

# CCF analysis
# temperature and glacier
ccf(temp_ts, glacier_ts)
ccf(glacier_ts, temp_ts)
# temperature and waterlevel
ccf(temp_ts, waterlevel_ts) # ccf(dv, iv)
ccf(waterlevel_ts, temp_ts) # lag -> 8-10, delayed response of waterlevel
# glacier and waterlevel
ccf(glacier_ts, waterlevel_ts)
ccf(waterlevel_ts, glacier_ts) # leading & lagging, likely influence of other factors?, lag9
# precipitation and waterlevel
ccf(precip_ts, waterlevel_ts)
ccf(waterlevel_ts, precip_ts) # only lag 27 sig. ?

# coefficients (lagged remainders)
temp_lag8 <- stats::lag(temp_ts, k = -1) # lag 1 months forward
cor(temp_lag8, waterlevel_ts)

glacier_lag9 <- stats::lag(glacier_ts, k = ) # lag 9 months forward
cor(glacier_lag9, waterlevel_ts) # negative correlation?



# ARIMAX

# Feature engineering

''' plots
plot(temp_ts, type="l", col="darkorange", lwd=2, ylab="Temperature residual", xlab="Time")
plot(glacier_ts, type="l", col="darkblue", lwd=2, ylab="Glacier mass balance residual", xlab = "Time")
plot(precip_ts, tpye="l", col="green", lwd=2, ylab="Precipitation residual", xlab="Time")
plot(waterlevel_ts, type="l", col="purple", lwd=2, ylab="Waterlevel residual", xlab="Time")

# overlay signals
plot(waterlevel_ts, type="l", col="purple", lwd=2, ylab="Residual / Stochastic signal", xlab="Time", main="Waterlevel & Temperature")
lines(temp_ts, col="darkorange", lwd=2)

plot(waterlevel_ts, type="l", col="purple", lwd=2, ylab="Residual / Stochastic signal", xlab="Time", main="Waterlevel & Glacier")
lines(glacier_ts, col="darkblue", lwd=2)

plot(waterlevel_ts, type="l", col="purple", lwd=2, ylab="Residual / Stochastic signal", xlab="Time", main="Waterlevel & Glacier")
lines(precip_ts, col="green", lwd=2)
'''

'''# --- 1. Simple test series ---
x <- ts(1:12, start=c(2000,1), frequency=12)   # independent/driver
y <- ts(1:12, start=c(2000,1), frequency=12)   # response same as x

# --- 2. Shift x with different k values ---
x_lag1  <- stats::lag(x, k=1)   # shift backward 1 month
x_lag3  <- stats::lag(x, k=3)   # shift backward 3 months
x_lead2 <- stats::lag(x, k=-2)  # shift forward 2 months

# --- 3. Plot all series ---
plot(x, type="o", col="black", ylim=c(0,14), ylab="Value", xlab="Time", main="Effect of k in stats::lag()")
lines(x_lag1, type="o", col="blue")
lines(x_lag3, type="o", col="red")
lines(x_lead2, type="o", col="green")
legend("topleft", legend=c("x original","lag 1","lag 3","lead 2"), col=c("black","blue","red","green"), lty=1, pch=1)'''




