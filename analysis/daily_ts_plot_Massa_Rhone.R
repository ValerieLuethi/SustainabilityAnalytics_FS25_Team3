df_massa <- read.csv("data/processed/massa_cleaned.csv", stringsAsFactors = FALSE)
df_rhone <- read.csv("data/processed/rhone_cleaned.csv", stringsAsFactors = FALSE)

# check data types
str(df_massa)
df_massa <- df_massa %>%
  mutate(
    date_yyyymmdd = as.Date(date_yyyymmdd))

str(df_rhone)
df_rhone <- df_rhone %>%
  mutate(
    date_yyyymmdd = as.Date(date_yyyymmdd))

# create daily ts objects
freq_daily <- 365.2422
ts_massa <- ts(df_massa$waterlevel_m, 
              start = c(year(min(df_massa$date_yyyymmdd)),
                        yday(min(df_massa$date_yyyymmdd))), 
              frequency = freq_daily)

ts_rhone <- ts(df_rhone$waterlevel_m, 
               start = c(year(min(df_massa$date_yyyymmdd)),
                         yday(min(df_massa$date_yyyymmdd))), 
               frequency = freq_daily)

# plotting daily ts Massa
plot(ts_massa, 
     main = "Daily Time Series of Massa Water Level", 
     ylab = "Water Level (m) above sea level",
     xlab = "Time",
     type = "l",
     col = "blue")
# Add smoothed trend line
lines(lowess(ts_massa), col = "red", lwd = 2)
# to add a legend
 legend("bottomleft", 
        legend = c("Water Level", "Lowess Trend"), 
        col = c("blue", "red"), 
        lty = 1, 
        lwd = c(1, 2))

# plotting daily TS Rhone
plot(ts_rhone, 
     main = "Daily Time Series of Rhone Water Level", 
     ylab = "Water Level (m) above sea level",
     xlab = "Time",
     type = "l",
     col = "blue")

# Add smoothed trend line
lines(lowess(ts_rhone), col = "red", lwd = 2)