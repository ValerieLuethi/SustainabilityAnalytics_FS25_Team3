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

## Time Series Massa River - daily data
freq_daily <- 365.2422
numeric_cols <- names(df_massa)[sapply(df_massa, is.numeric)]
# key variables for detailed analysis
key_vars <- c("waterlevel_m", "discharge_vol_m3s", "precipitation_mm")

for(col in numeric_cols){
  
  cat("\n==============================\n")
  cat("ADF test for:", col, "\n")
  
  # creating time series object
  ts_data <- ts(df_massa[[col]], 
                start = c(year(min(df_massa$date_yyyymmdd)),
                          yday(min(df_massa$date_yyyymmdd))), 
                frequency = freq_daily)
  
  # run initial ADF
  test <- adf.test(ts_data)
  cat("Original series: Dickey-Fuller =", round(test$statistic, 3),
      ", p-value =", test$p.value, "\n")
 # according to ADF all numeric columns are stationary
  
  # ## perform differencing if not stationary
  # if (test$p.value > 0.05) {
  #   ts_data_diff <- diff(ts_data)
  #   test <- adf.test(ts_data_diff)
  #   cat("After 1 difference: Dickey-Fuller =",
  #       round(test$statistic, 3), ", p-value =", test$p.value, "\n")
  #   
  #   ## saving differenced series as new column
  #   new_colname <- paste0(col, "_diff")
  #   df_massa[[new_colname]] <- c(NA, as.numeric(ts_data_diff))
  # }
  
  
  ##Time series plot with LOWESS smoothing
  plot(ts_data, main = paste("Daily Time Series of", col), ylab = col)
  lines(lowess(ts_data), col = "red")
  
  if(col %in% key_vars) {
    cat("Creating decomposition for:", col, "\n")
    
    # Classical decomposition
    decomp_classical <- decompose(ts_data, type = "additive")
    plot(decomp_classical)
    mtext(paste("Daily Classical Decomposition -", col))
    
    # STL decomposition
    decomp_stl <- stl(ts_data, s.window = "periodic")
    plot(decomp_stl)
    mtext(paste("Daily STL Decomposition -", col))
  
  #if(col %in% key_vars) {
    #cat("Creating ACF/PACF plots for key variable:", col, "\n")
    par(mfrow = c(1, 2))
    acf(ts_data, main = paste("Daily ACF -", col))
    pacf(ts_data, main = paste("Daily PACF -", col))
    par(mfrow = c(1, 1))
  }
}


## Time Series Massa River - monthly data

# change daily data to monthly

df_monthly <- df_massa %>%
  mutate(
    year = year(date_yyyymmdd),
    month = month(date_yyyymmdd)
  ) %>%
  group_by(year, month) %>%
  summarise(
    waterlevel_m = mean(waterlevel_m),
    discharge_vol_m3s = mean(discharge_vol_m3s),
    discharge_specific_mm = mean(discharge_specific_mm),
    precipitation_mm = sum(precipitation_mm),
    temp_min = mean(temp_min),
    temp_mean = mean(temp_mean),
    temp_max = mean(temp_max),
    rel_sunshine_duration = mean(rel_sunshine_duration),
    .groups = "drop"
  )

# make sure data type is numeric
numeric_cols_monthly <- names(df_monthly)[!names(df_monthly) %in% c("year", "month")]

for(col in numeric_cols_monthly){
  
  cat("\n==============================\n")
  cat("Monthly ADF test for:", col, "\n")
  
  # creating time series object
  ts_data_mon <- ts(df_monthly[[col]], 
                start = c(year(min(df_massa$date_yyyymmdd)),
                          month(min(df_massa$date_yyyymmdd))), 
                frequency = 12) # months per year
  
  # run initial ADF
  test <- adf.test(ts_data_mon)
  cat("Monthly series: Dickey-Fuller =", round(test$statistic, 3),
      ", p-value =", test$p.value, "\n")
  
  # according to ADF all numeric columns are stationary for the monthly ts (p-value less than 0.5)
  
  # # ## perform differencing if not stationary
  #  if (test$p.value > 0.05) {
  #    ts_data_diff <- diff(ts_data)
  #    test <- adf.test(ts_data_diff)
  #    cat("After 1 difference: Dickey-Fuller =",
  #        round(test$statistic, 3), ", p-value =", test$p.value, "\n")
  #    
  #    ## saving differenced series as new column
  # #   new_colname <- paste0(col, "_diff")
  # #   df_massa[[new_colname]] <- c(NA, as.numeric(ts_data_diff))
  #  }
  
  # focus on most important columns: waterlevel_m, discharge_vol_m3s, precipitation_mm
  
  # acf(ts_data, main = paste("ACF of", col))
  # pacf(ts_data, main = paste("PACF of", col))
  
  ##Time series plot with LOWESS smoothing
  plot(ts_data_mon, main = paste("Monthly Time Series of", col), ylab = col)
  lines(lowess(ts_data_mon), col = "red")
  
  if(col %in% key_vars) {
    cat("Creating decomposition for:", col, "\n")
    
    # Classical decomposition
    decomp_classical <- decompose(ts_data_mon, type = "additive")
    plot(decomp_classical)
    mtext(paste("Monthly Classical Decomposition -", col))
    
    # STL decomposition
    decomp_stl <- stl(ts_data_mon, s.window = "periodic")
    plot(decomp_stl)
    mtext(paste("Monthly STL Decomposition -", col))
    
    par(mfrow = c(1, 2))
    acf(ts_data_mon, main = paste("Monthly ACF -", col))
    pacf(ts_data_mon, main = paste("Monthly PACF -", col))
    par(mfrow = c(1, 1))
  }
}


#write.csv(massa_stationary, "data/processed/glacier_cleaned.csv", row.names = FALSE)