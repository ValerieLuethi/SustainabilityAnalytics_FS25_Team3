library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(forecast)
library(lubridate)

## reading the dataset
df_glacier <- read.csv("data/raw/massbalance_observation.csv",sep = ";")
## remove unwanted columns
df_glacier <- df_glacier %>% 
  select(-c("glacier.id", "observer", "X", "X.1", "X.2", "minimum.elevation.of.glacier", "maximum.elevation.of.glacier" ))
## rename columns to standard format
colnames(df_glacier) <- c("glacier_name", "obs_start_date", "obs_winter_end_date", "obs_end_date", 
                          "winter_mass_balance_mmwe", "summer_mass_balance_mmwe", "annual_mass_balance_mmwe", 
                          "equilibrium_line_altitude_m", "accumulation_area_ratio", "glacier_area_m2")
## filter out rows of unwanted glaciers
df_glacier <- df_glacier %>%
  filter(glacier_name == "Grosser Aletschgletscher")
## convert relevant columns to date type values
df_glacier <- df_glacier %>%
  mutate(across(c(obs_start_date, obs_winter_end_date, obs_end_date), 
                ~ as.Date(.x, format = "%d.%m.%Y")))
## check for NAs
colSums(is.na(df_glacier))

str(df_glacier)
head(df_glacier)


numeric_cols <- c("winter_mass_balance_mmwe", 
                  "summer_mass_balance_mmwe", 
                  "annual_mass_balance_mmwe", 
                  "equilibrium_line_altitude_m", 
                  "accumulation_area_ratio", 
                  "glacier_area_m2")

for(col in numeric_cols){
  
  cat("\n==============================\n")
  cat("ADF test for:", col, "\n")
  
  ## creating time series object
  ts_data <- ts(df_glacier[[col]], 
                start = as.numeric(format(min(df_glacier$obs_end_date), "%Y")), 
                frequency = 1) # yearly data
  
  ## run initial ADF
  test <- adf.test(ts_data)
  cat("Original series: Dickey-Fuller =", round(test$statistic, 3),
      ", p-value =", test$p.value, "\n")
  
  ## perform differencing if not stationary
  if (test$p.value > 0.05) {
    ts_data_diff <- diff(ts_data)
    test <- adf.test(ts_data_diff)
    cat("After 1 difference: Dickey-Fuller =",
        round(test$statistic, 3), ", p-value =", test$p.value, "\n")
    
    ## saving differenced series as new column
    new_colname <- paste0(col, "_diff")
    df_glacier[[new_colname]] <- c(NA, as.numeric(ts_data_diff))
  }
  
  # acf(ts_data, main = paste("ACF of", col))
  # pacf(ts_data, main = paste("PACF of", col))
  
  ##Time series plot with LOWESS smoothing
  plot(ts_data, main = paste("Time Series of", col), ylab = col)
  lines(lowess(ts_data), col = "red")
}

write.csv(df_glacier, "data/processed/glacier_cleaned.csv", row.names = FALSE)
