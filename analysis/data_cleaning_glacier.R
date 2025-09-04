library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(forecast)
library(urca)
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
  
  ##Time series plot with LOWESS smoothing
  plot(ts_data, main = paste("Time Series of", col), ylab = col)
  lines(lowess(ts_data), col = "red")
}

write.csv(df_glacier, "data/processed/glacier_cleaned.csv", row.names = FALSE)
saveRDS(df_glacier, "data/processed/glacier_cleaned.rds")


################################################################################

## creating monthly data using weighted averages based on the climate data


df_climate <- readRDS("data/processed/jungfraujoch_det_stoch.rds")
head(df_climate)

df_climate <- df_climate %>%
  mutate(
    month_num = match(substr(year_month, 1, 3), month.abb),
    year_num = as.numeric(substr(year_month, 5, 8))  # extract year from "Jan 1933"
  )

## Function to allocate seasonal mass balance by fixed calendar seasons
allocate_mass_balance_per_year <- function(row, df_climate){
  
  obs_year <- year(row$obs_start_date)  # ie Observation year for this glacier row
  
  ## Winter months: Oct–Dec from previous year, Jan–Apr from current observation year
  winter_climate <- df_climate %>%
    filter((month_num %in% 10:12 & year_num == (obs_year - 1)) |
             (month_num %in% 1:4 & year_num == obs_year))
  
  winter_weights <- abs(winter_climate$observed)
  if(sum(winter_weights) == 0) winter_weights <- rep(1/nrow(winter_climate), nrow(winter_climate))
  winter_weights <- winter_weights / sum(winter_weights)
  winter_alloc <- winter_weights * row$winter_mass_balance_mmwe
  
  ## Summer months: May–Sep of current observation year
  summer_climate <- df_climate %>%
    filter(month_num %in% 5:9 & year_num == obs_year)
  
  summer_weights <- abs(summer_climate$observed)
  if(sum(summer_weights) == 0) summer_weights <- rep(1/nrow(summer_climate), nrow(summer_climate))
  summer_weights <- summer_weights / sum(summer_weights)
  summer_alloc <- summer_weights * row$summer_mass_balance_mmwe
  
  ## Combine summer and winter calculations
  dates <- c(winter_climate$year_month, summer_climate$year_month)
  alloc <- c(winter_alloc, summer_alloc)
  
  ## Amend the data frame
  data.frame(
    year_month = dates,
    month = c(winter_climate$month_num, summer_climate$month_num),
    mass_balance_monthly = alloc
  )
}

## Contruct df_glacier_monthly
df_glacier_monthly <- bind_rows(lapply(1:nrow(df_glacier), function(i){
  allocate_mass_balance_per_year(df_glacier[i, ], df_climate)
}))

head(df_glacier_monthly,10)

ts_glacier <- ts(df_glacier_monthly$mass_balance_monthly, frequency = 12,
                 start = c(1933, 1))

###Plotting and ANalyzing

autoplot(ts_glacier) +
  ggtitle("Monthly Glacier Mass Balance") +
  ylab("mm w.e.") + xlab("Time")

adf_raw <- adf.test(ts_glacier)
print(adf_raw)

ts_seasonal <- diff(ts_glacier, lag = 12)
adf_seasonal <- ur.df(ts_seasonal, type = "drift", selectlags = "AIC")
summary(adf_seasonal)

autoplot(ts_seasonal) + 
  ggtitle("Seasonally Differenced Glacier Mass Balance") +
  ylab("Change (mm w.e.)")

ggAcf(ts_seasonal) + ggtitle("ACF after Seasonal Differencing")
ggPacf(ts_seasonal) + ggtitle("PACF after Seasonal Differencing")
decomp <- stl(ts_seasonal, s.window = "periodic")
autoplot(decomp)

## Extract residuals
residuals_stl <- decomp$time.series[,"remainder"]

## Plot ACF and PACF of residuals
par(mfrow=c(2,1))
acf(residuals_stl, main="ACF of STL Residuals", na.action=na.pass)
pacf(residuals_stl, main="PACF of STL Residuals", na.action=na.pass)

## adding components from stl decomp on original time series to the df
decomp_original <- stl(ts_glacier, s.window = "periodic")
df_glacier_monthly$trend <- as.numeric(decomp_original$time.series[,"trend"])
df_glacier_monthly$seasonal <- as.numeric(decomp_original$time.series[,"seasonal"]) 
df_glacier_monthly$deterministic <- df_glacier_monthly$trend + df_glacier_monthly$seasonal
df_glacier_monthly$stochastic <- as.numeric(decomp_original$time.series[,"remainder"])

write.csv(df_glacier_monthly, "data/processed/glacier_monthly_processed.csv", row.names = FALSE)
saveRDS(df_glacier_monthly, "data/processed/glacier_monthly_processed.rds")
