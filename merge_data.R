library(dplyr)
library(lubridate)
library(zoo)

# laod data
df_jungfrau <- readRDS("data/processed/jungfraujoch_temperature.rds")
head(df_jungfrau)
df_glacier <- readRDS("data/processed/glacier_monthly_processed.rds")
head(df_glacier)
#df_waterlevel <- readRDS("data/preprocessed/waterlevel_det_stoch.rds")
df_waterlevel <- waterlevel_det_stoch
head(df_waterlevel)
#df_precipitation <- readRDS("data/preprocessed/rain_decomposition.rds")
df_precipitation <- rain_decomposition
head(df_precipitation)

str(df_jungfrau)
str(df_glacier)
str(df_precipitation)
str(df_waterlevel)

# convert date columns to identical format
# jungfrau
# Number of rows
n <- nrow(df_jungfrau)

# Create monthly sequence from start year/month
start_date <- as.Date("1933-01-01")
df_jungfrau <- df_jungfrau %>%
  mutate(Date = seq.Date(from = start_date, by = "month", length.out = n))
head(df_jungfrau)
str(df_jungfrau)
# glacier
df_glacier <- df_glacier %>%
  mutate(Date = as.Date(year_month))
head(df_glacier)
str(df_glacier)
# waterlevel
# Map German month abbreviations to month numbers
month_map <- c(
  "Jan" = 1, "Feb" = 2, "Mär" = 3, "Apr" = 4,
  "Mai" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8,
  "Sep" = 9, "Okt" = 10, "Nov" = 11, "Dez" = 12
)

df_waterlevel <- df_waterlevel %>%
  mutate(
    # split year_month into month abbreviation and year
    Month = sapply(strsplit(year_month, " "), `[`, 1),
    Year  = as.numeric(sapply(strsplit(year_month, " "), `[`, 2)),
    Month = month_map[Month],                        # map to numeric month
    Date  = as.Date(paste0(Year, "-", Month, "-01")) # create Date
  ) %>%
  select(-Month, -Year, -year_month)  # remove helper columns
head(df_waterlevel)
str(df_waterlevel)

# precipiation
str(df_precipitation) # already in right format

# merge by dates, full outer join
df_merged <- df_jungfrau %>%
  select(Date, observed, trend, seasonal, remainder) %>%
  full_join(df_glacier %>%
              select(Date, mass_balance_monthly, trend, seasonal, deterministic, stochastic),
            by = "Date", suffix = c("_jungfrau", "_glacier")) %>%
  full_join(df_waterlevel %>%
              select(Date, waterlevel_m, trend, seasonal, deterministic, stochastic),
            by = "Date", suffix = c("", "_waterlevel")) %>%
  full_join(df_precipitation %>%
              select(Date, Original, Trend, Seasonal, Remainder),
            by = "Date", suffix = c("", "_precipitation")) %>%
  arrange(Date)

head(df_merged)
str(df_merged)

# rename columns
df_merged <- df_merged %>%
  rename(
    # Jungfrau
    observed_jungfrau = observed,
    trend_jungfrau = trend_jungfrau,
    seasonal_jungfrau = seasonal_jungfrau,
    remainder_jungfrau = remainder,
    
    # Glacier
    mass_balance_glacier = mass_balance_monthly,
    trend_glacier = trend_glacier,
    seasonal_glacier = seasonal_glacier,
    deterministic_glacier = deterministic,
    stochastic_glacier = stochastic,
    
    # Waterlevel
    waterlevel = waterlevel_m,
    trend_waterlevel = trend,
    seasonal_waterlevel = seasonal,
    deterministic_waterlevel = deterministic_waterlevel,
    stochastic_waterlevel = stochastic_waterlevel,
    
    # Precipitation
    precipitation = Original,
    trend_precipitation = Trend,
    seasonal_precipitation = Seasonal,
    remainder_precipitation = Remainder
  )

# select same time span
# Find first and last date of each dataset
start_jungfrau <- min(df_merged$Date[!is.na(df_merged$observed_jungfrau)])
end_jungfrau   <- max(df_merged$Date[!is.na(df_merged$observed_jungfrau)])

start_glacier  <- min(df_merged$Date[!is.na(df_merged$mass_balance_glacier)])
end_glacier    <- max(df_merged$Date[!is.na(df_merged$mass_balance_glacier)])

start_water    <- min(df_merged$Date[!is.na(df_merged$waterlevel)])
end_water      <- max(df_merged$Date[!is.na(df_merged$waterlevel)])

start_precip   <- min(df_merged$Date[!is.na(df_merged$precipitation)])
end_precip     <- max(df_merged$Date[!is.na(df_merged$precipitation)])

# Overlapping date range
start_overlap <- max(start_jungfrau, start_glacier, start_water, start_precip)
end_overlap   <- min(end_jungfrau, end_glacier, end_water, end_precip)

df_merged_overlap <- df_merged %>%
  filter(Date >= start_overlap & Date <= end_overlap)

head(df_merged_overlap)
colSums(is.na(df_merged_overlap))
any(is.na(df_merged_overlap)) # no NAs

