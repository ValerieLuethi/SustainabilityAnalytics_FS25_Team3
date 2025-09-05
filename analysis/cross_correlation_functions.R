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



