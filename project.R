install.packages(c("readxl","tidyverse","naniar","VIM","mice","caret","data.table","janitor","ggplot2","scales","gridExtra"))

library(readxl)
library(tidyverse)   # dplyr, ggplot2 etc.
library(naniar)      # visualise missingness
library(VIM)         # kNN imputation & missing visual
library(mice)        # multiple imputation
library(caret)       # train/test split
library(data.table)  # fast handling
library(janitor)     # clean_names()
library(gridExtra)   # arrange plots

# CSV with forward slashes
file_path <- "C:/Users/Rafit Tishan Neo/Downloads/Data Science Project/archive/earthquake_project.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# or using readr
df <- readr::read_csv(file_path)

#problem1
# make a copy to introduce issues (so raw remains untouched)
df_dirty <- df

# 1) Introduce missing values at random in magnitude and depth
n <- nrow(df_dirty)
idx_mag_na <- sample(1:n, size = round(0.03*n))   # 3% missing magnitudes
idx_depth_na <- sample(1:n, size = round(0.03*n)) # 3% missing depth
df_dirty$magnitude[idx_mag_na] <- NA
df_dirty$depth_km[idx_depth_na] <- NA


#soulution for problem 1
# Count missing values
sum(is.na(df_dirty$magnitude))
sum(is.na(df_dirty$depth))
df_clean <- df_dirty

# Impute missing magnitudes
df_clean$magnitude[is.na(df_clean$magnitude)] <- 
  median(df_clean$magnitude, na.rm = TRUE)

# Impute missing depth_km
df_clean$depth[is.na(df_clean$depth)] <- 
  median(df_clean$depth, na.rm = TRUE)

#p1 solution
sum(is.na(df_clean$magnitude))
sum(is.na(df_clean$depth))

table(is.na(df_dirty$magnitude), is.na(df_clean$magnitude))
table(is.na(df_dirty$depth), is.na(df_clean$depth))


# 2) Introduce some invalid magnitude values (strings, negatives)
bad_idx <- sample(1:n, size = round(0.01*n))  # 1% invalid
df_dirty$magnitude[bad_idx[1:floor(length(bad_idx)/2)]] <- "unknown"
df_dirty$magnitude[bad_idx[(floor(length(bad_idx)/2)+1):length(bad_idx)]] <- -9

df_dirty[df_dirty$magnitude == "unknown", ]
df_dirty[df_dirty$magnitude == -9, ]

sum(df_dirty$magnitude == "unknown", na.rm = TRUE)
sum(df_dirty$magnitude == -9, na.rm = TRUE)

df_clean <- df_dirty

df_clean$magnitude <- suppressWarnings(as.numeric(df_clean$magnitude))
df_clean$magnitude[df_clean$magnitude < 0] <- NA

df_clean$magnitude[is.na(df_clean$magnitude)] <- 
  median(df_clean$magnitude, na.rm = TRUE)
summary(df_clean$magnitude)
sum(df_clean$magnitude < 0, na.rm = TRUE)
sum(is.na(df_clean$magnitude))

# 1) How many rows and duplicates?
nrow(df_dirty)                   # total rows
sum(duplicated(df_dirty))        # number of duplicate rows

# 2) See some duplicate examples (complete-row duplicates)
head(df_dirty[duplicated(df_dirty), ])

# 3) Remove duplicates, keeping the first occurrence
df_clean <- df_dirty[!duplicated(df_dirty), ]

# 4) Check that duplicates are gone
nrow(df_clean)                   # rows after cleaning
sum(duplicated(df_clean))        # should be 0



#problem 4

# 4) Introduce inconsistent date formats in 'date_time' column
if("date_time" %in% names(df_dirty)){
  samp <- sample(1:nrow(df_dirty), size = round(0.02 * nrow(df_dirty)))
  
  # keep only the date part from original, but in inconsistent format
  random_dates <- Sys.Date() - sample(1:1000, length(samp), replace = TRUE)
  df_dirty$date_time[samp] <- format(random_dates, "%d-%m-%Y")  # no time, different format
}

library(lubridate)

# 1) Inspect some samples that look inconsistent (no space/time part)
head(df_dirty$date_time[!grepl(" ", df_dirty$date_time)])

# 2) Try multiple possible formats using lubridate
# Your dataset currently has formats like:
#   "16-08-2023 12:47"
#   "10/7/2023 20:28"
#   "2/7/2023 10:27"
# and dirty ones like "15-03-2024"

df_dirty$dt_parsed <- parse_date_time(
  df_dirty$date_time,
  orders = c("d-m-Y H:M", "d/m/Y H:M", "d/m/y H:M", "d-m-Y"),  # add more if needed
  tz = "UTC"
)

# 3) Check parsing success
sum(is.na(df_dirty$dt_parsed))      # should be small or 0
head(df_dirty[, c("date_time", "dt_parsed")])

# 4) Optionally, convert back to a single clean character format for saving/reporting
df_dirty$date_time_clean <- format(df_dirty$dt_parsed, "%Y-%m-%d %H:%M:%S")
