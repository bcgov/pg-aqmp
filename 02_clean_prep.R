# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#------------------------------------------------------
# read and format original data
#------------------------------------------------------

library(dplyr)

data_1hr_o <- readRDS("data/data_1hr_original.rds")

### format
# Rename column names and parameter column to lower case
# Rename datetime to date_hour_begin (datetime stamp is hour beginning): for example,
#   2015-01-01 02:00 represents data collected from 02:00 - 02:59; confirmed against Envista ARM
# Drop unnecessary columns

data_1hr <- data_1hr_o |>
  rename_with(tolower) |>
  rename(param = parameter,
         date_hour_begin = datetime) |>
  mutate(param = stringr::str_to_lower(param),
         param = case_when(
           param == "temp_mean" ~ "temp",
           param == "wdir_vect" ~ "wd",
           param == "wspd_sclr" ~ "ws",
           .default = as.character(param)),
         instrument = stringr::str_to_lower(instrument)) |>
  select(date_hour_begin, # hour begin is retained for padding data, hour end is added back later
         station_name,
         param,
         raw_value,
         rounded_value,
         instrument,
         validation_status,
         unit)

### clean
# Remove PM2.5 TEOM (keep PM2.5 SHARP): TEOM and SHARP were collocated ~ 2012(?) - 2015
# Keep one of PM10 TEOM or PM10 SHARP, which ever is not NA. The PM10 monitors were not collocated, the PM10 SHARP replaced the PM10 TEOM in May 2020.

data_1hr <- data_1hr |>
  filter(instrument != "pm25_r&p_teom") |>
  group_by(param, date_hour_begin, station_name) |>
  slice(which.max(!is.na(raw_value))) |>
  ungroup()

### pad and fill missing rows
library(lubridate)
library(tidyr)

# define start and end times
ts_start <- ymd_hm(paste0(min(year(data_1hr$date_hour_begin)), "-01-01 00:00"), tz = "Etc/GMT+8")
ts_end <- ymd_hm(paste0(max(year(data_1hr$date_hour_begin)), "-12-31 23:00"), tz = "Etc/GMT+8")

# generate hourly time series
hour_ts <- seq(from = ts_start, to = ts_end, by = "hour")

# pad missing datetime stamps based on date_hour_begin
# fill instrument, validation_status, and unit columns (down)
data_1hr <- data_1hr |>
  complete(date_hour_begin = hour_ts, station_name, param) |>
  arrange(station_name, param, date_hour_begin) |>
  fill(instrument, validation_status, unit)

# QC check: confirm data_1hr has correct number of rows
nhours <- difftime(ts_end, ts_start, unit = "hours") + 1
nparams <- n_distinct(data_1hr$param)
expected_nrows = nparams * as.numeric(nhours)

cat("The number of rows in data_1hr are as expected: ", nrow(data_1hr) == expected_nrows)
stopifnot(nrow(data_1hr) == expected_nrows)

### format
# add date_hour_end, date, time, year and month
# convert to factor: time, year, and month
# reorder columns
data_1hr <- data_1hr |>
  mutate(date_hour_end = date_hour_begin + hours(1),
         date = as.Date(date_hour_begin, tz = "Etc/GMT+8"),
         time = factor(paste0(hour(date_hour_begin), ":00"), levels = paste0(0:23, ":00")),
         year = year(date_hour_begin),
         month = factor(month(date_hour_begin, label = TRUE, abbr = TRUE), levels = month.abb[1:12]),
         param = factor(param)) |>
  select(station_name,
         date_hour_end,
         date_hour_begin,
         date,
         time,
         year,
         month,
         param,
         raw_value,
         rounded_value,
         unit,
         instrument,
         validation_status)

# if data_1hr contains expected number of rows, save dataframe to an RDS file and clean up unnecessary objects and dataframes
if(nrow(data_1hr) == expected_nrows){
  saveRDS(data_1hr, file = "data/data_1hr.rds")
  rm(ts_start)
  rm(ts_end)
  rm(hour_ts)
  rm(nhours)
  rm(nparams)
  rm(expected_nrows)
  rm(data_1hr_o)
}

#------------------------------------------------------
# calculate and format averaged data frames to long format
#------------------------------------------------------

library(openair)

#data_1hr <- readRDS("data/data_1hr.rds")

### daily average  #HERE - need to remove duplicate days - different instrument (keep 5014i)
data_24hr_meta <- data_1hr |>
  select(station_name, date, year, month, param, unit, instrument, validation_status) |>
  distinct()

data_1hr_wide <- data_1hr |>
  select(station_name, date_hour_begin, param, raw_value) |>
  tidyr::pivot_wider(names_from = param, values_from = raw_value) |>
  rename(date = date_hour_begin)

data_24hr_wide <- timeAverage(data_1hr_wide,
                         avg.time = "day",
                         data.thresh = 75,
                         statistic = "mean",
                         fill = TRUE,
                         type = "station_name")

### monthly, annual and seasonal averages
data_1m_wide <- timeAverage(data_24hr_wide,
                          avg.time = "month",
                          data.thresh = 75,
                          statistic = "mean",
                          fill = TRUE,
                          type = "station_name")

data_1y_wide <- timeAverage(data_24hr_wide,
                          avg.time = "year",
                          data.thresh = 75,
                          statistic = "mean",
                          fill = TRUE,
                          type = "station_name")

data_season_wide <- timeAverage(data_24hr_wide,
                           avg.time = "season",
                           statistic = "mean",
                           fill = TRUE,
                           type = "station_name")

# format averaged data frames to long format and add meta data back
data_24hr <- data_24hr_wide |>
  pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         value = round(value, 1),
         param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

data_1m <- data_1m_wide |>
  pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         value = round(value, 1),
         param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

data_1y <- data_1y_wide |>
  pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

data_season <- data_season_wide |>
  pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

#------------------------------------------------------
# add tf_ee to data_1hr and data_24hr: based on data/tfee-log.csv
#------------------------------------------------------

### notes:
# envair considers tfee from 2017 onwards
# data/tfee_log.csv is based on an independent review (by G.Roth):
# includes 2010 - 2024 for all Omineca-Peace region stations
# includes 2010 - 2021 (?) for select Skeena region stations
# flag_tfee = TRUE indicates evidence of wildfire smoke in the vicinity of the monitor (e.g. satellite image)
# flag_tfee = TRUE does not necessarily indicate exceedance of an AQO/CAAQS (or other threshold)
# [data_24hr] value_tfee = pm2.5 concentration set to zero if >= 25 ug/m3 and flag_tfee = TRUE

tfee <- read.csv("data/tfee_log.csv") |>
  filter(station_name == "Prince George Plaza 400") |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8")) |>
  select(date, station_name, event_type, comment)

data_1hr <- data_1hr |>
  left_join(tfee,
            by = c("date", "station_name"),
            relationship = "many-to-many") |>
  mutate(event_type = factor(event_type),
         flag_tfee = case_when(event_type == "TF" ~ TRUE,
                               event_type == "EE" ~ TRUE,
                               .default = FALSE))

data_24hr <- data_24hr |>
  left_join(tfee,
            by = c("date", "station_name"),
            relationship = "many-to-many") |>
  mutate(event_type = factor(event_type),
         flag_tfee = case_when(event_type == "TF" ~ TRUE,
                               event_type == "EE" ~ TRUE,
                               .default = FALSE),
         value_tfee = ifelse(flag_tfee == TRUE & param == "pm25" & value >= 25, NA_real_, value)) |>
  select(station_name, year, month, date, param, value, value_tfee, instrument, unit, validation_status, flag_tfee, event_type, comment)

#------------------------------------------------------
# save data sets: data_1hr, data_24hr, data_1m, data_1y, and data_season
#------------------------------------------------------

save(data_1hr, file = "data/data_1hr.rds")
save(data_24hr, file = "data/data_24hr.rds")
save(data_1m, file = "data/data_1m.rds")
save(data_1y, file = "data/data_1y.rds")
save(data_season, file = "data/data_season.rds")

#------------------------------------------------------
# data capture summaries: month, year and season
#------------------------------------------------------

data_cap_1m <- timeAverage(data_24hr_wide,
                           avg.time = "month",
                           data.thresh = 75,
                           statistic = "data.cap",
                           fill = TRUE,
                           type = "station_name") |>
  mutate_if(is.numeric, round, digits = 1) |>
  select(-Uu,-Vv) |>
  pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param))

data_cap_1y <- timeAverage(data_24hr_wide,
                            avg.time = "year",
                            data.thresh = 75,
                            statistic = "data.cap",
                            fill = TRUE,
                            type = "station_name") |>
  mutate_if(is.numeric, round, digits = 1)|>
  select(-Uu,-Vv) |>
  pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param))


data_cap_season <- timeAverage(data_24hr_wide,
                                avg.time = "season",
                                statistic = "data.cap",
                                fill = TRUE,
                                type = "station_name")|>
  mutate_if(is.numeric, round, digits = 1) |>
  select(-Uu,-Vv)|>
  pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param))

save(data_cap_1m, file = "data/data_cap_1m.rds")
save(data_cap_1y, file = "data/data_cap_1y.rds")
save(data_cap_season, file = "data/data_cap_season.rds")


#------------------------------------------------------
# clean up
#------------------------------------------------------

rm(list = ls(pattern = "wide"))
rm(list = ls(pattern = "meta"))
rm(tfee)




#------------------------------------------------------
# Number of Advisories
#------------------------------------------------------

#Make dataframe of number of advisory days (recorded on LAN) in province,omineca peace region, and PG - don't have 2015 because there is no advisory day data for PG
ADVISORYDAYS <- data.frame(
  Year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
  Province = c(52, 61, 60, 50, 26, 35, 58, 62, 38),
  OminecaPeace = c(27, 29, 29, 30, 7, 13, 34, 22, 27),
  PrinceGeorge = c(4, 3, 6, 14, 3, 2, 1, 8, 16)
)

#Pivot long
ADVISORYDAYS <- pivot_longer(ADVISORYDAYS, cols = c(Province, OminecaPeace, PrinceGeorge),
                             names_to = "Region",
                             values_to = "AdvisoryDays")

save(ADVISORYDAYS, file = "data/Advisorydays.rds")
