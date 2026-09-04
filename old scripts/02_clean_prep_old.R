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

data_1hr_original <- readRDS("data/data_1hr_original.rds")

### format
# Rename column names
# Convert character columns to lower case
# Add additional time related columns: year, month, hour
# Drop unnecessary columns

data_1hr <- data_1hr_original |>
  rename(date_hour_begin = datetime, # Hour beginning average. For example, 2015-01-01 02:00 represents data collected from 02:00 - 02:59.
         date_hour_end = date_pst) |> # Hour ending average. For example, 2015-01-01 02:00 represents data collected from 01:01 - 02:00.
  mutate(param = stringr::str_to_lower(parameter),
         param = case_when(
           param == "temp_mean" ~ "temp",
           param == "wdir_vect" ~ "wd",
           param == "wspd_sclr" ~ "ws",
           .default = as.character(param)),
         param = factor(param),
         instrument = stringr::str_to_lower(instrument),
         year = year(date_hour_begin),
         month = factor(month(date_hour_begin, label = TRUE, abbr = TRUE), levels = month.abb[1:12]),
         hour = factor(time, levels = paste0(0:23, ":00")),
         # calculate the rolling 8hr average for O3 only
         rolling8hrO3 = if_else(param == "o3",
                                zoo::rollapply(rounded_value, width = 8, FUN = mean, align = "right", fill = NA),
                                rounded_value)
         ) |>
  select(date_hour_begin,
         date_hour_end,
         date,
         year,
         month,
         hour,
         station_name,
         param,
         raw_value,
         rounded_value,
         rolling8hrO3,
         unit,
         instrument,
         validation_status
         )

### clean
data_1hr <- data_1hr |>
  filter(instrument != "pm25_r&p_teom") |>  # Remove PM2.5 TEOM (keep PM2.5 SHARP): TEOM and SHARP were collocated ~ 2012(?) - 2015
  group_by(param, date_hour_begin, station_name) |> # Keep one of PM10 TEOM or PM10 SHARP, which ever is not NA. The PM10 SHARP replaced the PM10 TEOM in May 2020. There are no periods where the monitors were collocated.
  slice(which.max(!is.na(raw_value))) |>
  ungroup()


### format
# add date_hour_end, date, time, year and month
# convert to factor: time, year, and month
# reorder columns
# data_1hr <- data_1hr |>
#   mutate(date_hour_end = date_hour_begin + hours(1),
#          date = as.Date(date_hour_begin, tz = "Etc/GMT+8"),
#          time = factor(paste0(hour(date_hour_begin), ":00"), levels = paste0(0:23, ":00")),
#          year = year(date_hour_begin),
#          month = factor(month(date_hour_begin, label = TRUE, abbr = TRUE), levels = month.abb[1:12]),
#          param = factor(param),
#           |>
#   select(station_name,
#          date_hour_end,
#          date_hour_begin,
#          date,
#          time,
#          year,
#          month,
#          param,
#          raw_value,
#          rounded_value,
#          rolling8hrO3,
#          unit,
#          instrument,
#          validation_status)

  #------------------------------------------------------
  # save formatted data
  #------------------------------------------------------
  saveRDS(data_1hr, file = "data/data_1hr.rds")

  #------------------------------------------------------
  # clean up
  #------------------------------------------------------
  rm(ts_start)
  rm(ts_end)
  rm(hour_ts)
  rm(nhours)
  rm(nparams)
  rm(expected_nrows)
  rm(data_1hr_o)


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

### monthly, annual, calendar quarter and seasonal averages
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

data_qtr_wide <- timeAverage(data_24hr_wide,
                             avg.time = "quarter",
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
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         value = round(value, 1),
         param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

data_1m <- data_1m_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         value = round(value, 1),
         param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param")) |>
  select(-date)

data_1y <- data_1y_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param")) |>
  mutate(year = year(date)) |>
  select(-date, -month)

data_qtr <- data_qtr_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param")) |>
  mutate(quarter = quarter(date)) |>
  select(-date, -month)

data_season <- data_season_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param")) |>
  select(-date)

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
         value_tfee = ifelse(flag_tfee == TRUE & param == "pm25" & round(value, 0) >= 25, NA_real_, value)) |>
  select(station_name, year, month, date, param, value, value_tfee, instrument, unit, validation_status, flag_tfee, event_type, comment)


#------------------------------------------------------
# data capture summaries: month, year, calendar quarter, and season
#------------------------------------------------------

data_cap_1m <- timeAverage(data_24hr_wide,
                           avg.time = "month",
                           statistic = "data.cap",
                           fill = TRUE,
                           type = "station_name") |>
  mutate_if(is.numeric, round, digits = 1) |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param),
         year = year(date),
         month = factor(month(date, label = TRUE, abbr = TRUE), levels = month.abb[])) |>
  select(-date)

data_cap_1y <- timeAverage(data_24hr_wide,
                            avg.time = "year",
                            statistic = "data.cap",
                            fill = TRUE,
                            type = "station_name") |>
  mutate_if(is.numeric, round, digits = 1)|>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param),
         year = year(date)) |>
  select(-date)

data_cap_qtr <- timeAverage(data_24hr_wide,
                            avg.time = "quarter",
                            statistic = "data.cap",
                            fill = TRUE,
                            type = "station_name")|>
  mutate_if(is.numeric, round, digits = 1) |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param),
         year = year(date),
         quarter = quarter(date)) |>
  select(-date)

data_cap_qtr_wide <- data_cap_qtr |>
  tidyr::pivot_wider(names_from = "quarter",
                     values_from = data_cap_percent
                     )

data_cap_season <- timeAverage(data_24hr_wide,
                                avg.time = "season",
                                statistic = "data.cap",
                                fill = TRUE,
                                type = "station_name")|>
  mutate_if(is.numeric, round, digits = 1) |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param),
         year = year(date)) |>
  select(-date)

#------------------------------------------------------
# combine data with data capture: data_1m, data_1y, data_qtr, and  data_season
#------------------------------------------------------

data_1m <- data_1m |> left_join(data_cap_1m)

data_qtr <- data_qtr |> left_join(data_cap_qtr)

data_season <- data_qtr |> left_join(data_cap_season)

data_1y <- data_1y |>
  left_join(data_cap_1y) |>
  left_join(data_cap_qtr_wide) |>
  rename(annual_data_capture = data_cap_percent,
         q1_data_capture = `1`,
         q2_data_capture = `2`,
         q3_data_capture = `3`,
         q4_data_capture = `4`)

#------------------------------------------------------
# save data sets (with data capture where applicable):
#------------------------------------------------------

saveRDS(data_1hr, file = "data/data_1hr.rds")
saveRDS(data_24hr, file = "data/data_24hr.rds")
saveRDS(data_1m, file = "data/data_1m.rds")
saveRDS(data_1y, file = "data/data_1y.rds")
saveRDS(data_qtr, file = "data/data_qtr.rds")
saveRDS(data_season, file = "data/data_season.rds")

#------------------------------------------------------
# clean up
#------------------------------------------------------

rm(list = ls(pattern = "wide"))
rm(list = ls(pattern = "meta"))
rm(list = ls(pattern = "data_cap"))
rm(tfee)

#------------------------------------------------------
# Number of Advisories
#------------------------------------------------------

#Make dataframe of number of advisory days (recorded on LAN) in province,omineca peace region, and PG - don't have 2015 because there is no advisory day data for PG
ADVISORYDAYS <- data.frame(
  Year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
  Province = c(52, 61, 60, 50, 26, 35, 58, 62, 38),
  OminecaPeace = c(27, 29, 29, 30, 7, 13, 34, 22, 27),
  PrinceGeorge = c(4, 3, 6, 14, 3, 2, 1, 8, 16),
  PrinceGeorgepm25 = c(0, 2, 6, 7, 2, 1, 0, 1, 6),
  PrinceGeorgepm10 = c(4, 1, 0, 7, 1, 1, 1, 7, 10)
)

#Pivot long
ADVISORYDAYS <- tidyr::pivot_longer(ADVISORYDAYS, cols = c(Province, OminecaPeace, PrinceGeorge, PrinceGeorgepm25, PrinceGeorgepm10),
                             names_to = "Region",
                             values_to = "AdvisoryDays")

save(ADVISORYDAYS, file = "data/Advisorydays.rds")

#-------------------------------------------------------------------------
#Load in percent_above_below_threshold excel file
#-------------------------------------------------------------------------
library(readxl)

percent_above_below_threshold <- read_xlsx("C:/R_working_directory/pg-aqmp/data/percent_above_or_below_threshold_pm25_no2_o3_so2.xlsx")

save(percent_above_below_threshold, file = "data/percent_above_below_threshold.rds")

#--------------------------------------------------------------------------
#Load in daily_exceedances excel file
#-------------------------------------------------------------------------

daily_exceedance_pm10_trs <- read_xlsx("C:/R_working_directory/pg-aqmp/data/number_daily_exceedances_pm10_trs.xlsx")

save(daily_exceedance_pm10_trs, file = "data/daily_exceedance_pm10_trs.rds")
