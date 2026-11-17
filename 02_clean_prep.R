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
library(lubridate)
library(openair)

data_1hr_original <- readRDS("data/data_1hr_original.rds")

### format
# Rename column names
# Convert character columns to lower case
# Fix date-time columns - errors stem from incorrect assignment of time zone
# Add additional time related columns: year, month, hour
# Drop unnecessary columns

# correct date-time stamp to PST; use this column to recreate date and other date_time objects
tz(data_1hr_original$date_pst) <- "Etc/GMT+8"

data_1hr <- data_1hr_original |>

  # Hour ending average. For example, 2015-01-01 02:00 represents data collected from 01:01 - 02:00.
  rename(date_hour_end = date_pst) |>

  mutate(
    # create time-based columns
    date_hour_begin = date_hour_end - lubridate::hours(1), # Hour beginning average. For example, 2015-01-01 02:00 represents data collected from 02:00 - 2:59.
    date = as.Date(format(date_hour_begin, "%Y-%m-%d", usetz = TRUE)),
    year = year(date),
    month = factor(month(date, label = TRUE, abbr = TRUE), levels = month.abb[1:12]),
    hour = factor(format(date_hour_begin, "%H:%M")),

    station_name = factor(station_name),

    param = stringr::str_to_lower(parameter),
    param = case_when(
           param == "temp_mean" ~ "temp",
           param == "wdir_vect" ~ "wd",
           param == "wspd_sclr" ~ "ws",
           .default = as.character(param)),
    param = factor(param),
    instrument = stringr::str_to_lower(instrument)
    )|>

  select(
    date_hour_begin,
    date_hour_end,
    date,
    year,
    month,
    hour,
    station_name,
    param,
    raw_value,
    rounded_value,
    unit,
    instrument,
    validation_status
    )

# Calculate the rolling 8hr average for O3; data completeness requirement is a minimum of 6hr per 8hr average
o3_8hr_roll <- data_1hr |>
  filter(param == "o3") |>
  mutate(
    param = "o3_rolling_8hr",
    raw_value = zoo::rollapply(
      rounded_value,
      width = 8,
      align = "right",
      fill = NA,
      FUN = function(w) {
        # count NA values, return NA if fewer than 6 hours available
        non_na_count <- sum(!is.na(w))
        if (non_na_count >= 6) {
          return(round(mean(w, na.rm = TRUE), 1))
        } else {
          return(NA_real_)
        }
      }
    ),
    rounded_value = raw_value
  )

# add o3 8-hour rolling average to data_1hr
data_1hr <- data_1hr |>
  bind_rows(o3_8hr_roll) |>
  mutate(param = factor(param))

#------------------------------------------------------
# clean up inconsistencies
#------------------------------------------------------
data_1hr <- data_1hr |>
  # Remove PM2.5 TEOM (keep PM2.5 SHARP): TEOM and SHARP were collocated ~ 2012(?) - 2015
  filter_out(instrument == "pm25_r&p_teom") |>
  # Keep PM10 TEOM to May 6, 2020 09:00 (last measurement), then SHARP 5014. There are no periods when the monitors were collocated.
  filter_out(instrument == "pm10_r&p_teom" & date > as.Date("2020-05-06")) |>
  filter_out(instrument == "pm10_5014i" & date <= as.Date("2020-05-06")) |>
  # Drop cases where all values for any given parameter are NA for any given station
  filter_out(all(is.na(raw_value)),
           .by = c(station_name, param),
           .preserve = FALSE
           )

#------------------------------------------------------
# read tfee data
#------------------------------------------------------

### notes:
# The envair package considers tfee from 2017 onwards.
# The file data/tfee_log.csv is based on an independent review of tfee and includes:
#  2010 - 2024 for all Omineca-Peace region stations; 2010-2025 for Plaza 400
#  2010 - 2021 (?) for select Skeena region stations

# Event types
#  EE = exceptional event
#  TF = transboundary flow (typically wildfire smoke)
#  local = local emissions

# An assigned event_type does not necessarily indicate an AQO was exceeded.
# For example, there is evidence (e.g. satellite imagery) to suggest wildfire smoke impacted the monitor but
# the 24-hour PM2.5 average was 22 ug/m3.

tfee <- read.csv("data/tfee_log.csv") |>
  filter(station_name == "Prince George Plaza 400") |> # Plaza 400 is the only current monitoring site with PM2.5 and PM10
  mutate(date = as.Date(date)) |>
  select(date, station_name, event_type, comment)

#------------------------------------------------------
# calculate and format averaged data frames
#------------------------------------------------------

#data_1hr <- readRDS("data/data_1hr.rds")

# create data frames

### daily average
data_24hr_meta <- data_1hr |>
  distinct(station_name,
           date, year,
           month,
           param,
           unit,
           instrument,
           validation_status)

data_1hr_wide <- data_1hr |>
  # keep air quality parameters only
  filter(param %in% c("pm25", "pm10", "so2", "trs", "no2", "o3")) |>
  select(station_name, date_hour_begin, param, raw_value) |>
  rename(date = date_hour_begin) |>  # timeAverage() requires date-time column to be named 'date'
  tidyr::pivot_wider(names_from = param, values_from = raw_value)

data_24hr_wide <- timeAverage(data_1hr_wide,
                         avg.time = "day",
                         data.thresh = 75,
                         statistic = "mean",
                         type = "station_name",
                         fill = TRUE
                         )

### monthly, annual, calendar quarter and seasonal averages
data_1m_wide <- timeAverage(data_24hr_wide,
                          avg.time = "month",
                          data.thresh = 75,
                          statistic = "mean",
                          fill = TRUE,
                          type = "station_name"
                          )

data_1y_wide <- timeAverage(data_24hr_wide,
                          avg.time = "year",
                          data.thresh = 75,
                          statistic = "mean",
                          fill = TRUE,
                          type = "station_name"
                          )

data_qtr_wide <- timeAverage(data_24hr_wide,
                             avg.time = "quarter",
                             statistic = "mean",
                             fill = TRUE,
                             type = "station_name"
                             )

data_season_wide <- timeAverage(data_24hr_wide,
                           avg.time = "season", # spring = Mar-May; summer = Jun-Aug; autumn = Sep-Nov, winter = Dec-Feb
                           statistic = "mean",
                           fill = TRUE,
                           type = "station_name"
                           )

#### format data frames to long-format, add tf_ee

data_24hr <- data_24hr_wide |>

  # add tf_ee
  left_join(tfee,
            by = c("date", "station_name")) |>

  # add flags
  mutate(
    # indicates pm25 exceeds 25 ug/m3 and TF or EE event documented
    flag_tfee = ifelse(
      (event_type == "TF" | event_type == "EE") &
        (round(pm25, 0) >= 25 | is.na(pm25)),  # assigning flag_tfee = TRUE when pm2.5 data are NA but there is evidence of wildfire smoke (avoids missed flags for PM10)
      TRUE,
      FALSE
    ),

    # flag days pm25 > 50 ug/mg (used for interpreting PM10 exceedances)
    # flagging 40 ug/m3 to account for monitor accuracy. For example if PM2.5 is 48.8 ug/m3, it should still be flagged as a PM2.5 driven event
    flag_pm25_above_40 = ifelse(round(pm25, 0) >= 40, TRUE, FALSE)
    ) |>

  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date),
         value = round(value, 1),
         param = factor(param)) |>

  # add an adjusted value column that assigns NA when
  # PM concentrations exceed AQO thresholds
  # due to a tfee_event (pm25) or
  # driven by pm25 (pm10)
  # Any exceedances that remain are locally driven and,
  # for pm10, driven by the coarse fraction.
  mutate(value_adj = case_when(
    param == "pm25" & flag_tfee == TRUE ~ NA_real_,
    param == "pm10" & (flag_tfee == TRUE | flag_pm25_above_40 == TRUE) ~ NA_real_,
    .default =  value
    )
  ) |>

  # add meta data and format column order
  left_join(data_24hr_meta, by = c("station_name", "date", "param")) |>
  select(station_name,
         date,
         year,
         month,
         param,
         value,
         value_adj,
         unit,
         event_type,
         comment,
         flag_tfee,
         flag_pm25_above_40,
         instrument,
         validation_status)

data_1m <- data_1m_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date, tz = "Etc/GMT+8"),
         value = round(value, 1),
         param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

data_1y <- data_1y_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

data_qtr <- data_qtr_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

data_season <- data_season_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date)|>
  mutate(param = factor(param),
         month = format(date, "%m"),
         met_season = case_when(
           month %in% 9:11 ~ "Fall (Sep - Nov)",
           month %in% c(12, 1,2) ~ "Winter(Dec - Feb)",
           month %in% 3:5 ~ "Spring (Mar - May)",
           TRUE ~ "Summer (Jun - Aug)"
         ),
         met_season = factor(met_season,
                             levels = c("Winter(Dec - Feb)",
                                        "Fall (Sep - Nov)",
                                        "Summer (Jun - Aug)",
                                        "Spring (Mar - May)")
         ))|>
  select(-month) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param"))

# add tfee_flags to data_1hr
tfee_meta <- data_24hr |>
  select(station_name,
         date,
         event_type,
         comment,
         flag_tfee,
         flag_pm25_above_40) |>
  distinct()

data_1hr <- data_1hr |>
  left_join(tfee_meta,
            by = c("date", "station_name"),
            relationship = "many-to-many"
            ) |>
  mutate(station_name = factor(station_name),
         event_type = factor(event_type)
         )

#------------------------------------------------------
# data capture summaries: month, year, calendar quarter, and season
#------------------------------------------------------

data_cap_1m <- timeAverage(data_24hr_wide,
                           avg.time = "month",
                           statistic = "data.cap",
                           fill = TRUE,
                           type = "station_name") |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param),
         year = year(date),
         month = factor(month(date, label = TRUE, abbr = TRUE), levels = month.abb[]),
         data_cap_percent = round(data_cap_percent * 100, digits = 1)) |>
  select(-date)

data_cap_1y <- timeAverage(data_24hr_wide,
                            avg.time = "year",
                            statistic = "data.cap",
                            fill = TRUE,
                            type = "station_name"
                           )|>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param),
         year = year(date),
         data_cap_percent = round(data_cap_percent * 100, digits = 1)) |>
  select(-date)

data_cap_qtr <- timeAverage(data_24hr_wide,
                            avg.time = "quarter",
                            statistic = "data.cap",
                            fill = TRUE,
                            type = "station_name"
                            )|>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param),
         year = year(date),
         quarter = quarter(date),
         data_cap_percent = round(data_cap_percent * 100, digits = 1)
         ) |>
  select(-date)

data_cap_qtr_wide <- data_cap_qtr |>
  tidyr::pivot_wider(names_from = "quarter",
                     values_from = data_cap_percent)

data_cap_season <- timeAverage(data_24hr_wide,
                                avg.time = "season",
                                statistic = "data.cap",
                                fill = TRUE,
                                type = "station_name"
                               )|>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "data_cap_percent") |>
  mutate(param = factor(param),
         year = year(date),
         data_cap_percent = round(data_cap_percent * 100, digits = 1)
         )|>
  select(-date)

 # saveRDS(data_cap_1m, file = "data/data_cap_1m.rds")
 # saveRDS(data_cap_1y, file = "data/data_cap_1y.rds")
 # saveRDS(data_cap_qtr, file = "data/data_qtr_season.rds")
 # saveRDS(data_cap_season, file = "data/data_cap_season.rds")

#------------------------------------------------------
# add data capture to data sets (where applicable):
#------------------------------------------------------

data_1m <- data_1m |>
  left_join(data_cap_1m) |>
  # Drop parameters where all values are NA for any given station
  filter_out(all(is.na(value)),
             .by = c(station_name, param),
             .preserve = FALSE)

data_qtr <- data_qtr |>
  left_join(data_cap_qtr,
            relationship = "many-to-many") |>
  # Drop parameters where all values are NA for any given station
  filter_out(all(is.na(value)),
             .by = c(station_name, param),
             .preserve = FALSE)

data_season <- data_season |>
  left_join(data_cap_season,
            relationship = "many-to-many"
            ) |>
  # Drop parameters where all values are NA for any given station
  filter_out(all(is.na(value)),
             .by = c(station_name, param),
             .preserve = FALSE)

data_1y <- data_1y |>
  left_join(data_cap_1y) |>
  left_join(data_cap_qtr_wide) |>
  rename(annual_data_capture = data_cap_percent,
         q1_data_capture = `1`,
         q2_data_capture = `2`,
         q3_data_capture = `3`,
         q4_data_capture = `4`) |>
  mutate(data_cap_met = ifelse(
    annual_data_capture >= 75 &
      pmin(q1_data_capture, q2_data_capture, q3_data_capture, q4_data_capture)>= 60,
    TRUE,
    FALSE)) |>
  # Drop parameters where all values are NA for any given station
  filter_out(all(is.na(value)),
             .by = c(station_name, param),
             .preserve = FALSE)

#------------------------------------------------------
# save data sets (include tf_ee and data capture where applicable)
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

rm(data_1hr_original)
rm(tfee)
rm(o3_8hr_roll)
rm(list = ls(pattern = "wide"))
rm(list = ls(pattern = "meta"))
rm(list = ls(pattern = "data_cap"))
