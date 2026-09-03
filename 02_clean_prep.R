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

  rename(date_hour_end = date_pst) |> # Hour ending average. For example, 2015-01-01 02:00 represents data collected from 01:01 - 02:00.

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
    instrument = stringr::str_to_lower(instrument),

    # calculate the rolling 8hr average for O3; data completeness requirement is a minimum of 6hr per 8hr average
    # if not O3, return rounded_value
    rounded_value_r8ho3 = case_when(param == "o3" ~
                               zoo::rollapply(rounded_value, width = 8, align = "right", fill = NA, FUN = function(w) {
                                 # count NA values, return NA if fewer than 6 hours available
                                 non_na_count <- sum(!is.na(w))
                                 if(non_na_count >= 6) {
                                   return(round(mean(w, na.rm = TRUE), 1))
                                   } else {
                                     return(NA_real_)
                                     }
                                 }),
                           TRUE ~ rounded_value
                           )
    ) |>

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
        rounded_value_r8ho3,
        unit,
        instrument,
        validation_status
      )

### clean
data_1hr <- data_1hr |>
  # Remove PM2.5 TEOM (keep PM2.5 SHARP): TEOM and SHARP were collocated ~ 2012(?) - 2015
  filter_out(instrument == "pm25_r&p_teom") |>
  # Keep PM10 TEOM to May 6, 2020 (last measurement 0900), then SHARP 5014. There are no periods where the monitors were collocated.
  filter_out(instrument == "pm10_r&p_teom" & date > as.Date("2020-05-06")) |>
  filter_out(instrument == "pm10_5014i" & date <= as.Date("2020-05-06")) |>
  # Drop cases where all values for any given parameter are NA for any given station
  filter_out(all(is.na(raw_value)),
           .by = c(station_name, param),
           .preserve = FALSE
           )

#------------------------------------------------------
# save formatted data
#------------------------------------------------------
saveRDS(data_1hr, file = "data/data_1hr.rds")

#------------------------------------------------------
# clean up
#------------------------------------------------------
rm(data_1hr_original)

#------------------------------------------------------
# calculate and format averaged data frames to long format
#------------------------------------------------------

# data_1hr <- readRDS("data/data_1hr.rds")

### daily average
data_24hr_meta <- data_1hr |> distinct(station_name, date, year, month, param, unit, instrument, validation_status)

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
                           avg.time = "season", # spring = Mar-May; summer = Jun-Aug; autumn = Sep-Nov, winter = Dec-Feb
                           statistic = "mean",
                           fill = TRUE,
                           type = "station_name")

# format averaged data frames to long format and add meta data back
data_24hr <- data_24hr_wide |>
  tidyr::pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") |>
  arrange(param, date) |>
  mutate(date = as.Date(date),
         value = round(value, 1),
         param = factor(param)) |>
  left_join(data_24hr_meta, by = c("station_name", "date", "param")) |>
  # Drop parameters where all values are NA for any given station
  filter_out(all(is.na(value)),
             .by = c(station_name, param),
             .preserve = FALSE)

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
# data/tfee_log.csv is based on an independent review (by G.Roth) and includes:
#  2010 - 2024 for all Omineca-Peace region stations # to 2010-2025 for Plaza 400
#  2010 - 2021 (?) for select Skeena region stations
#  flag_tfee = TRUE indicates evidence of wildfire smoke in the vicinity of the monitor (e.g. satellite image)
#  flag_tfee = TRUE does not necessarily indicate AQO/CAAQS (or other threshold) was exceeded
#  In the data_24hr dataset:
#  removed_tfee = TRUE indicates AQO was exceed when flag_tfee = TRUE;
#  the value_tfee column is set to NA for params = pm2.5 or params = pm10 when removed_tfee = TRUE.

tfee <- read.csv("data/tfee_log.csv") |>
  filter(station_name == "Prince George Plaza 400") |> # Plaza 400 is the only current monitoring site with PM2.5 and PM10
  mutate(date = as.Date(date)) |>
  select(date, station_name, event_type, comment)

data_1hr <- data_1hr |>
  left_join(tfee,
            by = c("date", "station_name"),
            relationship = "many-to-many"
            ) |>
  distinct() |>
  mutate(station_name = factor(station_name),
         event_type = factor(event_type),
         flag_tfee = case_when(event_type == "TF" ~ TRUE,
                               event_type == "EE" ~ TRUE,
                               .default = FALSE)
         ) |>
  # Drop parameters where all values are NA for any given station
  filter_out(all(is.na(raw_value)),
             .by = c(station_name, param),
             .preserve = FALSE)

tfee_pm25 <- data_24hr |>
  left_join(tfee,
            by = c("date", "station_name"),
            relationship = "many-to-many") |>
  distinct() |>
  mutate(station_name = factor(station_name),
         event_type = factor(event_type),
         flag_tfee = case_when(event_type == "TF" ~ TRUE,
                               event_type == "EE" ~ TRUE,
                               .default = FALSE),
         removed_tfee = ifelse(flag_tfee == TRUE & param == "pm25" & round(value, 0) >= 25, TRUE, FALSE),
         value_tfee = ifelse(flag_tfee == TRUE & param == "pm25" & round(value, 0) >= 25, NA_real_, value)) |>
  select(station_name, year, month, date, param, value, value_tfee, instrument, unit, validation_status, flag_tfee, removed_tfee, event_type, comment) |>
  # Drop parameters where all values are NA for any given station
  filter_out(all(is.na(value)),
             .by = c(station_name, param),
             .preserve = FALSE)

# For days where removed_tfee flag is TRUE for PM2.5, also assign TRUE to PM10 and set value_tfee to NA
removed_tfee_days <- tfee_pm25 |>
  filter(removed_tfee == TRUE) |>
  pull(date)

# assign tfee_removed and value_tfee to pm10
data_24hr <- tfee_pm25 |>
  mutate(
    removed_tfee = removed_tfee | (param == "pm10" & date %in% removed_tfee_days),
    value_tfee = if_else(
      param == "pm10" & date %in% removed_tfee_days,
      NA_real_,
      value_tfee
      )
    )

#clean up
rm(tfee_pm25)
rm(removed_tfee_days)


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

#------------------------------------------------------
# combine data with data capture: data_1m, data_1y, data_qtr, and  data_season
#------------------------------------------------------

data_1m <- data_1m |>
  left_join(data_cap_1m) |>
  # Drop parameters where all values are NA for any given station
  filter_out(all(is.na(value)),
             .by = c(station_name, param),
             .preserve = FALSE)

data_qtr <- data_qtr |>
  left_join(data_cap_qtr) |>
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

