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

# Packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

#Make a figure_path
figure_path <- file.path("C:", "R_working_directory", "pg-aqmp", "Figures")

#----------------------------------------------------------------------
#PM2.5 visualisation
#----------------------------------------------------------------------

#Plot PM25 1m data capture
DATACAP1MPLOT <- data_cap_1m %>%
  filter(param == "pm25")%>%
  ggplot(aes(x = date, y = data_cap_percent)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_hline(yintercept = 60, col = "red", linetype = "dotted") +
  labs(x = "Month", y = "% data capture") +
  scale_x_datetime(breaks = "1 year", # Adjust frequency as needed
    labels = scales::date_format("%Y")) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 0),
    legend.position = "none")
DATACAP1MPLOT
ggsave("data_cap_1m_plot.png",
       plot = DATACAP1MPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot PM25 season data capture
DATACAPSEASONPLOT <- data_cap_season %>%
  filter(param == "pm25")%>%
  ggplot(aes(x = date, y = data_cap_percent)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_hline(yintercept = 60, col = "red", linetype = "dotted") +
  geom_text(aes(label = season, colour = "green"),  # Adding the season labels from your column
            position = position_dodge(width = 0.9),
            angle = 90) +  # Adjust label position above bars
  labs(x = "Season", y = "% data capture") +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 0),
    legend.position = "none")
DATACAPSEASONPLOT

ggsave("data_cap_season_plot.png",
       plot = DATACAPSEASONPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot PM25 1year data capture
DATACAP1YPLOT <- data_cap_1y %>%
  filter(param == "pm25")%>%
  ggplot(aes(x = date, y = data_cap_percent)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_hline(yintercept = 60, col = "red", linetype = "dotted") +
  labs(x = "Year", y = "% data capture") +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 0),
    legend.position = "none")
DATACAP1YPLOT
ggsave("data_cap_1y_plot.png",
       plot = DATACAP1YPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot 24hr PM2.5 average
DATA24HRPLOT <- data_24hr %>%
  filter(param == "pm25") %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_hline(yintercept = 25, colour = "red", linetype = "dashed") +
  labs(x = "Year", y = expression(paste("24-hr average ", PM[2.5]," (",mu,"g/",m^3,")"))) +
  scale_x_date(breaks = "1 year", labels = scales::date_format("%Y"))
DATA24HRPLOT
ggsave("data_24hr_plot.png",
       plot = DATA24HRPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot 1m PM2.5 average
DATA1MPLOT <- data_1m %>%
  filter(param == "pm25") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  labs(x = "Month", y = expression(paste("Monthly average ", PM[2.5]," (",mu,"g/",m^3,")")))
DATA1MPLOT
ggsave("data_1m_plot.png",
       plot = DATA1MPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot seasonal PM2.5 average
DATASEASONPLOT <- data_season %>%
  filter(param == "pm25") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  labs(x = "Year", y = expression(paste("Seasonal average ", PM[2.5]," (",mu,"g/",m^3,")")))
DATASEASONPLOT
ggsave("data_season_plot.png",
       plot = DATASEASONPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot yearly PM2.5 average with 3-year annual CAAQS standard for reference (8.8ug/m3)
DATA1YPLOT <- data_1y %>%
  filter(param == "pm25") %>%
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  geom_hline(yintercept = 8.8, col = "red", linetype = "dotted") +
  labs(x = "Year", y = expression(paste("Annual average ", PM[2.5]," (",mu,"g/",m^3,")"))) +
  scale_x_continuous(
    breaks = seq(2015, 2024, by = 1),  # This ensures ticks for every year
    labels = as.character(seq(2015, 2024, by = 1))  # Format tick labels as years
  )
DATA1YPLOT
ggsave("data_1y_plot.png",
       plot = DATA1YPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#-----------------------------------------------------------------------
# PM2.5 openair plot
#-----------------------------------------------------------------------
PM25SEASONPLOT <- data_1hr %>%
  select(-date) %>%
  rename(date = date_hour_begin) %>%
  filter(param == "pm25",
         flag_tfee == "FALSE") %>% #get rid of wildfire influence
  timeVariation(pollutant = "rounded_value",
                group = "param",
                ylab = expression(paste(PM[2.5]," (",mu,"g/",m^3,")")),
                type = "season",
                plot = FALSE)

# Use a temporary device to save the plot
png(filename = "Figures/PGPM25Seasonaldiurnalv2.png", width = 2500, height = 2000, res = 300)

plot(PM25SEASONPLOT, subset = "hour", na.rm = TRUE,
     main = expression(paste("Average ", PM[2.5], " concentrations between 2015 to 2024")),
     legend = FALSE)

dev.off()

#------------------------------------------------------------------------
#ADVISORY DAYS
#------------------------------------------------------------------------

#Plot advisory days
PROVINCE_PG_OMINECA_ADVISORY_PLOT <- ADVISORYDAYS %>%
  filter(Region %in% c("OminecaPeace", "PrinceGeorge", "Province")) %>%
  ggplot(aes(x = Year, y = AdvisoryDays, fill = Region)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(
    values = c("OminecaPeace" = "lightblue", "PrinceGeorge" = "salmon", "Province" = "seagreen3"), # Custom colors
    labels = c("Omineca Peace", "Prince George", "Province")  # Custom legend labels
  ) +
  labs(title = "Number of Advisory Days",
       x = "Year",
       y = "Number of Advisory Days",
       fill = " ") +
  scale_x_continuous(breaks = ADVISORYDAYS$Year)

PROVINCE_PG_OMINECA_ADVISORY_PLOT
ggsave("PROVINCE_PG_OMINECA_ADVISORY_PLOT.png",
       plot = PROVINCE_PG_OMINECA_ADVISORY_PLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot PM2.5 and PM10 advisories in PG
PG_PM_ADVISORY_PLOT <- ADVISORYDAYS %>%
  filter(Region %in% c("PrinceGeorgepm25", "PrinceGeorgepm10")) %>%
  ggplot(aes(x = Year, y = AdvisoryDays, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = c("PrinceGeorgepm25" = "lightblue", "PrinceGeorgepm10" = "salmon"), # Custom colors
    labels = c(expression(PM[10]), expression(PM[2.5]))  # Custom legend labels
  ) +
  labs(title = "Number of Advisory Days",
       x = "Year",
       y = "Number of Advisory Days",
       fill = " ") +
  scale_x_continuous(breaks = ADVISORYDAYS$Year)

PG_PM_ADVISORY_PLOT

ggsave("PG_PM_ADVISORY_PLOT.png",
       plot = PG_PM_ADVISORY_PLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot advisory days Province and PG only
PROVINCE_PG_ADVISORY_PLOT <- ADVISORYDAYS %>%
  filter(Region %in% c("PrinceGeorge", "Province")) %>%
  ggplot(aes(x = Year, y = AdvisoryDays, fill = Region)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(
    values = c("PrinceGeorge" = "salmon", "Province" = "seagreen3"), # Custom colors
    labels = c("Prince George", "Province")  # Custom legend labels
  ) +
  labs(title = expression(paste("Total number of Advisory Days (for ", PM[10], " and ", PM[2.5], ")")),
       x = "Year",
       y = "Number of Advisory Days",
       fill = " ") +
  scale_x_continuous(breaks = ADVISORYDAYS$Year)

PROVINCE_PG_ADVISORY_PLOT

ggsave("PROVINCE_PG_ADVISORY_PLOT.png",
       plot = PROVINCE_PG_ADVISORY_PLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)
#------------------------------------------------------------------------
#TRS visualisation
#------------------------------------------------------------------------
#Plot 24hr trs average
DATA24HRTRSPLOT <- data_24hr %>%
  filter(param == "trs") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  geom_hline(yintercept = 3, colour = "red", linetype = "dotted") +
  labs(x = "Year", y = expression(paste("24-hr average TRS (ppb)"))) +
  scale_x_date(breaks = "1 year", labels = scales::date_format("%Y"))
DATA24HRTRSPLOT
ggsave("data_24hr_trs_plot.png",
       plot = DATA24HRTRSPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot 1m trs average
DATA1MTRSPLOT <- data_1m %>%
  filter(param == "trs") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  labs(x = "Month", y = expression(paste("Monthly average TRS (ppb)")))
DATA1MTRSPLOT
ggsave("data_1m_trs_plot.png",
       plot = DATA1MTRSPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot seasonal trs average
DATASEASONTRSPLOT <- data_season %>%
  filter(param == "trs") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  labs(x = "Year", y = expression(paste("Seasonal average TRS (ppb)")))
DATASEASONPLOT
ggsave("data_season_trs_plot.png",
       plot = DATASEASONTRSPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#Plot yearly TRS average
DATA1YTRSPLOT <- data_1y %>%
  filter(param == "trs") %>%
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  labs(x = "Year", y = expression(paste("Annual average TRS (ppb)"))) +
  scale_x_continuous(
    breaks = seq(2015, 2024, by = 1),  # This ensures ticks for every year
    labels = as.character(seq(2015, 2024, by = 1))  # Format tick labels as years
  )
DATA1YTRSPLOT
ggsave("data_1y_trs_plot.png",
       plot = DATA1YTRSPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#-----------------------------------------------------------------------
# Number of days PM25, PM10 and TRS, and NO2, SO2 and O3 above AQO or PCO
#-----------------------------------------------------------------------
#Calculate percentage of days in each year the 24hr values were over the AQO or PCO for pm25,pm10 and trs
PERCENT_DAYS_ABOVE_AQO_PM_TRS <- data_24hr %>%
  filter(param %in% c("pm25", "pm10", "trs")) %>%
  mutate(
    # Create a new column for whether the value is above the AQO
    above_aqo = case_when(
      param == "pm25" & value > 25 ~ 1,  # AQO for PM2.5 is 25 ug/m³
      param == "pm10" & value > 50 ~ 1,  # AQO for PM10 is 50 ug/m³
      param == "trs" & value > 2 ~ 1,    # PCO TRS is 2 ppb
      TRUE ~ 0  # 0 if it's not above the AQO
    )
  ) %>%
  # Extract the year from the date column in the data_cap_1y dataframe
  left_join(
    data_cap_1y %>%
      mutate(year_from_date = as.integer(format(as.Date(date), "%Y"))),  # Extract year from date
    by = c("year" = "year_from_date", "param")  # Join on the extracted year and param
  ) %>%
  group_by(year, param) %>%
  summarise(
    total_days = sum(!is.na(value)),  # Total number of days in the year
    days_above_aqo = sum(above_aqo),  # Number of days above AQO
    percent_above_aqo = ifelse(
      sum(!is.na(value) & !is.na(data_cap_percent) & data_cap_percent >= 75) > 0,
      (days_above_aqo / total_days) * 100,  # Calculate percentage if data capture is sufficient
      NA_real_  # Set to NA if data capture is below 75% or missing
    )
  ) %>%
  ungroup()

#calculate percentage of hours in each year the 1hr values were over AQO for no2, so2 and o3
PERCENT_DAYS_ABOVE_AQO_NO2_SO2_O3 <- data_1hr %>%
  filter(param %in% c("no2", "so2", "o3")) %>%
  mutate(
    # Create a new column for whether the value is above the AQO
    above_aqo = case_when(
      param == "no2" & rounded_value > 60 ~ 1,  # AQO for no2 is 60ppb
      param == "so2" & rounded_value > 75 ~ 1,  # AQO for so2 is 75ppb
      param == "o3" & rolling8hrO3 > 62 ~ 1,    # AQO for o3 is 62ppb
      TRUE ~ 0  # 0 if it's not above the AQO
    )
  ) %>%
  # Extract the year from the date column in the data_cap_1y dataframe
  left_join(
    data_cap_1y %>%
      mutate(year_from_date = as.integer(format(as.Date(date), "%Y"))),  # Extract year from date
    by = c("year" = "year_from_date", "param")  # Join on the extracted year and param
  ) %>%
  group_by(year, param) %>%
  summarise(
    total_hours = sum(!is.na(rounded_value)),  # Total number of days in the year
    hours_above_aqo = sum(above_aqo),  # Number of days above AQO
    percent_above_aqo = ifelse(
      sum(!is.na(rounded_value) & !is.na(data_cap_percent) & data_cap_percent >= 75) > 0,
      (hours_above_aqo / total_hours) * 100,  # Calculate percentage if data capture is sufficient
      NA_real_)  # Set to NA if data capture is below 75% or missing
  ) %>%
  ungroup()

#JOIN PERCENT_DAYS_ABOVE_AQO_PM_TRS AND PERCENT_DAYS_ABOVE_AQO_NO2_SO2_O3
PERCENT_DAYS_ABOVE_AQO_JOINED <- PERCENT_DAYS_ABOVE_AQO_PM_TRS %>%
  full_join(PERCENT_DAYS_ABOVE_AQO_NO2_SO2_O3, by = c("year", "param", "percent_above_aqo"))

# Ensure the param factor levels match the desired order
PERCENT_DAYS_ABOVE_AQO_JOINED$param <- factor(PERCENT_DAYS_ABOVE_AQO_JOINED$param,
                                              levels = c("pm25", "pm10", "trs", "no2", "so2", "o3"))

#Plot percent of days in each year the conc for each pollutant was above AQO or PCO
PERCENTABOVEAQOPLOT <- ggplot(PERCENT_DAYS_ABOVE_AQO_JOINED, aes(x = year, y = percent_above_aqo, colour = param)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = PERCENT_DAYS_ABOVE_AQO_JOINED$year) + # Specify breaks to show each year as a tick
  labs(x = "Year", y = "Percent days contaminant above threshold", colour = "Parameter") +
  scale_color_manual(values = c("pm25" = "blue", "pm10" = "salmon", "trs" = "seagreen", "no2" = "red", "so2" = "darkgoldenrod2", "o3" = "purple"),
                     labels = c(expression(PM[2.5]), expression(PM[10]), "TRS", expression(NO[2]), expression(SO[2]), expression(O[3])))

PERCENTABOVEAQOPLOT

ggsave("percent_above_aqo_plot.png",
       plot = PERCENTABOVEAQOPLOT,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#------------------------------------------------------------------------
#Percent above or below threshold
#------------------------------------------------------------------------

PERCENT_THRESHOLD <-percent_above_below_threshold %>%
  filter(year >= 2015, !(param == "pm25" & objective_type %in% c("annual_caaqs", "24hr_caaqs"))) %>%
  mutate(
    param_obj_type = factor(paste(param, objective_type, sep = "_"), levels = c("pm25_annual_aqo", "pm25_annual_caaqs", "pm25_24hr_prov_aqo", "pm25_24hr_caaqs", "no2_annual_caaqs", "no2_1hr_caaqs", "so2_annual_caaqs", "so2_1hr_caaqs", "o3_8hr_caaqs"))
    ) %>%
  ggplot(aes(x = year, y = percent_above_below_threshold, colour = param_obj_type)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  xlim(2015, 2024) +
  scale_x_continuous(
    breaks = 2015:2024,  # Show ticks for each year
    labels = as.character(2015:2024)  # Optionally convert the ticks to characters for display
  ) +
  labs(
    x = "Year",
    y = "Percent above or below threshold",
    colour = "Pollutant"
  ) +
  scale_color_manual(
    values = c(
      "pm25_annual_aqo" = "deepskyblue4",
      "pm25_24hr_prov_aqo" = "deepskyblue3",
      "no2_annual_caaqs" = "darkgreen",
      "no2_1hr_caaqs" = "seagreen",
      "so2_annual_caaqs" = "yellow3",
      "so2_1hr_caaqs" = "goldenrod",
      "o3_8hr_caaqs" = "purple"),
    labels = c(
      expression(PM[2.5] ~ " - Annual"),
      expression(PM[2.5] ~ " - 24-hr"),
      expression(NO[2] ~ " - Annual"),
      expression(NO[2] ~ " - 1-hr"),
      expression(SO[2] ~ " - Annual"),
      expression(SO[2] ~ " - 1-hr"),
      expression(O[3] ~ " - 8-hr")
    )
  )


PERCENT_THRESHOLD

ggsave("percent_threshold.png",
       plot = PERCENT_THRESHOLD,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#-------------------------------------------------------------------------
# bar plot of number of daily exceedances each year for pm10 and trs
#--------------------------------------------------------------------------

DAILY_EXCEEDANCE_PM10_TRS <- daily_exceedance_pm10_trs %>%
  filter(type_exceed == "day") %>%
  ggplot(aes(x = year, y = value, fill = param)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +  # Adjust the vertical position of the text
  labs(x = "Year", y = "Number of daily exceedances per year", fill = "Parameter") +
  scale_fill_manual(values = c("pm10" = "blue", "trs" = "seagreen"),
                    labels = c(expression(PM[10]), "TRS"))  # Adjust this as needed

DAILY_EXCEEDANCE_PM10_TRS

ggsave("daily_exceedance_pm10_trs.png",
       plot = DAILY_EXCEEDANCE_PM10_TRS,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#-----------------------------------------------------------------------------
# bar plot of TRS #days above 24hr PCO and # hours above 1hr PCO
#-----------------------------------------------------------------------------

DAILY_HRLY_EXCEEDANCE_TRS <- daily_exceedance_pm10_trs %>%
  filter(param == "trs") %>%
  ggplot(aes(x = year, y = value, fill = type_exceed)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +  # Adjust the vertical position of the text
  labs(x = "Year", y = "Number of exceedances per year", fill = "Metric") +
  scale_fill_manual(values = c("day" = "blue", "hr" = "seagreen"),
                    labels = c("24-hr", "1-hr"))  # Adjust this as needed

DAILY_HRLY_EXCEEDANCE_TRS

ggsave("daily_hrly_exceedance_trs.png",
       plot = DAILY_HRLY_EXCEEDANCE_TRS,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

#-------------------------------------------------------------------------------
# plot of pm10  exceedances
#-------------------------------------------------------------------------------

DAILY_EXCEEDANCE_PM10 <- daily_exceedance_pm10_trs %>%
  filter(param == "pm10", type_exceed == "day") %>%
  ggplot(aes(x = year, y = value)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = value),
            vjust = -0.5) +
  labs(x = "Year", y = expression(paste("Number of daily ", PM[10], " exceedances per year")), fill = "Metric")

DAILY_EXCEEDANCE_PM10

ggsave("daily_exceedance_pm10.png",
       plot = DAILY_EXCEEDANCE_PM10,
       path = figure_path,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300
)

# #-------------------------------------------------------------------------
# # Days above 24hr CAAQS pm2.5
# #-------------------------------------------------------------------------
#
# #Count how many days were above CAAQS each year
# # Count days with values above 28 for 2015-2019 and above 27 for 2020-2024 excluding wildfire days
# count_above_threshold <- data_24hr %>%
#   filter(param == "pm25", flag_tfee == "FALSE") %>%
#   mutate(
#     # Create a new column for the threshold based on year
#     threshold = case_when(
#       year >= 2015 & year <= 2019 ~ 28,
#       year >= 2020 & year <= 2024 ~ 27,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   # Filter out rows where the threshold is NA (i.e., outside 2015-2024 range)
#   filter(!is.na(threshold)) %>%
#   # Count the number of days per year where the value exceeds the threshold
#   group_by(year) %>%
#   summarise(
#     count_above_threshold = sum(value > threshold, na.rm = TRUE)
#   )
#
# #Plot days above threshold for each year
# count_above_threshold %>%
#   ggplot(aes(x = year, y = count_above_threshold)) +
#   geom_bar(stat = "identity") +  # Use stat = "identity" to map y directly to values
#   geom_text(aes(label = count_above_threshold), vjust = -0.3) +  # Add labels on top of the bars
#   scale_x_continuous(breaks = count_above_threshold$year) +  # Specify breaks to show each year as a tick
#   labs(x = "Year", y = expression(paste("Number of days 24-hr average ", PM[2.5], " above CAAQS"))) +
#   theme_minimal()

# #----------------------------------------------------------------------
# #Percent above or below AQO/PCO
# #----------------------------------------------------------------------
#
# #For each year calculate the % above or below (percent_diff) the AQO or PCO the value is for each day (PM2.5 (25ug/m3) and PM10 (50ug/m3) and BC PCO for TRS (3ug/m3 or 2 ppb))
# PERCENT_DIFF_AQO <- data_24hr %>%
#   filter(param %in% c("pm25", "pm10", "trs")) %>%
#   mutate(
#     percent_diff = case_when(
#       param == "pm25" ~ ((value - 25) / 25) * 100,  # AQO for pm25 is 25
#       param == "pm10" ~ ((value - 50) / 50) * 100,  # AQO for pm10 is 50
#       param == "trs" ~ ((value - 2) / 2) * 100,     # AQO for trs is 2
#       TRUE ~ NA_real_  # Handle any other unexpected cases
#     ),
#     # Create a new column for whether the value is above the AQO
#     above_aqo = case_when(
#       param == "pm25" & value > 25 ~ 1,  # AQO for PM2.5 is 25 ug/m³
#       param == "pm10" & value > 50 ~ 1,  # AQO for PM10 is 50 ug/m³
#       param == "trs" & value > 2 ~ 1,    # PCO TRS is 2 ppb
#       TRUE ~ 0  # 0 if it's not above the AQO
#     )
#   )
#
# #Plot the 24hr percent difference from AQO and PCO
# PERCENTDIFF24HRPLOT <- ggplot(PERCENT_DIFF_AQO, aes(x = date, y = percent_diff, colour = param)) +
#   geom_line() +
#   geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
#   labs(x = "Date", y = "Percent above or below AQO or PCO", colour = "Parameter") +
#   scale_color_manual(values = c("pm25" = "blue", "pm10" = "salmon", "trs" = "seagreen"),
#                      labels = c("PM2.5", "PM10", "TRS"))
#
# ggsave("percent_diff_24hr_plot.png",
#        plot = PERCENTDIFF24HRPLOT,
#        path = figure_path,
#        width = 10,
#        height = 6,
#        units = "in",
#        dpi = 300
# )




# #Count how many days TRS were above 2ppb each year ###NEED TO ACCOUNT FOR LEAP YEARs###
# count_above_threshold_24trs <- data_24hr %>%
#   filter(param == "trs") %>%
#   # Count the number of days per year where the value exceeds 2ppb
#   group_by(year) %>%
#   reframe(count_above_threshold_24trs = sum(value > 2, na.rm = TRUE),
#           percent_days_above_24trs_threshold = (count_above_threshold_24trs / 365)*100)
#
#
# #Plot days above threshold for each year
# count_above_threshold_24trs %>%
#   ggplot(aes(x = year, y = percent_days_above_24trs_threshold)) +
#   geom_bar(stat = "identity") +  # Use stat = "identity" to map y directly to values
#   geom_text(aes(label = label_number(accuracy = 1)(percent_days_above_24trs_threshold)),
#             vjust = -0.3) +  # Add labels with 2 significant figures
#   #geom_text(aes(label = percent_days_above_24trs_threshold), vjust = -0.3) +  # Add labels on top of the bars
#   scale_x_continuous(breaks = count_above_threshold_24trs$year) +  # Specify breaks to show each year as a tick
#   labs(x = "Year", y = "Percent days 24-hr average TRS above PCO") +
#   theme_minimal()


# ###NEED TO FIGURE OUT CUMULATIVE PERCENT CHANGE RATHER THAN ONLY % CHANGE###
#
# # Calculate the percent change in PM2.5 by season
# seasonal_percent_change <- data_season %>%
#   filter(param == "pm25") %>%
#   mutate(
#     percent_change = c(NA, diff(value) / head(value, -1) * 100)  # Calculate percent change
#   )
#
# #Plot % change in seasonal PM2.5
# seasonal_percent_change %>%
#   ggplot(aes(x = date, y = percent_change)) +
#   geom_point() +
#   labs(x = "Year", y = "Seasonal percent change")
#
# #Or a lollipop plot?
# ggplot(seasonal_percent_change, aes(x = date, y = percent_change, label = format(percent_change, digits = 1))) +
#   geom_point(stat='identity', fill="black", size = 8)  +
#   geom_segment(aes(y = 0,
#                    x = date,
#                    yend = percent_change,
#                    xend = date),
#                color = "black") +
#   geom_text(color="white", size = 3) +
#   labs(x = "Year",
#        y = "Seasonal percent change")
