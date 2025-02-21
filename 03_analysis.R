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
  geom_point() +
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

#------------------------------------------------------------------------
#ADVISORY DAYS
#------------------------------------------------------------------------

#Plot advisory days
ADVISORYDAYS %>%
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
  scale_x_continuous(breaks = ADVISORYDAYS$Year) +
  theme_minimal()

#-------------------------------------------------------------------------
# Days above 24hr CAAQS pm2.5
#-------------------------------------------------------------------------

#Count how many days were above CAAQS each year
# Count days with values above 28 for 2015-2019 and above 27 for 2020-2024 excluding wildfire days
count_above_threshold <- data_24hr %>%
  filter(param == "pm25", flag_tfee == "FALSE") %>%
  mutate(
    # Create a new column for the threshold based on year
    threshold = case_when(
      year >= 2015 & year <= 2019 ~ 28,
      year >= 2020 & year <= 2024 ~ 27,
      TRUE ~ NA_real_
    )
  ) %>%
  # Filter out rows where the threshold is NA (i.e., outside 2015-2024 range)
  filter(!is.na(threshold)) %>%
  # Count the number of days per year where the value exceeds the threshold
  group_by(year) %>%
  summarise(
    count_above_threshold = sum(value > threshold, na.rm = TRUE)
  )

#Plot days above threshold for each year
count_above_threshold %>%
  ggplot(aes(x = year, y = count_above_threshold)) +
  geom_bar(stat = "identity") +  # Use stat = "identity" to map y directly to values
  geom_text(aes(label = count_above_threshold), vjust = -0.3) +  # Add labels on top of the bars
  scale_x_continuous(breaks = count_above_threshold$year) +  # Specify breaks to show each year as a tick
  labs(x = "Year", y = expression(paste("Number of days 24-hr average ", PM[2.5], " above CAAQS"))) +
  theme_minimal()

#------------------------------------------------------------------------
#TRS
#------------------------------------------------------------------------
#Plot 24hr trs average
DATA24HRTRSPLOT <- data_24hr %>%
  filter(param == "trs") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  geom_hline(yintercept = 3, colour = "red", linetype = "dotted") +
  labs(x = "Year", y = expression(paste("24-hr average TRS", " (",mu,"g/",m^3,")"))) +
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
  labs(x = "Month", y = expression(paste("Monthly average TRS"," (",mu,"g/",m^3,")")))
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
  labs(x = "Year", y = expression(paste("Seasonal average TRS"," (",mu,"g/",m^3,")")))
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
  labs(x = "Year", y = expression(paste("Annual average TRS"," (",mu,"g/",m^3,")"))) +
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
# Number of days TRS above PCO
#-----------------------------------------------------------------------
#Count how many days TRS were above 2ppb each year ###NEED TO ACCOUNT FOR LEAP YEARs###
count_above_threshold_24trs <- data_24hr %>%
  filter(param == "trs") %>%
  # Count the number of days per year where the value exceeds 2ppb
  group_by(year) %>%
  reframe(count_above_threshold_24trs = sum(value > 2, na.rm = TRUE),
    percent_days_above_24trs_threshold = (count_above_threshold_24trs / 365)*100)


#Plot days above threshold for each year
count_above_threshold_24trs %>%
  ggplot(aes(x = year, y = percent_days_above_24trs_threshold)) +
  geom_bar(stat = "identity") +  # Use stat = "identity" to map y directly to values
  geom_text(aes(label = label_number(accuracy = 1)(percent_days_above_24trs_threshold)),
            vjust = -0.3) +  # Add labels with 2 significant figures
  #geom_text(aes(label = percent_days_above_24trs_threshold), vjust = -0.3) +  # Add labels on top of the bars
  scale_x_continuous(breaks = count_above_threshold_24trs$year) +  # Specify breaks to show each year as a tick
  labs(x = "Year", y = "Percent days 24-hr average TRS above PCO") +
  theme_minimal()






###NEED TO FIGURE OUT CUMULATIVE PERCENT CHANGE RATHER THAN ONLY % CHANGE###

# Calculate the percent change in PM2.5 by season
seasonal_percent_change <- data_season %>%
  filter(param == "pm25") %>%
  mutate(
    percent_change = c(NA, diff(value) / head(value, -1) * 100)  # Calculate percent change
  )

#Plot % change in seasonal PM2.5
seasonal_percent_change %>%
  ggplot(aes(x = date, y = percent_change)) +
  geom_point() +
  labs(x = "Year", y = "Seasonal percent change")

#Or a lollipop plot?
ggplot(seasonal_percent_change, aes(x = date, y = percent_change, label = format(percent_change, digits = 1))) +
  geom_point(stat='identity', fill="black", size = 8)  +
  geom_segment(aes(y = 0,
                   x = date,
                   yend = percent_change,
                   xend = date),
               color = "black") +
  geom_text(color="white", size = 3) +
  labs(x = "Year",
       y = "Seasonal percent change")
