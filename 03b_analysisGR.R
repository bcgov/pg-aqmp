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

### Explore
# annual average trend
ann_avg_data <- data_1y %>%
  filter(param %in% c("no2", "o3", "pm10", "pm25", "so2", "trs")) |>
  mutate(value = case_when(
    annual_data_capture < 75 ~ NA,
    pmin(q1_data_capture, q2_data_capture, q3_data_capture, q4_data_capture, na.rm = TRUE) < 60 ~ NA,
    is.na(value) ~ NA,
    TRUE ~ value))

ggplot(data = ann_avg_data, aes(x = year, y = value)) +
  geom_line() +
  geom_point() +
  labs(xlab = "",
       ylab = "Concentration") +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
  facet_wrap(~param, scales = "free_y")

# plot AQOs as a percent - all params. For % difference graph.

annual_based_aqos <- read.csv("data/objectives.csv") |>
  filter(param %in% c("pm25", "so2", "no2", "o3"),
         station_name == "Prince George Plaza 400") |>
  mutate(value = ifelse(data_capture_met == "no", NA, value),
         percent_diff = round(((value - threshold)/threshold)*100,0),
         names = factor(names, levels = c("PM2.5 annual", "PM2.5 24-hr", "NO2 annual", "NO2 1-hr", "SO2 annual", "SO2 1-hr", "O3 8-hr")))

my_colours3 <- RColorBrewer::brewer.pal(n = 9, name = "Set1")[c(1,2,3,4,5,6,7)]

p1 <- ggplot(annual_based_aqos, aes(x = year, y = percent_diff, colour = name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-150, 450, by = 25)) +
  scale_x_continuous(
    breaks = 2010:2024,  # Show ticks for each year
    labels = c(2010:2023, "2024*") # Optionally convert the ticks to characters for display
  ) +
  expand_limits(y = c(-125, 400)) +
  labs(
    x = "Year",
    y = "Percent above or below objective/standard (%)",
    colour = "Objective/Standard"
  ) +
  scale_color_manual(
    values = my_colours3,
    labels = c(
      expression(PM[2.5] ~ " - Annual"),
      expression(PM[2.5] ~ " - 24-hr"),
      expression(NO[2] ~ " - Annual"),
      expression(NO[2] ~ " - 1-hr"),
      expression(SO[2] ~ " - Annual"),
      expression(SO[2] ~ " - 1-hr"),
      expression(O[3] ~ " - 8-hr")
    )
  ) +
  theme_bw() +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid = element_blank())
p1 + scale_y_break(c(200, 350))

ggplot(data_1y |> filter(param %in% c("no2", "o3", "pm25", "pm10", "so2", "trs")),
       aes(x = year, y = value)
       ) +
  geom_line() +
  geom_point() +
  facet_wrap(~param)

# relative differences between 2015 and 2024
diff_2015_2024 <- read.csv("data/objectives.csv") |>
  filter(param %in% c("pm25", "so2", "no2", "o3"),
         year %in% c(2014, 2023)) |>
  mutate(value = ifelse(data_capture_met == "no", NA, value)) |>
  select(station_name, year, name, value) |>
  pivot_wider(names_from = "year", values_from = "value") |>
  mutate(percent_diff = round(((`2023`-`2014`)/`2014`)*100,0))


# pm25 aqos with and without heavy wildfire impacts
pm25_aqos_for_plot <- data_24hr |>
  filter(param == "pm25") |>
  select(station_name, year, param, value, value_tfee) |>
  group_by(station_name, param, year) |>
  summarise(aqo_24hr = quantile(value, 0.98, na.rm = TRUE),
            aqo_24hr_local = quantile(value_tfee, 0.98, na.rm = TRUE),
            aqo_1y = mean(value, na.rm = TRUE),
            aqo_1y_local = mean(value_tfee, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(contains("aqo"), round, 1),
         aqo_24hr_smoke = aqo_24hr - aqo_24hr_local,
         aqo_1y_smoke = aqo_1y - aqo_1y_local) |>
  select(-aqo_24hr, -aqo_1y) |>
  pivot_longer(cols = matches("local|smoke"),
               names_to = c(".value", "wildfire"),
               names_pattern = c("(aqo_24hr|aqo_1y)_(local|smoke)")) |>
  pivot_longer(cols = matches("_24hr|1y"),
               names_to = "objective",
               values_to = "value") |>
  mutate(wildfire = factor(wildfire, levels = c("smoke", "local")),
         objective = factor(objective,
                            levels = c("aqo_24hr", "aqo_1y")),
         value = case_when(year %in% c(2015,2017,2019,2020,2022) ~ NA,  # invalidate years with insufficient data capture; to do: use data cap dfs to do this programmatically
                           TRUE ~ value))


### prep plots

thresholds <- data.frame(
  objective = c("aqo_24hr", "aqo_1y", "aqo_1y"),
  value = c(25, 8, 6),
  label = c("current objective", "current objective", "planning objective")
)

my_colours <- RColorBrewer::brewer.pal(n = 9, name = "Set1")[c(9,2)]
names(my_colours) <- levels(pm25_aqos_for_plot)

plt_ylab <- expression(paste(PM[2.5], " (", mu, "g ", m^{-3}, ")"))

facet_names <- c(
  `aqo_24hr` = "24-hour Objective",
  `aqo_1y` = "Annual Objective"
)

ggplot(data = pm25_aqos_for_plot, aes(fill = wildfire, x = factor(year), y = value)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_hline(data = thresholds, aes(yintercept = value, linetype = label)) +
  scale_fill_manual(name = "",
                    values = my_colours,
                    labels = c("Heavy wildfire smoke",
                               "Local sources")) +
  scale_linetype(name = "") +
  facet_wrap(~objective,
             scales = "free_y",
             labeller = as_labeller(facet_names),
             drop = TRUE,
             nrow = 2) +
  labs(title = bquote(PM[2.5]~"B.C. Ambient Air Quality Objecitves - Plaza 400"),
       x = "Year",
       y = plt_ylab,
       caption = "*2024 - preliminary data") +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 16))

# poor air quality days by season
 pm25_24hr_seasons <- data_24hr |>
   filter(param == "pm25") |>
   mutate(met_season = case_when(
     month %in% month.abb[9:11] ~ "Fall (Sep - Nov)",
     month %in% month.abb[c(12, 1,2)] ~ "Winter(Dec - Feb)",
     month %in% month.abb[3:5] ~ "Spring (Mar - May)",
     TRUE ~ "Summer (Jun - Aug)"
   ))


 exceed25_by_season <- pm25_24hr_seasons |>
   group_by(year, met_season) |>
   summarise(
     ndays_25 = sum(value > 25, na.rm = TRUE),
     ndays_25_nosmoke = sum(value_tfee > 25, na.rm = TRUE),
     .groups = "drop"
   ) |>
   pivot_longer(cols = contains("ndays"),
                names_to = "wildfire",
                values_to = "value") |>
   mutate(value = case_when(year %in% c(2015,2017,2019,2020,2022) ~ NA,  # invalidate years with insufficient data capture; to do: use data cap dfs to do this programmatically
                            TRUE ~ value))


 my_colours2 <- RColorBrewer::brewer.pal(n = 9, name = "Set1")[c(3,6,5,2)]
 plt_subtitle <- expression(paste(PM[2.5], " 24-hr concentration greater than or equal to 25 ", mu, "g ", m^3))

 facet_names <- c(
   `ndays_25_nosmoke` = "Heavy wildfire smoke smoke removed",
   `ndays_25` = "Wildfire smoke included"
 )

 advisory_days <- read.csv("data/advisory_summary.csv") |>
   filter(param == "pm25",
          year >= 2015)

 ggplot() +
   geom_col(data = exceed25_by_season,
            aes(x = factor(year),
                y = value,
                fill = forcats::fct_rev(met_season)),
            position = "stack") +
   scale_fill_manual(name = "Season",
                     values = my_colours2) +
   geom_line(data = advisory_days,
             aes(x = factor(year),
                 y = ndays,
                 group = type,
                 linetype = type),
             stat = "identity") +
   geom_point(data = advisory_days,
             aes(x = factor(year),
                 y = ndays,
                 group = type)) +
   scale_linetype(name = "Notification type",
                  labels = c("advisory", "smoky skies bulletin")) +
   labs(title = "Poor air quality days measured at Plaza 400",
        subtitle = plt_subtitle,
        x = "",
        y = "# Days",
        caption = "2024 - preliminary data") +
   facet_wrap(~forcats::fct_rev(wildfire),
              labeller = as_labeller(facet_names),
              drop = TRUE,
              nrow = 2) +
   theme(legend.position = "right",
         axis.text = element_text(size = 12),
         axis.title = element_text(size = 16),
         strip.text.x = element_text(size = 14),
         legend.text = element_text(size = 12))




