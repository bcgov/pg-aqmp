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
library(ggrepel)
library(dplyr)
library(lubridate)
library(scales)
library(RColorBrewer)

data_1y <- readRDS(file = "data/data_1y.rds")
data_24hr <- readRDS(file = "data/data_24hr.rds")
data_1hr <- readRDS(file = "data/data_1hr.rds")

#------------------------------------------------------
# Plot 1: time series of annually-based AQOs expressed as a percentage of objective threshold
# above the line = AQO exceeded; below the line = AQO not exceeded
#------------------------------------------------------

annual_based_aqos <- read.csv("data/objectives_pm25_sharp_est.csv") |>
  filter(param %in% c("pm25", "so2", "no2", "o3"),
         station_name == "Prince George Plaza 400",
         year > 2015) |>
  mutate(value = ifelse(data_capture_met == "no", NA, value),
         percent_diff = round(((value - threshold)/threshold)*100,0),
         name = factor(name, levels = c("PM2.5 annual", "PM2.5 24-hr", "NO2 annual", "NO2 1-hr", "SO2 annual", "SO2 1-hr", "O3 8-hr")),
         time_average = factor(time_average, levels = c("annual", "24-hour", "8-hour", "1-hour"))
         )

label_data <- annual_based_aqos |>
  group_by(name) |>
  filter(year == max(year)) |>
  ungroup() |>
  select(year, name, param, time_average, percent_diff)

outlier_value <- paste0(annual_based_aqos |> filter(percent_diff > 200) |>  select(percent_diff) |> pull(), "%")

p1 <- ggplot(annual_based_aqos,
             aes(x = year,
                 y = percent_diff,
                 colour = param,
                 linetype = time_average,
                 shape = time_average,
                 group = name)) +

  geom_line(linewidth = 1.1) +

  geom_point() +

  geom_point(data = annual_based_aqos |> filter(percent_diff > 200),  #outlier symbol
             aes(x = year, y = 212),
             shape = 24, fill = "red", size = 2) +

  geom_hline(yintercept = 0, colour = "black", linewidth = 0.8) +

  scale_x_continuous(
    breaks = seq(min(annual_based_aqos$year), max(annual_based_aqos$year), by = 1),  # every year
    labels = as.character(seq(min(annual_based_aqos$year), max(annual_based_aqos$year), by = 1))
    ) +

  expand_limits(x = 2025.5) +

  labs(
    x = "",
    y = "Percent above or below objective/standard (%)",
    colour = "Objective/Standard"
  ) +

  scale_linetype_manual(name = "Averaging Period",
                        values = c(
                          "annual" = "solid",
                          "24-hour" = "dashed",
                          "1-hour" = "dotted",
                          "8-hour" = "dotdash"
                        )) +

  scale_shape_manual(values = c(
    "annual" = 16,  # circle
    "24-hour" = 17, # triangle
    "1-hour" = 15,  # square
    "8-hour" = 18   # diamond
  )) +

    scale_colour_manual(
    values = c(
      "pm25" = "#0072B2",
      "no2" = "#009E73",
      "so2" = "#E69F00",
      "o3" = "#CC79A7"
    ),
    labels = c(
      expression(PM[2.5]),
      expression(NO[2]),
      expression(SO[2]),
      expression(O[3])
    )) +

    geom_text_repel(
    data = label_data,
    aes(label = name),
    direction = "y",
    hjust = 0,
    nudge_x = 0.05,
    segment.colour = "grey50",
    show.legend = FALSE
  ) +

  geom_text_repel(
    data = annual_based_aqos |> filter(percent_diff > 200),
    aes(x = year, y = 225, label = paste0(round(percent_diff), "%"))  # outlier label
  ) +

  annotate("text", x = 2018, y = 5, label = "Provincal Objective Threshold (2025)", size = 4) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf, alpha = 0.03, fill = "red") +

  coord_cartesian(ylim = c(-100, 225)) +

  theme_minimal(base_size = 12) +

  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90"),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

p1

#------------------------------------------------------
# Plot 2: time series PM2.5 at Plaza 400 that differentiates the AQOs with and without TFEE
#------------------------------------------------------

pm25_aqos_for_plot <- data_24hr |>
  filter(param == "pm25", year > 2015) |>
  select(station_name, year, param, value, value_tfee) |>
  summarise(aqo_24hr = quantile(value, 0.98, na.rm = TRUE),
            aqo_24hr_local = quantile(value_tfee, 0.98, na.rm = TRUE),
            aqo_1y = mean(value, na.rm = TRUE),
            aqo_1y_local = mean(value_tfee, na.rm = TRUE),
            .by = c(station_name, year)
            ) |>
  mutate(across(contains("aqo"), ~round(.x, 1)),
         aqo_24hr_smoke = aqo_24hr - aqo_24hr_local,
         aqo_1y_smoke = aqo_1y - aqo_1y_local) |>
  select(-aqo_24hr, -aqo_1y) |>
  tidyr::pivot_longer(cols = matches("local|smoke"),
               names_to = c(".value", "wildfire"),
               names_pattern = c("(aqo_24hr|aqo_1y)_(local|smoke)")) |>
  tidyr::pivot_longer(cols = matches("_24hr|1y"),
               names_to = "objective",
               values_to = "value") |>
  dplyr::mutate(wildfire = factor(wildfire, levels = c("smoke", "local")),
         objective = factor(objective,
                            levels = c("aqo_24hr", "aqo_1y")),
         value = case_when(year %in% c(2017,2019,2020,2022) ~ NA,  # invalidate years with insufficient data capture; to do: use data capture to do this programmatically
                           TRUE ~ as.numeric(value)))

thresholds <- data.frame(
  objective = c("aqo_24hr", "aqo_1y", "aqo_1y"),
  value = c(25, 8, 6),
  label = c("Current objective", "Current objective", "Planning objective")
)

my_colours <- RColorBrewer::brewer.pal(n = 9, name = "Set1")[c(9,2)]
names(my_colours) <- levels(pm25_aqos_for_plot)

plt_ylab <- expression(paste(PM[2.5], " (", mu, "g ", m^{-3}, ")"))

facet_names <- c(
  `aqo_1y` = "Annual Objective",
  `aqo_24hr` = "24-hour Objective"
)

p2 <- ggplot() +
  geom_bar(data = pm25_aqos_for_plot, aes(fill = wildfire, x = factor(year), y = value), position = "stack", stat = "identity") +
  geom_hline(data = thresholds, aes(yintercept = value, linetype = label)) +
  scale_fill_manual(name = "",
                    values = my_colours,
                    labels = c("Heavy wildfire smoke",
                               "Local sources")) +
  scale_linetype(name = "") +
  facet_wrap(~factor(objective, c("aqo_24hr", "aqo_1y")),
             scales = "free_y",
             labeller = as_labeller(facet_names),
             drop = TRUE,
             nrow = 2) +
  labs(title = bquote(PM[2.5]~"B.C. Ambient Air Quality Objecitves - Plaza 400"),
       x = "",
       y = plt_ylab) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.background = element_blank()) +
  guides(fill = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

# add NA labels to years with insufficient data to avoid misinterpretation of "0 days"
p2 <- p2 + annotate("text", x = c(2,4,5,7), y = 1, label = "NA")
p2

#------------------------------------------------------
# Plot 3: Number of days PM25 measured at Plaza 400 exceeded 24-hour AQO threshold
# columns coloured by season
#------------------------------------------------------

data_capture <- data_1y |>
  filter(param %in% c("pm25", "pm10")) |>
  select(station_name, year, param, data_cap_met)

# PM exceedances by season -- also used in Plot 4
 pm_24hr_seasons <- data_24hr |>
   filter(param %in% c("pm25", "pm10")) |>
   mutate(met_season = case_when(
     month %in% month.abb[9:11] ~ "Fall (Sep - Nov)",
     month %in% month.abb[c(12, 1,2)] ~ "Winter(Dec - Feb)",
     month %in% month.abb[3:5] ~ "Spring (Mar - May)",
     TRUE ~ "Summer (Jun - Aug)"
   ),
   met_season = factor(met_season,
                       levels = c("Winter(Dec - Feb)",
                                  "Fall (Sep - Nov)",
                                  "Summer (Jun - Aug)",
                                  "Spring (Mar - May)")
                       ))

exceed_by_season <- pm_24hr_seasons |>
  mutate(threshold = ifelse(param == "pm25", 25, 50)) |>
  summarise(ndays_exceed = sum(value > threshold,na.rm = TRUE),
            ndays_exceed_tfee_removed = sum(value_tfee > threshold, na.rm  = TRUE),
            .by = c(station_name, year, param, met_season)
  ) |>
  left_join(data_capture) |>
  mutate(ndays_exceed = ifelse(data_cap_met == FALSE, NA, ndays_exceed),
         ndays_exceed_tfee_removed = ifelse(data_cap_met == FALSE, NA, ndays_exceed_tfee_removed)) |>
  tidyr::pivot_longer(cols = starts_with("ndays_exceed"), values_to = "value", names_to = "wildfire")

 my_colours2 <- RColorBrewer::brewer.pal(n = 9, name = "Set1")[c(3,6,5,2)]
 plt_subtitle <- expression(paste(PM[2.5], " 24-hr concentration greater than or equal to 25 ", mu, "g ", m^3))

 facet_names <- c(
   `ndays_exceed` = "All data",
   `ndays_exceed_tfee_removed` = "Heavy wildfire smoke smoke removed"
 )

p3 <- ggplot() +
   geom_col(data = exceed_by_season |> filter(param == "pm25"),
            aes(x = factor(year),
                y = value,
                fill = forcats::fct_rev(met_season)),
            position = "stack") +
   scale_fill_manual(name = "Season",
                     values = my_colours2) +
   labs(title = "Number of days PM2.5 exceeded the 24-hour AQO threshold by season",
        subtitle = plt_subtitle,
        x = "",
        y = "# Days") +
   facet_wrap(~wildfire,
              labeller = as_labeller(facet_names),
              drop = TRUE,
              nrow = 2) +
   theme(legend.position = "bottom",
         axis.text = element_text(size = 10),
         axis.title = element_text(size = 16),
         strip.text.x = element_text(size = 14),
         legend.text = element_text(size = 12),
         legend.background = element_blank())

# add NA labels to years with insufficient data to avoid misinterpretation of "0 days"
p3 <- p3 + annotate("text", x = c(2,4,5,7), y = 1, label = "NA")
p3

#------------------------------------------------------
# Plot 4: Number of days PM10 measured at Plaza 400 exceeded 24-hour AQO threshold
# columns coloured by season
#------------------------------------------------------

plt_subtitle <- expression(paste(PM[10], " 24-hr concentration greater than or equal to 50 ", mu, "g ", m^3))

p4 <- ggplot() +
  geom_col(data = exceed_by_season |> filter(param == "pm10"),
           aes(x = factor(year),
               y = value,
               fill = forcats::fct_rev(met_season)),
           position = "stack") +
  scale_fill_manual(name = "Season",
                    values = my_colours2) +
  labs(title = "Number of days PM10 exceeded the 24-hour AQO threshold by season",
       subtitle = plt_subtitle,
       x = "",
       y = "# Days") +
  facet_wrap(~wildfire,
             labeller = as_labeller(facet_names),
             drop = TRUE,
             nrow = 2) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

# add NA labels to years with insufficient data to avoid misinterpretation of "0 days"
p4 <- p4 + annotate("text", x = c(2,3,9), y = 1, label = "NA")
p4

#------------------------------------------------------
# Plot 5: TRS
#------------------------------------------------------

odour_days <- data_1hr |>
  filter(param == "trs") |>
  # number of hours per day with trs concentration >= 5ppb
  summarise(nhrs_5ppb  = sum(raw_value >= 5, na.rm = TRUE),
            .by = c(param, station_name, year, month, date)) |>
  mutate(met_season = case_when(
    month %in% month.abb[9:11] ~ "Fall (Sep - Nov)",
    month %in% month.abb[c(12, 1,2)] ~ "Winter(Dec - Feb)",
    month %in% month.abb[3:5] ~ "Spring (Mar - May)",
    TRUE ~ "Summer (Jun - Aug)"
  ),
  met_season = factor(met_season,
                      levels = c("Winter(Dec - Feb)",
                                 "Fall (Sep - Nov)",
                                 "Summer (Jun - Aug)",
                                 "Spring (Mar - May)")
  )) |>
  # for each station, year and season, count the number of days where at least 6 hours were >= 5ppb
  summarise(odour_days = sum(nhrs_5ppb >= 6, na.rm = TRUE),
            .by = c(station_name, param, year, met_season)) |>
  left_join(data_1y |> filter(param == "trs") |>  select(station_name, year, data_cap_met)) |>
  # assign NA for years without sufficient data capture
  mutate(odour_days = ifelse(data_cap_met == FALSE, NA, odour_days),
         labels = ifelse(data_cap_met == FALSE|is.na(data_cap_met), "NA", "")) |>
  droplevels()

labels <- odour_days |>
  summarise(total_days = sum(odour_days, na.rm = TRUE),
            na_labels = unique(labels),
            .by = c(station_name, year)
  ) |>
  mutate(total_days = ifelse(total_days == 0, NA, total_days))


p5 <- ggplot() +
  geom_col(data = odour_days,
           aes(x = factor(year),
               y = odour_days,
               fill = forcats::fct_rev(met_season)),
           position = "stack") +

  geom_text(data = labels,
            aes(x = factor(year), y = 5,label = na_labels)
            ) +

  geom_text(data = labels,
            aes(x = factor(year), y = total_days + 5, label = total_days)
            ) +

  scale_fill_manual(name = "Season",
                    values = my_colours2) +
  labs(title = "Odorous days by season",
       subtitle = "Number of days TRS >= 5ppb for 6 or more hours",
       x = "",
       y = "# Days") +
  facet_wrap(~station_name) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

p5



