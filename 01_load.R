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
# retrieve data from ftp site
#------------------------------------------------------

library(envair)

params <- grep("pm|so2|trs|no2|o3|temp|wspd_sclr|wdir_vect",
               list_parameters(),
               value = TRUE)

data_1hr_original <- params |>
  purrr::map_dfr(
    ~ importBC_data(
      parameter_or_station = .,
      2016:2025,
      flag_TFEE = TRUE,
      clean_names = TRUE,
      pad_data = TRUE)
    )

  # select all stations in Prince George; suitable search term for this project, but may not capture all historical sites
data_1hr_pg_original <- data_1hr_original |> filter(grepl("Prince George", station_name))

# note: time zone is incorrectly assigned as UTC; fix is provided in 02b_clean_prep.r
saveRDS(data_1hr_pg_original, file = "data/data_1hr_original.rds")


