rm(list=ls())
library(tidyverse)
library(openmeteo)

weather_copenhagen = weather_history(
  location = "copenhagen",
  start = "2020-01-01",
  end ="2024-12-31",
  daily = c(
    "temperature_2m_max",
    "temperature_2m_min",
    "precipitation_sum",
    "precipitation_hours",
    "wind_speed_10m_max",
    "cloud_cover_mean",
    "sunshine_duration",
    "daylight_duration"
  ),
  model = "era5"
  )

weather_amsterdam = weather_history(
  location = "amsterdam",
  start = "2020-01-01",
  end ="2024-12-31",
  daily = c(
    "temperature_2m_max",
    "temperature_2m_min",
    "precipitation_sum",
    "precipitation_hours",
    "wind_speed_10m_max",
    "cloud_cover_mean",
    "sunshine_duration",
    "daylight_duration"
  ),
  model = "era5"
)

weather_copenhagen = weather_copenhagen |>
  mutate(country_code = "DK")
weather_amsterdam = weather_amsterdam |>
  mutate(country_code = "NL")
weather_data = bind_rows(weather_copenhagen, weather_amsterdam)

saveRDS(weather_data, "data/weather.rds")

