wrangle_weather = function(){
  weather_data = readRDS("data/weather.rds")
  
  # add month column
  weather_data = weather_data |> 
    dplyr::mutate(
      month = lubridate::month(date),
      day_of_year = lubridate::yday(date)
    )
  
  # read in climate data
  climate_normals = readr::read_csv(
    "data/climate_normals.csv", show_col_types = FALSE
    ) |> suppressMessages()
  
  # fix excel weirdness
  climate_normals = climate_normals |> 
    dplyr::select(
      month,
      avg_temp_c,
      min_temp_c,
      max_temp_c,
      precipitation_mm,
      sun_hours,
      country_code
    ) |>
    tidyr::drop_na() |> 
    dplyr::mutate( 
      month = dplyr::case_when( # make month numerical
        month == "January" ~ 1,
        month == "February" ~ 2,
        month == "March" ~ 3,
        month == "April" ~ 4,
        month == "May" ~ 5,
        month == "June" ~ 6,
        month == "July" ~ 7,
        month == "August" ~ 8,
        month == "September" ~ 9,
        month == "October" ~ 10,
        month == "November" ~ 11,
        month == "December" ~ 12,
      ),
      clim_avg_precipitation_mm = dplyr::case_when( # get daily avg precipitations
        month %in% c(1, 3, 5, 7, 8, 10, 12) ~ round(precipitation_mm / 31, 1),
        month %in% c(4, 6, 9, 11) ~ round(precipitation_mm / 30, 1),
        month == 2 ~ round(precipitation_mm / 28.24, 1)
      )
    ) |> 
    dplyr::rename( # make better names
      clim_avg_temp = avg_temp_c,
      clim_max_temp = max_temp_c,
      clim_min_temp = min_temp_c,
      clim_monthly_precipitation_mm = precipitation_mm,
      clim_sun_hours = sun_hours
    ) |> 
    dplyr::slice(rep(1:dplyr::n(), times = c(
      31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
      ))) |> 
    dplyr::mutate(
      day_of_year = rep(1:365, 2)
    ) 
  
  climate_normals = climate_normals |> dplyr::bind_rows(
    dplyr::mutate(climate_normals, day_of_year = day_of_year - 365),
    dplyr::mutate(climate_normals, day_of_year = day_of_year + 365)
    ) |> dplyr::arrange(country_code, day_of_year) |> 
    dplyr::group_by(country_code) |> 
    dplyr::mutate(
      dplyr::across(starts_with("clim_"), ~ RcppRoll::roll_mean(.x, n = 30, align = "center", fill = NA))
    ) |> 
    dplyr::ungroup()
  
  # join weather and climate data
  weather_data_joined = dplyr::left_join(
    weather_data, climate_normals, by = c("day_of_year", "country_code")
    )
  
  # add over expected variables and rate of change in daylight duration
  weather_data_joined = weather_data_joined |> 
    dplyr::group_by(country_code) |> 
    dplyr::mutate(
      daily_temperature_2m_max_oe = daily_temperature_2m_max - clim_max_temp,
      daily_temperature_2m_min_oe = daily_temperature_2m_min - clim_min_temp,
      daily_precipitation_sum_oe = daily_precipitation_sum - clim_avg_precipitation_mm,
      daily_sunshine_duration_hours = round(daily_sunshine_duration / 60**2, 4),
      daily_sunshine_duration_seconds = daily_sunshine_duration,
      daily_sunshine_duration_hours_oe = round(daily_sunshine_duration_hours - clim_sun_hours, 1),
      daily_daylight_duration_hours = round(daily_daylight_duration / 60**2, 4),
      daily_daylight_duration_seconds = daily_daylight_duration,
      daily_daylight_roc_hours = dplyr::lead(daily_daylight_duration_hours) - daily_daylight_duration_hours,
      daily_daylight_roc_seconds = dplyr::lead(daily_daylight_duration_seconds) - daily_daylight_duration_seconds
    ) |> 
    dplyr::select( # select relevant bits
      country_code,
      date,
      clim_max_temp,
      clim_min_temp,
      clim_avg_precipitation_mm,
      clim_sun_hours,
      daily_temperature_2m_max,
      daily_temperature_2m_max_oe,
      daily_temperature_2m_min,
      daily_temperature_2m_min_oe,
      daily_precipitation_sum,
      daily_precipitation_sum_oe,
      daily_precipitation_hours,
      daily_sunshine_duration_seconds,
      daily_sunshine_duration_hours,
      daily_sunshine_duration_hours_oe,
      daily_wind_speed_10m_max,
      daily_cloud_cover_mean,
      daily_daylight_duration_seconds,
      daily_daylight_duration_hours,
      daily_daylight_roc_hours,
      daily_daylight_roc_seconds
    ) |> 
    dplyr::ungroup()
  
  return(weather_data_joined)
}

