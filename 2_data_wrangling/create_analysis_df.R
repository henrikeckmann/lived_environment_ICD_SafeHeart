rm(list = ls())
gc()

library(dplyr)
library(readr)
library(readxl)
library(RcppRoll)
library(tidyr)
library(lubridate)
source("2_data_wrangling/wrangle_weather.R")

### ACCELEROMETRY
analysis_df = readRDS("../accel_df.rds") |> 
  rename(day_of_week = weekday) |> 
  mutate(# create weekend and week_number_file variables
    weekend = if_else((day_of_week == 1 | day_of_week == 7), 1, 0, missing = NA),
    week_number_file = floor(day_number_file / 7) + 1
  )


### FILE ALLOCATIONS
analysis_df = full_join(
  analysis_df, 
  read_xlsx(""),
  by = "BinfileName"
  ) |> 
  mutate( # add day and week number per participant, convert posix to date
    Enrolled = as.Date(Enrolled),
    FileStartDate = as.Date(FileStartDate),
    day_number_participant = as.numeric(difftime(date, Enrolled, units = "days")) + 1,
    week_number_participant = floor(day_number_participant / 7) + 1
  ) |> 
  rename(
    FileStartDayNumber = StartDayNumber,
    FileEndDayNumber = EndDayNumber,
    FileNumberDays = NumberDays,
    FileMeasurementDeviceID = MeasurementDeviceID,
    FileMeasurementDurationActual = MeasurementDurationActual,
    FileMeasurementFrequency = MeasurementFrequency,
    FileWearTime = WearTime,
    FileErrors = Errors
  ) |>
  arrange(Participant, FileStartDayNumber)


### BASELINE DATA
baseline_df = read_csv("") |> 
  mutate(# count comorbidities and medications
    comorbidities_n = rowSums(
      pick(
        starts_with("#"),
        -`#PCI`,
        -`#CABG`,
        -`#Cardiomyopathy`
        )
      ),
    cardiomyopathy = if_else(
      rowSums(
        pick(
          `#Cardiomyopathy`,
          ICM,
          DCM,
          HCM,
          ARVC,
          LQTC,
          Brugada,
          UnknownCMP
          )
        ) > 0, 1, 0
      ),
    medications_n = rowSums(pick(ends_with("_substance_"))),
    country_code = if_else(Site == "AUMC", "NL", "DK", missing = NA)
  ) |> 
  select(
    `Participant Id`,
    Site,
    country_code,
    Age,
    pat_sex,
    pat_bmi,
    baseline_smoking,
    secondary_prevention_yn,
    comorbidities_n,
    medications_n,
    cardiomyopathy,
    MyocardialInfarction,
  ) |> 
  rename(
    bmi = pat_bmi,
    smoking = baseline_smoking,
    sex = pat_sex,
    Participant = `Participant Id`,
    age = Age,
    myocardialinfarction = MyocardialInfarction,
    site = Site
  )

# join
analysis_df = analysis_df |> 
  full_join(baseline_df, by = c("Participant" = "Participant"))

rm(baseline_df)

### WEATHER
analysis_df = left_join(
  analysis_df,
  wrangle_weather(), # call wrangling function from data_wrangling/wrangle_weather.R
  by = c("date", "country_code")
  )

### Holidays
holidays_nl = readRDS("data/holidays.rds") |> 
  filter(countryCode == "NL")
holidays_dk = readRDS("data/holidays.rds") |> 
  filter(countryCode == "DK")

analysis_df = analysis_df |> 
  mutate(# create holiday indicator and weekend_or_holiday
    holiday_nl = if_else(date %in% as.Date(holidays_nl$date), 1, 0, missing = NA),
    holiday_dk = if_else(date %in% as.Date(holidays_dk$date), 1, 0, missing = NA),
    holiday = if_else(country_code == "NL", holiday_nl, holiday_dk, missing = NA),
    weekend_or_holiday = as.numeric(weekend + holiday > 0)
  ) |> 
  select(
    -holiday_nl,
    -holiday_dk,
  )

rm(holidays_dk, holidays_nl)

### MORTALITY
analysis_df = analysis_df |> 
  left_join(
    read_csv(""), 
    by = c("Participant" = "SubjectCode")
  ) |> 
  mutate(# rather than NAs if mortality not 1, fill in zeroes
    Mortality = if_else(Mortality == 1, 1, 0, missing = 0)
  ) |> 
  rename(
    mortality = Mortality,
    mort_date = Mort_date
  )


### EVENTS/ENDPOINTS
events_df = read_csv("") |> 
  mutate(# get date from string
    date = as.Date(Date_appropriate_therapy, format = "%d/%m/%Y")
  ) |> 
  select(
    SubjectCode,
    date,
    Appropriate_ICDtherapy
  ) |> 
  distinct(date, .keep_all = TRUE) # only keep distinct dates as we're just categorising dates as event y/n
  
analysis_df = analysis_df |> 
  left_join(
    events_df,
    by = c("Participant" = "SubjectCode", "date")
  ) |> 
  mutate(# as for mortality
    Appropriate_ICDtherapy = if_else(Appropriate_ICDtherapy > 0, 1, 0, missing = 0),
    bmi_factor = case_when(
      bmi >= 18.5 & bmi < 25 ~ "normal",
      bmi < 18.5 ~ "underweight",
      bmi < 30 ~ "overweight",
      bmi < 35 ~ "class 1 obese",
      bmi < 40 ~ "class 2 obese",
      bmi >= 40 ~ "class 3 obese"
    ),
    bmi_factor = relevel(as.factor(bmi_factor), ref = "normal")
  ) |> 
  rename(
    event_icd = Appropriate_ICDtherapy
  )
rm(events_df)

max_days = select(analysis_df, day_number_participant) |> max(na.rm = TRUE)
distinct_participants = select(analysis_df, Participant) |> distinct() |> pull()
distinct_days_df = select(analysis_df, Participant, day_number_participant, event_icd) |> distinct()

analysis_df = tibble(
  day_number_participant = rep(0:max_days, length(distinct_participants)),
  Participant = rep(distinct_participants, each = max_days + 1)
  ) |> 
  left_join(distinct_days_df, by = c("day_number_participant", "Participant")) |> 
  mutate(# proximity to event
    event_icd = if_else(event_icd == 1, 1, 0, missing = 0),
    before_event_28 = roll_maxl(event_icd, 28, fill = 0), 
    after_event_28 = roll_maxr(event_icd, 28, fill = 0),
    before_event_7 = roll_maxl(event_icd, 7, fill = 0),
    after_event_7 = roll_maxr(event_icd, 7, fill = 0),
    proximity_event_28 = case_when( 
      before_event_28 == 1 ~ 1, 
      after_event_28 == 1 ~ 1, 
      .default = 0 
    ), 
    proximity_event_7 = case_when( 
      before_event_7 == 1 ~ 1, 
      after_event_7 == 1 ~ 1, 
      .default = 0
    ),
    event_level_proximity = case_when(
      event_icd == 1 ~ 3,
      proximity_event_7 == 1 ~ 2,
      proximity_event_28 == 1 ~ 1,
      .default = 0
    ),
    event_level_before = case_when(
      event_icd == 1 ~ 0,
      before_event_7 == 1 ~ 2,
      before_event_28 == 1 ~ 1,
      .default = 0
    ),
    event_level_after = case_when(
      event_icd == 1 ~ 0,
      after_event_7 == 1 ~ 1,
      after_event_28 == 1 ~ 2,
      .default = 0
    )
  ) |>
  select(
    -starts_with("before_event_"),
    -starts_with("after_event_"),
    -starts_with("proximity_event_"),
    -event_icd
  ) |> 
  right_join(analysis_df, by = c("day_number_participant", "Participant"))

rm(wrangle_weather, max_days, distinct_participants, distinct_days_df)
gc()

analysis_df_exclusions = analysis_df

analysis_df = analysis_df |> 
  filter(
    day_number_file != 1,
    day_number_file != FileNumberDays,
    valid_wear_hours >= 22,
    mortality == 0,
    ) |>
  select(
    -FileErrors,
    -mort_date,
    -mortality
    ) |> 
  drop_na() |> 
  filter(# drop 8 days which are duplicate in pairs where both days in each pair look valid
    !((day_number_participant == lag(day_number_participant) & Participant == lag(Participant))),
    !((day_number_participant == lead(day_number_participant) & Participant == lead(Participant)))
  )

analysis_df_weekly = analysis_df |> 
  select(
    -day_of_year,
    -day_number_file
  ) |> 
  group_by(BinfileName, week_number_file) |> 
  summarise(
    week_of_year_start = first(week_of_year),
    day_of_week_start = first(day_of_week),
    event_level_proximity = max(event_level_proximity),
    event_level_before = max(event_level_before),
    event_level_after = max(event_level_after),
    event_icd = max(event_icd),
    week_number_participant = round(mean(week_number_participant), 0),
    across(
      where(is.numeric) &
        !c(week_of_year, day_of_week, week_number_participant) &
        !starts_with("event_"),
      ~ mean(.x)
      ),
    across(where(is.character), ~ first(.x)),
    across(where(is.Date), ~ first(.x)),
    valid_days = n()
  ) |> 
  ungroup() |> 
  arrange(Participant, week_number_participant) |> 
  filter( # 3 valid days
    valid_days > 2
  ) 

analysis_df_weekly_factors = analysis_df_weekly|> 
  mutate(
    across(
      where(is.character), ~ as.factor(.x)
    ),
    Participant = as.factor(Participant),
    event_level_proximity = as.factor(event_level_proximity),
    event_level_before = as.factor(event_level_before),
    event_level_after = as.factor(event_level_after),
    event_icd = as.factor(event_icd),
    weekend = as.factor(weekend),
    holiday = as.factor(holiday),
    weekend_or_holiday = as.factor(weekend_or_holiday),
    FileMeasurementDeviceID = as.factor(FileMeasurementDeviceID),
    FileMeasurementFrequency = as.factor(FileMeasurementFrequency),
    day_number_participant = as.factor(day_number_participant),
    week_number_participat = as.factor(week_number_participant),
    cardiomyopathy = as.factor(cardiomyopathy),
    myocardialinfarction = as.factor(myocardialinfarction),
    sex = relevel(sex, "Male"),
    secondary_prevention_yn = relevel(secondary_prevention_yn, "Yes")
  )
  
analysis_df_factors = analysis_df |> 
  mutate(
    across(
      where(is.character), ~ as.factor(.x)
    ),
    Participant = as.factor(Participant),
    event_level_proximity = as.factor(event_level_proximity),
    event_level_before = as.factor(event_level_before),
    event_level_after = as.factor(event_level_after),
    event_icd = as.factor(event_icd),
    weekend = as.factor(weekend),
    holiday = as.factor(holiday),
    weekend_or_holiday = as.factor(weekend_or_holiday),
    FileMeasurementDeviceID = as.factor(FileMeasurementDeviceID),
    FileMeasurementFrequency = as.factor(FileMeasurementFrequency),
    day_number_participant = as.factor(day_number_participant),
    week_number_participant = as.factor(week_number_participant),
    cardiomyopathy = relevel(as.factor(cardiomyopathy), ref = "1"),
    myocardialinfarction = as.factor(myocardialinfarction),
    sex = relevel(sex, "Male"),
    secondary_prevention_yn = relevel(secondary_prevention_yn, "Yes")
  )

