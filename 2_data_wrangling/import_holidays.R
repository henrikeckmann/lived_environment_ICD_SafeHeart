rm(list = ls())
library(httr)
library(jsonlite)
library(tidyverse)

years = c("2020", "2021", "2022", "2023", "2024")
country_codes = c("NL", "DK")

data = list()
index = 1
for (country_code in country_codes){
  for (year in years){
    res = GET(paste(
      "https://date.nager.at/api/v3/publicholidays",
      year,
      country_code,
      sep = "/"
    ))
    data_in = fromJSON(rawToChar(res$content))
    data[[index]] = data_in
    index = index + 1
  }
}
data = bind_rows(data)

saveRDS(data, "data/holidays.rds")
