library(readr)
library(vroom)
library(dplyr)
library(data.table)


csv_file <- file.path("data-raw", "worldwide_seisme.csv")
if (!file.exists(csv_file)) {
  deb <- c(2010, 2000, 1990, 1980, 1970, 1960, 1950, 1940, 1930, 1920, 1910, 1900)
  fin <- c(2020, 2010, 2000, 1990, 1980, 1970, 1960, 1950, 1940, 1930, 1920, 1910)

  url <- paste("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=", as.character(deb[1]), "-01-01&endtime=", as.character(fin[1]), "-01-01&minmagnitude=5", sep = "")
  combined_data <- read_csv(url)

  for (i in 2:12) {
    url <- paste("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=", as.character(deb[i]), "-01-01&endtime=", as.character(fin[i]), "-01-01&minmagnitude=5", sep = "")
    newz <- read_csv(url)
    combined_data <- bind_rows(combined_data, newz)
  }

  globalearthquake <- combined_data[, c("time", "latitude", "longitude", "mag")]

  Seuil_mag <- 5
  globalearthquake <- globalearthquake[globalearthquake$mag > Seuil_mag, ]
  write_csv(globalearthquake, csv_file)
}


globalearthquake <- read_csv(csv_file)
colnames(globalearthquake)[colnames(globalearthquake) == "time"] <- "date_time"
globalearthquake$Date <- as.Date(globalearthquake$date_time, format = "%d/%m/%Y")
globalearthquake$nbweeks <- trunc(as.numeric(difftime(globalearthquake$Date, "1900-01-01", units = "weeks")))

setDT(globalearthquake)
globalearthquake[, `:=`(
  year = year(Date),
  month = month(Date),
  month_day = mday(Date),
  week = week(Date),
  week_day = wday(Date),
  year_day = yday(Date)
)]


usethis::use_data(globalearthquake, overwrite = TRUE)
