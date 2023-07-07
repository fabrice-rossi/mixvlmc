library(vroom)
library(dplyr)
library(data.table)

csv_file <- file.path("data-raw", "worldwide_seisme.csv")
min_mag <- 5
if (!file.exists(csv_file)) {
  start <- seq(1900, 2010, by = 10)
  end <- start + 10
  start <- c(start, 2020)
  end <- c(end, 2023)
  requests <- paste("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=", start, "-01-01&endtime=", end, "-01-01&minmagnitude=", min_mag, sep = "")
  dataset <- vroom(requests)

  globalearthquake <- dataset[, c("time", "latitude", "longitude", "mag")]

  vroom_write(globalearthquake, csv_file)
}

globalearthquake <- vroom(csv_file)

colnames(globalearthquake)[colnames(globalearthquake) == "time"] <- "date_time"
globalearthquake$Date <- as.Date(globalearthquake$date_time, format = "%d/%m/%Y")
globalearthquake$nbweeks <- trunc(as.numeric(difftime(globalearthquake$Date, "1900-01-01", units = "weeks")))

setDT(globalearthquake)
setorder(globalearthquake, date_time)

globalearthquake[, `:=`(
  year = year(Date),
  month = month(Date),
  month_day = mday(Date),
  week = week(Date),
  week_day = wday(Date),
  year_day = yday(Date)
)]

usethis::use_data(globalearthquake, overwrite = TRUE)
