library(stringr)
library(data.table)

zip_file <- file.path("data-raw", "household_power_consumption.zip")
if (!file.exists(zip_file)) {
  download.file(
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip",
    destfile = zip_file, quiet = TRUE
  )
}

power_data <- fread(
  cmd = str_c("unzip -cq ", zip_file),
  sep = ";", dec = ".", header = TRUE, na.strings = c("?", "NA")
)

power_data[, `:=`(
  Date = as.IDate(Date, format = "%d/%m/%Y"),
  Time = as.ITime(Time)
)]

power_data[, `:=`(
  hour = hour(Time),
  by_10 = minute(Time) %/% 10
)]

powerconsumption <- power_data[, lapply(.SD, mean, na.rm = TRUE), by = .(Date, hour, by_10), .SDcols = !c("Time")]
date_time <- as.POSIXct(
  stringr::str_c(
    powerconsumption$Date, " ", powerconsumption$hour, ":",
    stringr::str_pad(powerconsumption$by_10 * 10, 2, pad = "0"), ":00"
  ),
  format = "%Y-%m-%d %H:%M:%OS",
  tz = "CET"
)
powerconsumption[, `:=`(
  year = year(Date),
  month = month(Date),
  month_day = mday(Date),
  week = week(Date),
  week_day = wday(Date),
  year_day = yday(Date),
  minute = 10 * by_10,
  date_time = date_time
)]
powerconsumption[, `:=`(Date = NULL, by_10 = NULL)]
setcolorder(powerconsumption, c("year", "month", "month_day", "hour", "minute"))
powerconsumption <- powerconsumption[year == 2008, ][, year := NULL]
setnames(powerconsumption, str_replace(str_to_lower(names(powerconsumption)), "global_", ""))

usethis::use_data(powerconsumption, overwrite = TRUE)
