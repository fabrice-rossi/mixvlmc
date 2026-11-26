#' Individual household electric power consumption
#'
#' A data set containing measurements of the electric power consumption of one
#' household with a time resolution of 10 minutes for the full year 2008.
#'
#' This is a simplified version of the full data available on the UCI Machine
#' Learning Repository under a
#' \href{https://creativecommons.org/licenses/by/4.0/legalcode}{Creative Commons
#' Attribution 4.0 International} (CC BY 4.0) licence, and provided by Georges
#' Hebrail and Alice Berard.
#'
#' The original data (one measurement per minute) have been averaged over 10
#' minute periods (discarding missing data in each period). The measurements of
#' a period in which all the original data are missing are coded as `NA`. The
#' data set contains only the measurements from year 2008.
#'
#' Notice that the variables are expressed in different units. In particular,
#' the sub-meters measure active energy (in watt-hour) while the global active
#' power is expressed in kilowatt.
#'
#' @format A data frame with 52704 rows and 15 variables:
#' \describe{
#' \item{month}{month of 2008}
#' \item{month_day}{day of the month}
#' \item{hour}{hour (0 to 23)}
#' \item{minute}{starting minute of the 10 minute period of this row}
#' \item{active_power}{global average active power over the 10 minute period
#'       (in kilowatt)}
#' \item{reactive_power}{global average reactive power over the 10 minute
#'       period (in kilowatt)}
#' \item{voltage}{average voltage over the 10 minute period (in volt)}
#' \item{intensity}{global average current intensity over the 10 minute
#'       period (in ampere)}
#' \item{sub_metering_1}{energy sub-metering No. 1 (in watt-hour of active
#'      energy averaged over the 10 minute period). It corresponds to the
#'      kitchen, containing mainly a dishwasher, an oven and a microwave (hot
#'      plates are not electric but gas powered).}
#' \item{sub_metering_2}{energy sub-metering No. 2 (in watt-hour of active
#'      energy averaged over the 10 minute period). It corresponds to the laundry
#'      room, containing a washing-machine, a tumble-drier, a refrigerator and a
#'      light.}
#' \item{sub_metering_3}{energy sub-metering No. 3 (in watt-hour of active
#'      energy averaged over the 10 minute period). It corresponds to an
#'      electric water-heater and an air-conditioner.}
#' \item{week}{week of the year, from 1 to 53 (week 1 consists of the first
#'      seven days of the year, week 2 of the next seven days, etc.)}
#' \item{week_day}{day of the week from 1 = Sunday to 7 = Saturday}
#' \item{year_day}{day of the year from 1 to 366 (2008 is a leap year)}
#' \item{date_time}{date and time in POSIXct format}
#' }
#'
#' @source
#'   Individual household electric power consumption, 2012, G. Hebrail and
#'   A. Berard, UC Irvine Machine Learning repository.
#'   \doi{10.24432/C58K54}
#'
#' @examples
#' ## active power during the fifth week of 2008
#' pc <- powerconsumption[powerconsumption$week == 5, ]
#' summary(pc$active_power)
#' plot(pc$date_time, pc$active_power,
#'   type = "l", xlab = "Time", ylab = "Active power (kW)"
#' )
#' ## discretisation into four levels using the quartiles
#' power_dts <- dts(cut(pc$active_power,
#'   breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1)))
#' ))
#' power_dts
#' model <- vlmc(power_dts)
#' draw(model)
"powerconsumption"

#' Significant Earthquake Dataset
#'
#' A data set containing the earthquakes of magnitude at least 5 that occurred
#' during the period 1900-2022, with their GPS coordinates and magnitudes.
#'
#' This is a compiled version of the full data set available on
#' \href{https://www.usgs.gov/programs/earthquake-hazards}{U.S. Geological Survey Earthquake Events}
#' (USGS) which is in the
#' \href{https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits}{public domain}.
#'
#' The data set contains only the earthquakes between 1900 and 2022 with a
#' magnitude of at least 5, sorted by date and time.
#'
#' @format A data frame with 98785 rows and 12 variables:
#' \describe{
#' \item{date_time}{date and time in POSIXct format}
#' \item{latitude}{latitude of the earthquake, from -90° to 90°}
#' \item{longitude}{longitude of the earthquake, from -180° to 180°}
#' \item{mag}{the magnitude of the earthquake, indicating its strength}
#' \item{Date}{date when the earthquake occurred}
#' \item{nbweeks}{number of complete weeks elapsed since 1900/01/01}
#' \item{year}{year}
#' \item{month}{month of the year}
#' \item{month_day}{day of the month}
#' \item{week}{week of the year, from 1 to 53 (week 1 consists of the first
#'      seven days of the year, week 2 of the next seven days, etc.)}
#' \item{week_day}{day of the week from 1 = Sunday to 7 = Saturday}
#' \item{year_day}{day of the year from 1 to 366}
#' }
#'
#' @source
#'    Earthquake Catalog, U.S. Geological Survey, Department of the Interior.
#'    <https://www.usgs.gov/programs/earthquake-hazards>
#'
#' @examples
#' summary(globalearthquake$mag)
#' ## earthquakes in a rectangle around Japan since 1980
#' japan <- subset(
#'   globalearthquake,
#'   latitude > 30 & latitude < 46 & longitude > 129 & longitude < 146 &
#'     year >= 1980
#' )
#' ## binary time series: was there at least one earthquake during the week?
#' weeks <- seq(min(japan$nbweeks), max(globalearthquake$nbweeks))
#' japan_weeks <- weeks %in% japan$nbweeks
#' table(japan_weeks)
#' japan_model <- tune_vlmc(japan_weeks)
#' draw(as_vlmc(japan_model))
"globalearthquake"
