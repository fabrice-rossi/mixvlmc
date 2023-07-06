#' Individual household electric power consumption
#'
#' A data set containing measurements of the electric power consumption of one
#' household with a time resolution of 10 minutes for the full year of 2008.
#'
#' This is a simplified version of the full data available on the UCI Machine
#' Learning Repository under a
#' \href{https://creativecommons.org/licenses/by/4.0/legalcode}{Creative Commons
#' Attribution 4.0 International} (CC BY 4.0) license, and provided by Georges
#' Hebrail and Alice Berard.
#'
#' The original data have been averaged over a 10 minute time period (discarding
#' missing data in each period). The data set contains only
#' the measurements from year 2008.
#'
#' Notice that the different variables are expressed in the adapted units.
#' In particular, the sub-meters are measuring active energy (in watt-hour) while
#' the global active power is expressed in kilowatt.
#'
#' @format A data frame with 52704 rows and 15 variables:
#' \describe{
#' \item{month}{month of 2008}
#' \item{month_day}{day of the month}
#' \item{hour}{hour (0 to 23)}
#' \item{minute}{starting minute of the 10 minutes period of this row}
#' \item{active_power}{global average active power on the 10 minute period
#'       (in kilowatt)}
#' \item{reactive_power}{global average reactive power on the 10 minute
#'       period (in kilowatt)}
#' \item{voltage}{Average voltage on the 10 minute period (in volt)}
#' \item{intensity}{global average current intensity  on the 10 minute
#'       period (in ampere)}
#' \item{sub_metering_1}{energy sub-metering No. 1 (in watt-hour of active
#'      energy averaged over the 10 minute period). It corresponds to the kitchen,
#'      containing mainly a dishwasher, an oven and a microwave (hot plates are
#'      not electric but gas powered)}
#' \item{sub_metering_2}{energy sub-metering No. 2 (in watt-hour of active
#'      energy averaged over the 10 minute period). It corresponds to the laundry
#'      room, containing a washing-machine, a tumble-drier, a refrigerator and a light.}
#' \item{sub_metering_3}{energy sub-metering No. 3 (in watt-hour of active energy
#'      averaged over the 10 minute period). It corresponds to an electric
#'      water-heater and an air-conditioner.}
#' \item{week}{week number}
#' \item{week_day}{day of the week from 1 = Sunday to 7 = Saturday}
#' \item{year_day}{day of the year from 1 to 366 (2008 is a leap year)}
#' \item{date_time}{Date and time in POSIXct format}
#' }
#'
#' @source
#'   Individual household electric power consumption, 2012, G. Hebrail and A. Berard,
#'   UC Irvine Machine Learning repository.
#'   \doi{10.24432/C58K54}
#'
"powerconsumption"
